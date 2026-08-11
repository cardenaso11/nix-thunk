-- | This module defines flake references, and the two forms of a reference that
-- a lock records.
--
-- A reference says where a tree comes from. 'FetchableRef' is the part of a
-- reference that a flake declares. 'NodeRefs' pairs that part with the
-- attributes that a fetch discovered. A lock entry holds both.
--
-- This module also restates an input that upstream declared as a relative path.
-- Such an input needs a reference that means something to anyone else.
module Nix.Thunk.Flake.Ref where

import Control.Monad (foldM, guard)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (isAbsolute, joinPath, splitDirectories, (</>))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | An attribute of a flake reference, e.g. @owner@ or @rev@.
newtype AttrName = AttrName {unAttrName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

-- | A scalar in a flake reference. Nix rejects anything else: /flake reference
-- attribute sets may only contain integers, Booleans, and strings/.
data FlakeRefValue
  = FlakeRefValue_String Text
  | FlakeRefValue_Bool Bool
  | FlakeRefValue_Int Integer
  deriving stock (Eq, Ord, Show)

instance Aeson.FromJSON FlakeRefValue where
  parseJSON = \case
    Aeson.String s -> pure $ FlakeRefValue_String s
    Aeson.Bool b -> pure $ FlakeRefValue_Bool b
    Aeson.Number n -> pure $ FlakeRefValue_Int $ truncate n
    v -> Aeson.typeMismatch "String, Bool or Number" v

-- | A flake reference in attribute-set form, e.g.
-- @{ type = "github"; owner = "NixOS"; repo = "nixpkgs"; rev = "..."; }@.
-- Attribute-set form needs no escaping, and the code never assembles a query
-- string.
newtype FlakeRef = FlakeRef {unFlakeRef :: Map AttrName FlakeRefValue}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

-- | A reference that holds only the attributes that say how to fetch a tree. A
-- flake declares these attributes, and a lock records them as @original@.
newtype FetchableRef = FetchableRef {unFetchableRef :: FlakeRef}
  deriving stock (Eq, Ord, Show)

-- | The two references that a lock records for one input: the one that the
-- flake declares, and the one that the input resolves to.
--
-- The two differ only in the attributes that a fetch discovers. So we copy the
-- locked one from upstream's own lock, and we do not compute it. That is how we
-- lock a thunk without a fetch.
--
-- The two have different types, so they cannot swap places by mistake. Nix
-- accepts either swap without a word: the flake would declare attributes that
-- Nix recomputes, or the lock would hold no hashes.
data NodeRefs = NodeRefs
  { nodeRefs_original :: FetchableRef
  , nodeRefs_locked :: FlakeRef
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Node references
--------------------------------------------------------------------------------

-- | The references that a lock records for the repository that a thunk points
-- at.
--
-- Upstream's own nodes appear in a lock that we can copy. This node appears in
-- no lock, so the caller must pass the attributes that its fetch discovered.
-- The reference that the thunk pins takes priority wherever the two disagree.
sourceNodeRefs
  :: FlakeRef
  -- ^ The reference that the thunk pins
  -> FlakeRef
  -- ^ The attributes that the fetch discovered, e.g. @narHash@ and
  -- @lastModified@
  -> NodeRefs
sourceNodeRefs srcRef discovered =
  NodeRefs
    { nodeRefs_original = original
    , nodeRefs_locked = FlakeRef $ Map.union originalAttrs (unFlakeRef discovered)
    }
  where
    original = fetchableRef srcRef
    originalAttrs = unFlakeRef $ unFetchableRef original

-- | A locked node of type @path@ names a store path, or a location on the
-- machine that wrote the lock. Neither name means anything to anyone else.
-- 'withRelativeDir' can restate such a node as a subdirectory of its parent. In
-- every other case the node needs an alias, because a reference without its
-- @path@ points at nothing.
--
-- The locked form is upstream's own form, word for word, and it includes the
-- hashes.
fetchableNodeRefs :: FlakeRef -> Maybe NodeRefs
fetchableNodeRefs locked = do
  guard $ not $ isPathRef locked
  pure
    NodeRefs
      { nodeRefs_original = fetchableRef locked
      , nodeRefs_locked = locked
      }

-- | Restates both of a parent's references as the same subdirectory. The locked
-- one keeps the parent's hashes, because it is the same tree. Nix records the
-- same hashes for a @dir@ input.
withRelativeDirRefs :: NodeRefs -> FilePath -> Maybe NodeRefs
withRelativeDirRefs parent rel =
  NodeRefs
    <$> mapFetchableRef (`withRelativeDir` rel) parent.nodeRefs_original
    <*> withRelativeDir parent.nodeRefs_locked rel

--------------------------------------------------------------------------------
-- Relative paths
--------------------------------------------------------------------------------

-- | The relative path that a flake declared for a reference. 'Nothing' when the
-- flake declared no relative path.
--
-- Such references are the reason why we track a parent. The lock records their
-- locked form as a bare store path, and that path is useless to anyone else.
-- Only the declaration still says anything usable.
--
-- This function refuses an absolute path such as
-- @path:\/home\/them\/src\/thing@, because nobody else can name what that path
-- is relative to.
relativePathOf :: FlakeRef -> Maybe FilePath
relativePathOf orig = do
  guard $ isPathRef orig
  relPath <- T.unpack <$> refString pathAttr orig
  guard $ not $ isAbsolute relPath
  pure relPath

-- | Restates a path that is relative to a parent reference, as a subdirectory
-- of that same reference.
--
-- The parent pins a revision, so the result resolves to the identical tree. A
-- consumer can still override the result, and a consumer cannot override an
-- alias.
--
-- This function composes the parent's own @dir@ with the child's path, so a
-- child can nest inside a parent that is already nested. A parent at
-- @dir=deps\/thing@ with a child declared at @.\/pins\/x@ becomes
-- @dir=deps\/thing\/pins\/x@.
withRelativeDir :: FlakeRef -> FilePath -> Maybe FlakeRef
withRelativeDir parentRef rel =
  (`setRefDir` parentRef) <$> normaliseRelative (refDir parentRef </> rel)

refDir :: FlakeRef -> FilePath
refDir flakeRef = maybe "" T.unpack $ refString dirAttr flakeRef

-- | The branch deletes @dir@ instead of writing @dir = ""@. The code compares
-- these references, so a path that resolves to the repository root must give
-- the same reference as a path that never had a @dir@.
setRefDir :: FilePath -> FlakeRef -> FlakeRef
setRefDir dir (FlakeRef attrs) =
  FlakeRef $
    if null dir
      then Map.delete dirAttr attrs
      else Map.insert dirAttr (FlakeRefValue_String $ T.pack dir) attrs

-- | Collapses @.@ and @..@ segments. Returns 'Nothing' when the path leaves the
-- tree that it started in, because a subdirectory of a repository cannot name
-- such a path.
normaliseRelative :: FilePath -> Maybe FilePath
normaliseRelative = fmap (joinPath . reverse) . foldM step [] . splitDirectories
  where
    -- The accumulator holds the innermost segment first. So @..@ is a total
    -- pattern match, and it needs no partial `init`.
    step acc = \case
      "." -> Just acc
      ".." -> case acc of
        [] -> Nothing
        _innermost : outer -> Just outer
      seg -> Just $ seg : acc

--------------------------------------------------------------------------------
-- Reference attributes
--------------------------------------------------------------------------------

-- | One attribute of a reference, when the attribute is a string.
--
-- Every attribute that this package reads from a reference is a string. An
-- absent attribute and a non-string attribute both give 'Nothing'.
refString :: AttrName -> FlakeRef -> Maybe Text
refString name flakeRef = case Map.lookup name $ unFlakeRef flakeRef of
  Just (FlakeRefValue_String s) -> Just s
  _ -> Nothing

-- | Whether a reference has type @path@.
isPathRef :: FlakeRef -> Bool
isPathRef flakeRef = refString typeAttr flakeRef == Just "path"

typeAttr :: AttrName
typeAttr = AttrName "type"

pathAttr :: AttrName
pathAttr = AttrName "path"

dirAttr :: AttrName
dirAttr = AttrName "dir"

--------------------------------------------------------------------------------
-- Fetchable references
--------------------------------------------------------------------------------

-- | Keeps only the attributes of a locked node that say /how to fetch it/.
-- Those attributes still mean something after this code copies them into an
-- input. A lock records other attributes, such as @lastModified@ and
-- @revCount@. Nix derives those attributes and recomputes them.
--
-- The set is an allowlist, not a denylist. If this set keeps a derived
-- attribute, the flake becomes noisier. If this set drops an essential
-- attribute, an input loses its pin.
fetchableRef :: FlakeRef -> FetchableRef
fetchableRef = FetchableRef . FlakeRef . (`Map.restrictKeys` fetchableAttrs) . unFlakeRef

-- | Changes a fetchable reference, and the result stays fetchable. So the
-- change must write only attributes that 'fetchableAttrs' holds, such as
-- @dir@.
mapFetchableRef :: Functor f => (FlakeRef -> f FlakeRef) -> FetchableRef -> f FetchableRef
mapFetchableRef f = fmap FetchableRef . f . unFetchableRef

fetchableAttrs :: Set AttrName
fetchableAttrs =
  Set.fromList $
    fmap
      AttrName
      [ "type"
      , "owner"
      , "repo"
      , "url"
      , "rev"
      , "ref"
      , "dir"
      , "host"
      , "submodules"
      , "allRefs"
      , "shallow"
      , "exportIgnore"
      , -- narHash is the only pin that a tarball or file node has. This set must
        -- keep it, or those inputs lose their pin in silence.
        "narHash"
      ]
