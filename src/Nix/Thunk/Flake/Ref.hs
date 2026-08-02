-- | Flake references, and the two forms of one that a lock records.
--
-- A reference says where a tree comes from. 'FetchableRef' is the part of that
-- a flake declares, and 'NodeRefs' pairs it with what fetching it discovered,
-- which together are what a lock entry holds.
--
-- Also here: restating a reference to a directory inside another one, which is
-- how an input upstream declared as a relative path is given a reference that
-- means something to anyone else.
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
-- Attribute-set form is preferred over URL form so that no escaping or
-- query-string assembly is needed.
newtype FlakeRef = FlakeRef {unFlakeRef :: Map AttrName FlakeRefValue}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

-- | A reference carrying only what says how to fetch it, which is what a flake
-- declares and what a lock records as @original@.
--
-- A refinement of 'FlakeRef' in the same spirit as
-- 'Nix.Thunk.Flake.Name.FlakeId' is one of 'Nix.Thunk.Flake.Name.InputName',
-- with one difference worth knowing: the rule a @FlakeId@ keeps is Nix's, and
-- breaking it gets you an error, while the rule here is ours, and breaking it
-- gets you a lock that looks fine.
--
-- 'fetchableRef' is the way in and 'mapFetchableRef' the way to change one.
newtype FetchableRef = FetchableRef {unFetchableRef :: FlakeRef}
  deriving stock (Eq, Ord, Show)

-- | The two references a lock records for one input: the one the flake
-- declares, and the one it resolves to.
--
-- They differ only in the attributes a fetch discovers, which is why the
-- locked one is copied out of upstream's own lock rather than computed: that
-- is what lets a thunk be locked without fetching anything.
--
-- The two are not the same type, so that they cannot quietly swap places:
-- writing the locked one where the original goes produces a flake declaring
-- attributes Nix recomputes, and writing the original where the locked one
-- goes produces a lock with no hashes in it, which Nix accepts without a word.
data NodeRefs = NodeRefs
  { nodeRefs_original :: FetchableRef
  , nodeRefs_locked :: FlakeRef
  }
  deriving stock (Eq, Show)

-- | The references a lock records for the repository a thunk points at.
--
-- Unlike upstream's own nodes, this one is not in any lock to copy from, so
-- the attributes a fetch discovers have to be handed in by whoever did the
-- fetch. They are layered under the reference the thunk pins, which stays
-- authoritative for anything they disagree on.
sourceNodeRefs
  :: FlakeRef
  -- ^ The reference the thunk pins
  -> FlakeRef
  -- ^ What fetching it discovered, e.g. @narHash@ and @lastModified@
  -> NodeRefs
sourceNodeRefs srcRef discovered =
  NodeRefs
    { nodeRefs_original = original
    , nodeRefs_locked = FlakeRef $ Map.union originalAttrs (unFlakeRef discovered)
    }
  where
    original = fetchableRef srcRef
    originalAttrs = unFlakeRef $ unFetchableRef original

-- | A locked node of type @path@ names either a store path or a location on
-- whichever machine wrote the lock, and neither means anything to anyone else.
-- Such a node can only be expressed when 'withRelativeDir' restates it as a
-- subdirectory of its parent; otherwise it has to be aliased, since emitting
-- the reference without its @path@ would produce a reference to nothing.
--
-- The locked form is upstream's own, verbatim: it is already the answer to
-- what this node resolves to, hashes included.
fetchableNodeRefs :: FlakeRef -> Maybe NodeRefs
fetchableNodeRefs locked = do
  guard $ refString typeAttr locked /= Just "path"
  pure
    NodeRefs
      { nodeRefs_original = fetchableRef locked
      , nodeRefs_locked = locked
      }

-- | Re-express both of a parent's references as the same subdirectory. The
-- locked one keeps the parent's hashes, since it is the same tree, which is
-- what Nix records for a @dir@ input too.
withRelativeDirRefs :: NodeRefs -> FilePath -> Maybe NodeRefs
withRelativeDirRefs parent rel =
  NodeRefs
    <$> mapFetchableRef (`withRelativeDir` rel) parent.nodeRefs_original
    <*> withRelativeDir parent.nodeRefs_locked rel

-- | The relative path a reference was declared with, if it was declared as
-- one. References like this are why a parent has to be tracked: the lock
-- records their locked form as a bare store path, which is useless to anyone
-- else, so only the declaration still says anything usable.
--
-- Takes the @original@ of a node rather than the node, since that is the only
-- part it looks at.
--
-- Both guards are refusals rather than checks. The first passes over
-- everything that already has a reference of its own, which is the ordinary
-- case. The second passes over @path:\/home\/them\/src\/thing@, which is
-- relative to nothing anybody else can name.
relativePathOf :: FlakeRef -> Maybe FilePath
relativePathOf orig = do
  guard $ refString typeAttr orig == Just "path"
  relPath <- T.unpack <$> refString pathAttr orig
  guard $ not $ isAbsolute relPath
  pure relPath

-- | Re-express a path relative to a parent reference as a subdirectory of that
-- same reference. Because the parent is pinned to a revision this resolves to
-- the identical tree, and unlike an alias it stays overridable.
--
-- The parent's own @dir@ is composed with rather than replaced, which is what
-- makes nesting work: a parent already at @dir=deps\/thing@ with a child
-- declared at @.\/pins\/x@ comes out at @dir=deps\/thing\/pins\/x@.
withRelativeDir :: FlakeRef -> FilePath -> Maybe FlakeRef
withRelativeDir parentRef rel =
  (`setRefDir` parentRef) <$> normaliseRelative (refDir parentRef </> rel)

refDir :: FlakeRef -> FilePath
refDir flakeRef = maybe "" T.unpack $ refString dirAttr flakeRef

-- | Deleting rather than writing @dir = ""@ is the point of the branch: these
-- references are compared, and a path that resolves back to the repository
-- root has to come out as the same reference as one that never had a @dir@.
setRefDir :: FilePath -> FlakeRef -> FlakeRef
setRefDir dir (FlakeRef attrs) =
  FlakeRef $
    if null dir
      then Map.delete dirAttr attrs
      else Map.insert dirAttr (FlakeRefValue_String $ T.pack dir) attrs

dirAttr :: AttrName
dirAttr = AttrName "dir"

-- | One attribute of a reference, when it is a string.
--
-- Every attribute this package reads back off a reference is one, and every
-- one of them wants the same answer to "absent, or present but not a string":
-- no.
refString :: AttrName -> FlakeRef -> Maybe Text
refString name flakeRef = case Map.lookup name $ unFlakeRef flakeRef of
  Just (FlakeRefValue_String s) -> Just s
  _ -> Nothing

typeAttr :: AttrName
typeAttr = AttrName "type"

pathAttr :: AttrName
pathAttr = AttrName "path"

-- | Collapse @.@ and @..@ segments. 'Nothing' when the path climbs out of the
-- tree it started in, which cannot be expressed as a subdirectory of a repo.
normaliseRelative :: FilePath -> Maybe FilePath
normaliseRelative = fmap (joinPath . reverse) . foldM step [] . splitDirectories
  where
    -- Innermost segment first, so that @..@ is a total pattern match rather
    -- than a partial `init`.
    step acc = \case
      "." -> Just acc
      ".." -> case acc of
        [] -> Nothing
        _innermost : outer -> Just outer
      seg -> Just $ seg : acc

-- | Keep only the attributes of a locked node that describe /how to fetch it/,
-- and so are meaningful when copied into an input. Everything else a lock
-- records (@lastModified@, @revCount@) is derived, and Nix recomputes it.
--
-- An allowlist rather than a denylist, because the cost of the two mistakes is
-- not the same. Keeping something derived makes a noisier flake; dropping
-- something load-bearing unpins an input, and see @narHash@ below for how
-- close that came to happening.
fetchableRef :: FlakeRef -> FetchableRef
fetchableRef = FetchableRef . FlakeRef . (`Map.restrictKeys` fetchableAttrs) . unFlakeRef

-- | Change a fetchable reference in a way that leaves it fetchable.
--
-- Every use of this adds or replaces @dir@, which is itself in
-- 'fetchableAttrs', so the property survives. Anything that would take an
-- attribute outside that set belongs in
-- 'Nix.Thunk.Flake.Render.withFlakeAttr' instead, which says so by handing
-- back a plain 'FlakeRef'.
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
      , -- narHash is the only pin a tarball or file node has, so dropping it would
        -- silently unpin those inputs.
        "narHash"
      ]
