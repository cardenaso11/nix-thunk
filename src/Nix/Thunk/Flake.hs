-- | Generating the @flake.nix@ of a packed thunk.
--
-- A packed thunk is itself a flake which forwards the outputs of the repository
-- it points at. For a consumer to be able to write
--
-- > inputs.someinput.follows = "mythunk/nixpkgs";       # read
-- > inputs.mythunk.inputs.nixpkgs.follows = "nixpkgs";  # override
--
-- the thunk's flake has to expose upstream's inputs under upstream's own names.
-- @follows@ is resolved against the lock graph, so exposing them means giving
-- the thunk a root-level input per node of upstream's @flake.lock@, each pinned
-- to the revision upstream itself locked, with every edge of upstream's graph
-- reproduced as a @follows@. Reading works because the names are there;
-- overriding works because each one is a real node rather than an alias.
--
-- Everything here is pure: 'flattenLock' does the graph work and
-- 'renderFlakeNix' turns the result into Nix source. Fetching upstream and
-- running @nix flake lock@ afterwards is left to "Nix.Thunk.Internal".
module Nix.Thunk.Flake where

import Control.Monad (foldM, guard)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (fold, toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (isAbsolute, joinPath, splitDirectories, (</>))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data FlattenedFlake = FlattenedFlake
  { flattenedFlake_sourceName :: InputName
  , flattenedFlake_sourceRef :: FlakeRef
  , flattenedFlake_sourceEdges :: Map InputName InputName
  , flattenedFlake_inputs :: Map InputName FlattenedInput
  }
  deriving stock (Eq, Show)

-- | One root-level input of the generated flake.
data FlattenedInput = FlattenedInput
  { flattenedInput_ref :: Either FollowsPath FlakeRef
  -- ^ 'Left' is used when the node cannot be expressed as a fetchable
  -- reference. Such an input is readable but not overridable.
  , flattenedInput_isFlake :: Bool
  , flattenedInput_edges :: Map InputName InputName
  }
  deriving stock (Eq, Show)

-- | What the per-node translation needs to consult. The references are tied
-- back to the context that produced them, since a relative path node has to be
-- resolved against a parent whose own reference may itself have been rewritten.
data FlattenContext = FlattenContext
  { flattenContext_sourceName :: InputName
  , flattenContext_sourceRef :: FlakeRef
  , flattenContext_lock :: FlakeLock
  , flattenContext_origins :: Map NodeId NodeOrigin
  , flattenContext_refs :: Map NodeId (Either FollowsPath FlakeRef)
  , flattenContext_names :: Map NodeId InputName
  }

-- | How a node was first reached from upstream's root. This is what lets a
-- relative path input be resolved against its parent, and what an unresolvable
-- one falls back to aliasing.
data NodeOrigin = NodeOrigin
  { nodeOrigin_parent :: Maybe NodeId
  -- ^ 'Nothing' when the node hangs directly off upstream's root, in which
  -- case its parent is the thunk's own source.
  , nodeOrigin_path :: [InputName]
  -- ^ Edge names from upstream's root down to this node.
  }
  deriving stock (Eq, Show)

data FlakeLock = FlakeLock
  { flakeLock_root :: NodeId
  , flakeLock_nodes :: Map NodeId FlakeNode
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON FlakeLock where
  parseJSON = Aeson.withObject "FlakeLock" $ \o ->
    FlakeLock
      <$> o Aeson..: "root"
      <*> o Aeson..: "nodes"

data FlakeNode = FlakeNode
  { flakeNode_inputs :: Map InputName FlakeEdge
  , flakeNode_locked :: Maybe FlakeRef
  , flakeNode_original :: Maybe FlakeRef
  , flakeNode_isFlake :: Bool
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON FlakeNode where
  parseJSON = Aeson.withObject "FlakeNode" $ \o ->
    FlakeNode . fold
      <$> o Aeson..:? "inputs"
      <*> o Aeson..:? "locked"
      <*> o Aeson..:? "original"
      <*> (fromMaybe True <$> o Aeson..:? "flake")

-- | An edge in a lock graph: either a node id, or a @follows@ path interpreted
-- from the root node.
data FlakeEdge
  = FlakeEdge_Node NodeId
  | FlakeEdge_Follows FollowsPath
  deriving stock (Eq, Ord, Show)

instance Aeson.FromJSON FlakeEdge where
  parseJSON = \case
    v@(Aeson.String _) -> FlakeEdge_Node <$> Aeson.parseJSON v
    v@(Aeson.Array _) -> FlakeEdge_Follows <$> Aeson.parseJSON v
    v -> Aeson.typeMismatch "String or Array" v

-- | A @follows@ target: a sequence of input names walked from the root node.
newtype FollowsPath = FollowsPath {unFollowsPath :: [InputName]}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

-- | A flake reference in attribute-set form, e.g.
-- @{ type = "github"; owner = "NixOS"; repo = "nixpkgs"; rev = "..."; }@.
-- Attribute-set form is preferred over URL form so that no escaping or
-- query-string assembly is needed.
newtype FlakeRef = FlakeRef {unFlakeRef :: Map AttrName FlakeRefValue}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON)

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

-- | An attribute of a flake reference, e.g. @owner@ or @rev@.
newtype AttrName = AttrName {unAttrName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

-- | The id of a node in a @flake.lock@, e.g. @nixpkgs@ or @nixpkgs_2@.
newtype NodeId = NodeId {unNodeId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

-- | The name an input is bound to, either as an edge label within a lock or as
-- a root-level input of the flake being generated.
newtype InputName = InputName {unInputName :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

--------------------------------------------------------------------------------
-- Generating a thunk's flake
--------------------------------------------------------------------------------

-- | Flatten upstream's lock graph into the inputs of the thunk's own flake.
flattenLock
  :: FlakeRef
  -- ^ Reference to the repository the thunk points at
  -> FlakeLock
  -- ^ Upstream's @flake.lock@
  -> FlattenedFlake
flattenLock srcRef lock =
  FlattenedFlake
    { flattenedFlake_sourceName = ctx.flattenContext_sourceName
    , flattenedFlake_sourceRef = ctx.flattenContext_sourceRef
    , flattenedFlake_sourceEdges = overridableEdges ctx $ rootEdges lock
    , flattenedFlake_inputs =
        Map.mapKeys (inputNameOf ctx) $
          Map.mapWithKey (flattenedInput ctx) $
            nonRootNodes lock
    }
  where
    ctx = flattenContext srcRef lock

-- | An aliased node is bound with a @follows@ of its own, so overriding the
-- edge that reaches it would make the alias follow itself, which Nix rejects
-- as a follow cycle. Those edges are left alone for upstream's own lock to
-- resolve.
overridableEdges :: FlattenContext -> Map InputName NodeId -> Map InputName InputName
overridableEdges ctx = fmap (inputNameOf ctx) . Map.filter overridable
  where
    overridable target = case Map.lookup target ctx.flattenContext_refs of
      Just (Right _) -> True
      _ -> False

inputNameOf :: FlattenContext -> NodeId -> InputName
inputNameOf ctx nodeId =
  fromMaybe (nodeIdToInputName nodeId) $ Map.lookup nodeId ctx.flattenContext_names

-- | The name each node is exposed under.
--
-- A lock node id is not an input name. Nix disambiguates node ids with @_2@
-- and @_3@ suffixes as it walks the graph, so upstream's own @nixpkgs@ is
-- frequently keyed @nixpkgs_2@ while some transitive dependency claims the
-- plain key. Exposing node ids would therefore bind @mythunk\/nixpkgs@ to the
-- wrong flake, and would retarget it whenever upstream's input set changes.
-- Upstream's root inputs are named as upstream named them; anything reachable
-- only transitively keeps its node id, with collisions suffixed.
nodeInputNames :: FlakeLock -> Map NodeId InputName
nodeInputNames lock = foldl' name fromRoot $ Map.keys $ nonRootNodes lock
  where
    fromRoot =
      Map.fromListWith
        (\_ firstSeen -> firstSeen)
        [(target, edgeName) | (edgeName, target) <- Map.toList $ rootEdges lock]
    name acc nodeId
      | Map.member nodeId acc = acc
      | otherwise =
          Map.insert
            nodeId
            (freshInputName (Set.fromList $ Map.elems acc) $ nodeIdToInputName nodeId)
            acc

freshInputName :: Set InputName -> InputName -> InputName
freshInputName taken base = go (0 :: Int)
  where
    go n
      | candidate `Set.member` taken = go (n + 1)
      | otherwise = candidate
      where
        candidate
          | n == 0 = base
          | otherwise = InputName $ unInputName base <> "_" <> T.pack (show $ n + 1)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render the @flake.nix@ of a packed thunk whose upstream is itself a flake.
renderFlakeNix :: FlattenedFlake -> Text
renderFlakeNix flake =
  T.unlines $
    fold
      [
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "{"
        , "  inputs = {"
        ]
      , renderRefEntry
          flake.flattenedFlake_sourceName
          flake.flattenedFlake_sourceRef
          flake.flattenedFlake_sourceEdges
      , fold $ Map.mapWithKey renderInputEntry flake.flattenedFlake_inputs
      ,
        [ "  };"
        , "  outputs = inputs: inputs."
            <> nixString (unInputName flake.flattenedFlake_sourceName)
            <> ".outputs;"
        , "}"
        ]
      ]

renderInputEntry :: InputName -> FlattenedInput -> [Text]
renderInputEntry name input = case input.flattenedInput_ref of
  -- An input carrying a `follows` may not also carry a flake reference, so an
  -- alias is emitted on its own.
  Left followsPath -> renderAliasEntry name followsPath
  Right fetchable ->
    renderRefEntry
      name
      (withFlakeAttr input.flattenedInput_isFlake fetchable)
      input.flattenedInput_edges

-- | Nix takes an input to be a flake unless told otherwise, so only the
-- negative case is worth recording.
withFlakeAttr :: Bool -> FlakeRef -> FlakeRef
withFlakeAttr True = id
withFlakeAttr False =
  FlakeRef . Map.insert (AttrName "flake") (FlakeRefValue_Bool False) . unFlakeRef

renderRefEntry :: InputName -> FlakeRef -> Map InputName InputName -> [Text]
renderRefEntry name flakeRef inputEdges =
  fold
    [ ["    " <> nixString (unInputName name) <> " = {"]
    , toList $ Map.mapWithKey renderAttr $ unFlakeRef flakeRef
    , renderEdges inputEdges
    , ["    };"]
    ]

renderAliasEntry :: InputName -> FollowsPath -> [Text]
renderAliasEntry name followsPath =
  [ "    " <> nixString (unInputName name) <> " = {"
  , "      follows = " <> nixString (renderFollowsPath followsPath) <> ";"
  , "    };"
  ]

renderEdges :: Map InputName InputName -> [Text]
renderEdges inputEdges
  | Map.null inputEdges = []
  | otherwise =
      fold
        [ ["      inputs = {"]
        , toList $ Map.mapWithKey renderEdge inputEdges
        , ["      };"]
        ]

renderEdge :: InputName -> InputName -> Text
renderEdge name target =
  "        "
    <> nixString (unInputName name)
    <> " = { follows = "
    <> nixString (unInputName target)
    <> "; };"

renderAttr :: AttrName -> FlakeRefValue -> Text
renderAttr name value = "      " <> unAttrName name <> " = " <> renderRefValue value <> ";"

-- | A reference as a standalone attribute set, for handing to
-- @builtins.fetchTree@. Fetching the repository the same way the generated
-- flake will also proves, at pack time, that the reference resolves.
renderFlakeRefExpr :: FlakeRef -> Text
renderFlakeRefExpr flakeRef = "{ " <> fold (Map.mapWithKey attr $ unFlakeRef flakeRef) <> "}"
  where
    attr name value = unAttrName name <> " = " <> renderRefValue value <> "; "

renderFollowsPath :: FollowsPath -> Text
renderFollowsPath = T.intercalate "/" . fmap unInputName . unFollowsPath

renderRefValue :: FlakeRefValue -> Text
renderRefValue = \case
  FlakeRefValue_String s -> nixString s
  FlakeRefValue_Bool b -> if b then "true" else "false"
  FlakeRefValue_Int n -> T.pack $ show n

nixString :: Text -> Text
nixString t = "\"" <> T.concatMap escapeNixChar t <> "\""

escapeNixChar :: Char -> Text
escapeNixChar = \case
  '"' -> "\\\""
  '\\' -> "\\\\"
  '$' -> "\\$"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c -> T.singleton c

-- | The @flake.nix@ of a packed thunk whose upstream is not a flake. There are
-- no inputs to expose, so this exposes the fetched source and nothing else. The
-- content is the same for every such thunk.
-- The source is declared as an input rather than fetched by reusing
-- @.\/thunk.nix@: that loader reaches @(import nixpkgs {}).fetchgit@ on its
-- common path, and importing nixpkgs needs @builtins.currentSystem@, which
-- does not exist under the pure evaluation a flake is read with.
renderSourceOnlyFlakeNix :: FlakeRef -> Text
renderSourceOnlyFlakeNix srcRef =
  T.unlines $
    fold
      [
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "{"
        , "  description = \"nix-thunk packed thunk\";"
        , "  inputs = {"
        ]
      , renderRefEntry
          (InputName "src")
          (withFlakeAttr False $ fetchableRef srcRef)
          Map.empty
      ,
        [ "  };"
        , "  outputs = { self, src }: { inherit src; };"
        , "}"
        ]
      ]

-- | The @flake.nix@ written into an unpacked checkout of a repository that is
-- not itself a flake, so that a consumer using the thunk as a flake input is
-- not broken by unpacking it. The checkout is the source, so unlike
-- 'sourceOnlyFlakeNix' there is nothing left to fetch.
unpackedSourceFlakeNix :: Text
unpackedSourceFlakeNix =
  """
  # DO NOT HAND-EDIT THIS FILE
  {
    description = "nix-thunk unpacked thunk";
    outputs = { self }: { src = self.outPath; };
  }
  """

-- | The lock of a flake with no inputs. Written directly rather than by
-- invoking Nix, so that packing a repository which is not a flake does not
-- require the flakes feature to be enabled.
emptyFlakeLock :: Text
emptyFlakeLock =
  """
  {
    "nodes": {
      "root": {}
    },
    "root": "root",
    "version": 7
  }
  """

--------------------------------------------------------------------------------
-- Translating nodes
--------------------------------------------------------------------------------

-- | Ties the knot between the context and the references it computes.
flattenContext :: FlakeRef -> FlakeLock -> FlattenContext
flattenContext srcRef lock = ctx
  where
    names = nodeInputNames lock
    ctx =
      FlattenContext
        { flattenContext_sourceName = sourceInputName names
        , flattenContext_sourceRef = fetchableRef srcRef
        , flattenContext_lock = lock
        , flattenContext_origins = discoverOrigins lock
        , flattenContext_refs = Map.mapWithKey (nodeRef ctx) $ nonRootNodes lock
        , flattenContext_names = names
        }

flattenedInput :: FlattenContext -> NodeId -> FlakeNode -> FlattenedInput
flattenedInput ctx nodeId node =
  FlattenedInput
    { flattenedInput_ref = fromMaybe (Left $ aliasPath ctx nodeId) $ Map.lookup nodeId ctx.flattenContext_refs
    , flattenedInput_isFlake = node.flakeNode_isFlake
    , flattenedInput_edges = overridableEdges ctx $ nodeEdges ctx.flattenContext_lock node
    }

-- | The reference the thunk should bind a single node to.
nodeRef :: FlattenContext -> NodeId -> FlakeNode -> Either FollowsPath FlakeRef
nodeRef ctx nodeId node = fromMaybe (Left $ aliasPath ctx nodeId) $ case relativePathOf node of
  Nothing -> Right <$> (fetchableNodeRef =<< node.flakeNode_locked)
  Just rel -> do
    parentRef <- parentRefOf ctx nodeId
    Right <$> withRelativeDir parentRef rel

-- | A locked node of type @path@ names either a store path or a location on
-- whichever machine wrote the lock, and neither means anything to anyone else.
-- Such a node can only be expressed when 'withRelativeDir' restates it as a
-- subdirectory of its parent; otherwise it has to be aliased, since emitting
-- the reference without its @path@ would produce a reference to nothing.
fetchableNodeRef :: FlakeRef -> Maybe FlakeRef
fetchableNodeRef locked = do
  guard $
    Map.lookup (AttrName "type") (unFlakeRef locked)
      /= Just (FlakeRefValue_String "path")
  pure $ fetchableRef locked

parentRefOf :: FlattenContext -> NodeId -> Maybe FlakeRef
parentRefOf ctx nodeId = do
  origin <- Map.lookup nodeId ctx.flattenContext_origins
  case origin.nodeOrigin_parent of
    Nothing -> Just ctx.flattenContext_sourceRef
    Just parentId -> rightToMaybe =<< Map.lookup parentId ctx.flattenContext_refs

-- | Where a node sits relative to the thunk's source, for nodes that cannot be
-- given a reference of their own.
aliasPath :: FlattenContext -> NodeId -> FollowsPath
aliasPath ctx nodeId =
  FollowsPath $
    ctx.flattenContext_sourceName : foldMap (.nodeOrigin_path) (Map.lookup nodeId ctx.flattenContext_origins)

--------------------------------------------------------------------------------
-- Walking a lock
--------------------------------------------------------------------------------

rootEdges :: FlakeLock -> Map InputName NodeId
rootEdges lock = Map.mapMaybe (resolveEdge lock) $ edgesOf lock lock.flakeLock_root

nodeEdges :: FlakeLock -> FlakeNode -> Map InputName NodeId
nodeEdges lock node = Map.mapMaybe (resolveEdge lock) node.flakeNode_inputs

nonRootNodes :: FlakeLock -> Map NodeId FlakeNode
nonRootNodes lock = Map.delete lock.flakeLock_root lock.flakeLock_nodes

-- | The name the thunk's own source is bound to. Upstream's inputs occupy
-- root-level names too, so on the off chance one of them is already called
-- @upstream@, pick another name rather than colliding.
sourceInputName :: Map NodeId InputName -> InputName
sourceInputName names = freshInputName (Set.fromList $ Map.elems names) $ InputName "upstream"

-- | Breadth-first walk of upstream's lock, recording where each node was first
-- reached from.
discoverOrigins :: FlakeLock -> Map NodeId NodeOrigin
discoverOrigins lock = go Map.empty $ nodeChildren lock lock.flakeLock_root []
  where
    go acc [] = acc
    go acc ((parentId, prefix, nodeId) : rest)
      | nodeId `Map.member` acc || nodeId == lock.flakeLock_root = go acc rest
      | otherwise =
          go
            (Map.insert nodeId (NodeOrigin parentId prefix) acc)
            (rest <> nodeChildren lock nodeId prefix)

nodeChildren :: FlakeLock -> NodeId -> [InputName] -> [(Maybe NodeId, [InputName], NodeId)]
nodeChildren lock nodeId prefix =
  [ (parentId, prefix <> [name], child)
  | (name, edge) <- Map.toList $ edgesOf lock nodeId
  , Just child <- [resolveEdge lock edge]
  ]
  where
    -- The root's own children have no parent node: their parent is the thunk's
    -- source, which is not itself part of upstream's lock.
    parentId = if nodeId == lock.flakeLock_root then Nothing else Just nodeId

edgesOf :: FlakeLock -> NodeId -> Map InputName FlakeEdge
edgesOf lock nodeId = foldMap (.flakeNode_inputs) $ Map.lookup nodeId lock.flakeLock_nodes

resolveEdge :: FlakeLock -> FlakeEdge -> Maybe NodeId
resolveEdge = resolveEdgeWithin followsFuel

resolveEdgeWithin :: Int -> FlakeLock -> FlakeEdge -> Maybe NodeId
resolveEdgeWithin fuel lock = \case
  FlakeEdge_Node nodeId -> Just nodeId
  FlakeEdge_Follows followsPath
    | fuel <= 0 -> Nothing
    | otherwise -> followPath (fuel - 1) lock lock.flakeLock_root $ unFollowsPath followsPath

followPath :: Int -> FlakeLock -> NodeId -> [InputName] -> Maybe NodeId
followPath _ _ nodeId [] = Just nodeId
followPath fuel lock nodeId (name : rest) = do
  edge <- Map.lookup name $ edgesOf lock nodeId
  next <- resolveEdgeWithin fuel lock edge
  followPath fuel lock next rest

-- | Bound on how far a @follows@ chain is walked, so that a malformed lock
-- cannot hang the caller.
followsFuel :: Int
followsFuel = 64

--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------

-- | The relative path a node was declared with, if it was declared as one.
-- Nodes like this are why the parent's reference has to be tracked: a lock
-- records their @locked@ form as a bare store path, which is useless to anyone
-- else.
relativePathOf :: FlakeNode -> Maybe FilePath
relativePathOf node = do
  orig <- node.flakeNode_original
  guard $ refAttr orig "type" == Just "path"
  relPath <- T.unpack <$> refAttr orig "path"
  guard $ not $ isAbsolute relPath
  pure relPath
  where
    refAttr orig name = case Map.lookup (AttrName name) $ unFlakeRef orig of
      Just (FlakeRefValue_String s) -> Just s
      _ -> Nothing

-- | Re-express a path relative to a parent reference as a subdirectory of that
-- same reference. Because the parent is pinned to a revision this resolves to
-- the identical tree, and unlike an alias it stays overridable.
withRelativeDir :: FlakeRef -> FilePath -> Maybe FlakeRef
withRelativeDir parentRef rel =
  flip setRefDir parentRef <$> normaliseRelative (refDir parentRef </> rel)

refDir :: FlakeRef -> FilePath
refDir flakeRef = case Map.lookup dirAttr $ unFlakeRef flakeRef of
  Just (FlakeRefValue_String dir) -> T.unpack dir
  _ -> ""

setRefDir :: FilePath -> FlakeRef -> FlakeRef
setRefDir dir (FlakeRef attrs) =
  FlakeRef $
    if null dir
      then Map.delete dirAttr attrs
      else Map.insert dirAttr (FlakeRefValue_String $ T.pack dir) attrs

dirAttr :: AttrName
dirAttr = AttrName "dir"

-- | Collapse @.@ and @..@ segments. 'Nothing' when the path climbs out of the
-- tree it started in, which cannot be expressed as a subdirectory of a repo.
normaliseRelative :: FilePath -> Maybe FilePath
normaliseRelative = fmap joinPath . foldM step [] . splitDirectories
  where
    step acc = \case
      "." -> Just acc
      ".." -> if null acc then Nothing else Just $ init acc
      seg -> Just $ acc <> [seg]

-- | Keep only the attributes of a locked node that describe /how to fetch it/,
-- and so are meaningful when copied into an input. Everything else a lock
-- records (@narHash@, @lastModified@, @revCount@) is derived, and Nix
-- recomputes it.
fetchableRef :: FlakeRef -> FlakeRef
fetchableRef = FlakeRef . (`Map.restrictKeys` fetchableAttrs) . unFlakeRef

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

-- | Upstream's lock node ids become root-level input names of the thunk's
-- flake. That identification is what makes @mythunk\/nixpkgs@ resolve, so it is
-- spelled out rather than left implicit in a shared 'Text'.
nodeIdToInputName :: NodeId -> InputName
nodeIdToInputName = InputName . unNodeId
