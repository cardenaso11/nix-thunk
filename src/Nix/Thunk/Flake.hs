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
import Data.Aeson.Encode.Pretty (Config (confCompare, confIndent, confTrailingNewline), Indent (Spaces), defConfig, encodePretty')
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlpha, isAscii, isDigit)
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
  , flattenedFlake_sourceRefs :: NodeRefs
  , flattenedFlake_sourceEdges :: Map InputName InputName
  , flattenedFlake_inputs :: Map InputName FlattenedInput
  , flattenedFlake_complete :: Bool
  -- ^ Whether every node of upstream's lock became an input with a reference
  -- of its own and every edge was reproduced. Only then does this flake say
  -- everything about its own inputs, and only then can 'renderFlakeLock'
  -- describe it without asking Nix.
  }
  deriving stock (Eq, Show)

-- | One root-level input of the generated flake.
data FlattenedInput = FlattenedInput
  { flattenedInput_ref :: Either FollowsPath NodeRefs
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
  , flattenContext_sourceRefs :: NodeRefs
  , flattenContext_lock :: FlakeLock
  , flattenContext_origins :: Map NodeId NodeOrigin
  , flattenContext_refs :: Map NodeId (Either FollowsPath NodeRefs)
  , flattenContext_names :: Map NodeId InputName
  }

-- | The two references a lock records for one input: the one the flake
-- declares, and the one it resolves to.
--
-- They differ only in the attributes a fetch discovers, which is why the
-- locked one is copied out of upstream's own lock rather than computed: that
-- is what lets a thunk be locked without fetching anything.
data NodeRefs = NodeRefs
  { nodeRefs_original :: FlakeRef
  , nodeRefs_locked :: FlakeRef
  }
  deriving stock (Eq, Show)

-- | One node of a @flake.lock@.
data LockNode = LockNode
  { lockNode_edges :: Map InputName LockEdge
  , lockNode_refs :: Maybe NodeRefs
  -- ^ 'Nothing' for the root node, which is the flake being locked.
  , lockNode_isFlake :: Bool
  }
  deriving stock (Eq, Show)

-- | An edge of a @flake.lock@: a node id, or a path walked from the root.
data LockEdge
  = LockEdge_Node InputName
  | LockEdge_Follows FollowsPath
  deriving stock (Eq, Show)

-- | How a node was first reached from upstream's root. This is what lets a
-- relative path input be resolved against its parent, and what an unresolvable
-- one falls back to aliasing.
data NodeOrigin = NodeOrigin
  { nodeOrigin_declaredIn :: Maybe DeclarationSite
  -- ^ 'Nothing' when the node was only ever reached across a @follows@, which
  -- says nothing about where the node came from.
  , nodeOrigin_path :: [InputName]
  -- ^ Edge names from upstream's root down to this node.
  }
  deriving stock (Eq, Show)

-- | The flake a node was declared by, which is the flake a relative path it
-- was declared with is relative to.
data DeclarationSite
  = -- | The repository the thunk points at: the node hangs off upstream's root.
    DeclarationSite_Source
  | -- | Another node of upstream's lock.
    DeclarationSite_Node NodeId
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
  :: NodeRefs
  -- ^ Reference to the repository the thunk points at
  -> FlakeLock
  -- ^ Upstream's @flake.lock@
  -> FlattenedFlake
flattenLock srcRefs lock =
  FlattenedFlake
    { flattenedFlake_sourceName = ctx.flattenContext_sourceName
    , flattenedFlake_sourceRefs = ctx.flattenContext_sourceRefs
    , flattenedFlake_sourceEdges = overridableEdges ctx $ rootEdges lock
    , flattenedFlake_inputs =
        Map.union
          ( Map.mapKeys (inputNameOf ctx) $
              Map.mapWithKey (flattenedInput ctx) $
                nonRootNodes lock
          )
          (aliasInput <$> rootAliases ctx)
    , flattenedFlake_complete = all (edgesFullyDeclared ctx) $ Map.keys lock.flakeLock_nodes
    }
  where
    ctx = flattenContext srcRefs lock

-- | Whether the generated flake declares every input a node of upstream's lock
-- declares.
--
-- 'overridableEdges' drops an edge it cannot restate: one pointing at a node
-- with no reference of its own, one whose @follows@ walks off the end of the
-- graph, and one whose name Nix will not accept in an override. Whatever is
-- dropped, Nix has to settle from upstream's own lock, and a lock that has to
-- say what upstream's inputs are is not one this module can write.
edgesFullyDeclared :: FlattenContext -> NodeId -> Bool
edgesFullyDeclared ctx nodeId =
  Map.size (overridableEdges ctx $ nodeEdgesOf lock nodeId) == Map.size (edgesOf lock nodeId)
  where
    lock = ctx.flattenContext_lock

-- | A node given no reference of its own is one 'overridableEdges' leaves the
-- edges to, so upstream's lock, not ours, is what says where it comes from.
-- The aliases 'rootAliases' adds are not these: they are extra names for a
-- node that does have one.
nodeHasRef :: Either FollowsPath NodeRefs -> Bool
nodeHasRef = \case
  Left _ -> False
  Right _ -> True

-- | The names upstream bound at its root that are not the name their node is
-- exposed under, mapped to that name. Several of upstream's root inputs can
-- resolve to one node, since a root-level @follows@ is just another name for
-- what it points at, and only one of them can be the name the node itself is
-- given. The rest are emitted as aliases, so that every name upstream used
-- still resolves through the thunk.
--
-- Except the ones Nix cannot refer to at all: an alias exists to be followed,
-- and a name that cannot appear in a @follows@ would only be dead weight.
rootAliases :: FlattenContext -> Map InputName InputName
rootAliases ctx =
  Map.fromList
    [ (edgeName, exposed)
    | (edgeName, target) <- Map.toList $ rootEdges ctx.flattenContext_lock
    , isFlakeId edgeName
    , let exposed = inputNameOf ctx target
    , edgeName /= exposed
    ]

-- | An input that is nothing but a second name for a sibling input.
aliasInput :: InputName -> FlattenedInput
aliasInput target =
  FlattenedInput
    { flattenedInput_ref = Left $ FollowsPath [target]
    , flattenedInput_isFlake = True
    , flattenedInput_edges = Map.empty
    }

-- | The edges of a node that the generated flake restates as a @follows@ onto
-- one of its own inputs.
--
-- Two kinds are left alone for upstream's own lock to resolve. An aliased node
-- is bound with a @follows@ of its own, so overriding the edge that reaches it
-- would make the alias follow itself, which Nix rejects as a follow cycle. And
-- an edge whose name is not a flake identifier cannot be overridden at all,
-- since the override is written as an attribute path.
overridableEdges :: FlattenContext -> Map InputName NodeId -> Map InputName InputName
overridableEdges ctx = fmap (inputNameOf ctx) . Map.filterWithKey overridable
  where
    overridable edgeName target =
      isFlakeId edgeName && maybe False nodeHasRef (Map.lookup target ctx.flattenContext_refs)

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
-- only transitively keeps its node id, with collisions suffixed. Either way the
-- name has to be one Nix can refer to, so 'toFlakeId' has the last word.
nodeInputNames :: FlakeLock -> Map NodeId InputName
nodeInputNames lock = foldl' nameTransitive (rootInputNames lock) $ Map.keys $ nonRootNodes lock
  where
    -- Every name upstream bound at its root is reserved, including the ones
    -- 'rootInputNames' had to drop: 'rootAliases' will claim those, and a
    -- transitive node must not be given a name an alias is about to take.
    reserved = Set.fromList $ toFlakeId <$> Map.keys (rootEdges lock)
    nameTransitive acc nodeId
      | Map.member nodeId acc = acc
      | otherwise =
          Map.insert
            nodeId
            (freshInputName (reserved <> Set.fromList (toList acc)) $ toFlakeId $ nodeIdToInputName nodeId)
            acc

-- | The name each node upstream binds at its root is exposed under. A node
-- bound more than once, which a root-level @follows@ does, keeps the first of
-- those names.
rootInputNames :: FlakeLock -> Map NodeId InputName
rootInputNames lock = foldl' nameFromRoot Map.empty $ Map.toList $ rootEdges lock
  where
    nameFromRoot acc (edgeName, target)
      | Map.member target acc = acc
      | otherwise =
          Map.insert
            target
            (freshInputName (Set.fromList $ toList acc) $ toFlakeId edgeName)
            acc

-- | Whether Nix will accept a name where it parses one: both a @follows@ and
-- the attribute path of an override are read as flake identifiers.
--
-- Declaring an input is not one of those places, so upstream can and does have
-- inputs named things a consumer cannot refer to: haskell.nix has fourteen,
-- including @hls-1.10@.
isFlakeId :: InputName -> Bool
isFlakeId name = case T.uncons $ unInputName name of
  Just (leading, rest) -> isFlakeIdStart leading && T.all isFlakeIdChar rest
  Nothing -> False

-- | The name to expose an input under, given the name upstream used for it.
--
-- Upstream's own name whenever Nix can refer to it, and the nearest thing to it
-- otherwise. These names exist to be followed, so a name that cannot appear in
-- a @follows@ is no use as one, however faithful.
toFlakeId :: InputName -> InputName
toFlakeId name
  | isFlakeId name = name
  | otherwise = InputName $ leadingLetter <> T.map keepOrReplace text
  where
    text = unInputName name
    leadingLetter = if maybe False (isFlakeIdStart . fst) (T.uncons text) then "" else "input-"
    keepOrReplace c = if isFlakeIdChar c then c else '_'

isFlakeIdStart :: Char -> Bool
isFlakeIdStart c = isAscii c && isAlpha c

isFlakeIdChar :: Char -> Bool
isFlakeIdChar c = isFlakeIdStart c || (isAscii c && isDigit c) || c == '_' || c == '-'

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
          flake.flattenedFlake_sourceRefs.nodeRefs_original
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
  Right refs ->
    renderRefEntry
      name
      (withFlakeAttr input.flattenedInput_isFlake refs.nodeRefs_original)
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
          sourceOnlyInputName
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
-- 'renderSourceOnlyFlakeNix' there is nothing left to fetch.
--
-- @src@ is @sourceInfo@ rather than @outPath@ because that is what the packed
-- thunk's @src@ is: there it is a @flake = false@ input, and Nix binds such an
-- input to the fetched tree's attributes. A consumer reading @src.outPath@ or
-- @src.narHash@ has to keep working across a pack or an unpack, which is the
-- whole point of writing this file at all.
--
-- Whatever this says, @default.nix@ has to recognise byte for byte, so that
-- 'Nix.Thunk.Internal.thunkSource' can keep it out of an unpacked
-- dependency's source. Change one and change the other.
unpackedSourceFlakeNix :: Text
unpackedSourceFlakeNix =
  """
  # DO NOT HAND-EDIT THIS FILE
  {
    description = "nix-thunk unpacked thunk";
    outputs = { self }: { src = self.sourceInfo; };
  }
  """

--------------------------------------------------------------------------------
-- Rendering a lock
--------------------------------------------------------------------------------

-- | Render the @flake.lock@ of a packed thunk whose upstream is itself a flake.
--
-- Written here rather than by running @nix flake lock@ on the generated flake.
-- Locking has to resolve every input a flake declares, and this one declares
-- an input per node of upstream's lock, so packing would fetch upstream's
-- whole transitive closure and fail on any single pin it could not reach.
-- Every reference and hash the lock needs is already in upstream's own lock,
-- which makes this file a relabelling of that one rather than new information.
--
-- Only valid when 'flattenedFlake_complete' holds: an input the generated
-- flake did not fully declare is one Nix would have had to work out for
-- itself, and a lock missing it is not a lock.
renderFlakeLock :: FlattenedFlake -> LBS.ByteString
renderFlakeLock flake = renderLockNodes (rootLockNode flake) (lockNodes flake)

-- | The root node of the generated flake's lock: an edge to the thunk's source
-- and one to every input, with the aliases recorded as the paths they follow.
rootLockNode :: FlattenedFlake -> LockNode
rootLockNode flake =
  LockNode
    { lockNode_edges =
        Map.insert
          flake.flattenedFlake_sourceName
          (LockEdge_Node flake.flattenedFlake_sourceName)
          (Map.mapWithKey rootLockEdge flake.flattenedFlake_inputs)
    , lockNode_refs = Nothing
    , lockNode_isFlake = True
    }

rootLockEdge :: InputName -> FlattenedInput -> LockEdge
rootLockEdge name input = case input.flattenedInput_ref of
  Left followsPath -> LockEdge_Follows followsPath
  Right _ -> LockEdge_Node name

-- | Every node of the generated flake's lock, keyed by node id. Node ids are
-- the input names, which are unique by construction, and an input that is only
-- a @follows@ has no node at all: it lives on the edge that reaches it.
lockNodes :: FlattenedFlake -> Map InputName LockNode
lockNodes flake =
  Map.insert flake.flattenedFlake_sourceName sourceNode $
    Map.mapMaybe inputLockNode flake.flattenedFlake_inputs
  where
    sourceNode =
      LockNode
        { lockNode_edges = LockEdge_Follows . rootFollows <$> flake.flattenedFlake_sourceEdges
        , lockNode_refs = Just flake.flattenedFlake_sourceRefs
        , lockNode_isFlake = True
        }

inputLockNode :: FlattenedInput -> Maybe LockNode
inputLockNode input = case input.flattenedInput_ref of
  Left _ -> Nothing
  Right refs ->
    Just
      LockNode
        { lockNode_edges = LockEdge_Follows . rootFollows <$> input.flattenedInput_edges
        , lockNode_refs = Just refs
        , lockNode_isFlake = input.flattenedInput_isFlake
        }

-- | Every edge of a non-root node points at one of our root-level inputs,
-- which from inside the lock is a one-step walk from the root.
rootFollows :: InputName -> FollowsPath
rootFollows name = FollowsPath [name]

-- | Render the @flake.lock@ of a packed thunk whose upstream is not a flake.
-- It has the one input the flake declares, and nothing else.
renderSourceOnlyFlakeLock :: NodeRefs -> LBS.ByteString
renderSourceOnlyFlakeLock srcRefs =
  renderLockNodes
    LockNode
      { lockNode_edges = Map.singleton sourceOnlyInputName $ LockEdge_Node sourceOnlyInputName
      , lockNode_refs = Nothing
      , lockNode_isFlake = True
      }
    $ Map.singleton
      sourceOnlyInputName
      LockNode
        { lockNode_edges = Map.empty
        , lockNode_refs = Just srcRefs
        , lockNode_isFlake = False
        }

renderLockNodes :: LockNode -> Map InputName LockNode -> LBS.ByteString
renderLockNodes root nodes = encodePretty' lockJsonConfig $ lockJson root nodes

lockJson :: LockNode -> Map InputName LockNode -> Aeson.Value
lockJson root nodes =
  Aeson.object
    [ ("nodes", Aeson.object $ nodeEntry rootId root : (uncurry nodeEntry <$> Map.toList nodes))
    , ("root", Aeson.String $ unInputName rootId)
    , ("version", lockVersion)
    ]
  where
    rootId = rootLockNodeId nodes
    nodeEntry name node = (Key.fromText $ unInputName name, lockNodeJson node)

-- | The id of the lock's root node. Nix takes it from the top-level @root@
-- field rather than by name, so it can step aside on the off chance upstream
-- has an input called @root@.
rootLockNodeId :: Map InputName LockNode -> InputName
rootLockNodeId nodes = freshInputName (Map.keysSet nodes) $ InputName "root"

lockNodeJson :: LockNode -> Aeson.Value
lockNodeJson node = Aeson.object $ fold [edges, refs, flake]
  where
    edges, refs, flake :: [Aeson.Pair]
    edges = [("inputs", lockEdgesJson node.lockNode_edges) | not $ Map.null node.lockNode_edges]
    refs = foldMap refsJson node.lockNode_refs
    -- Nix takes an input to be a flake unless told otherwise.
    flake = [("flake", Aeson.Bool False) | not node.lockNode_isFlake]
    refsJson r =
      [ ("locked", flakeRefJson r.nodeRefs_locked)
      , ("original", flakeRefJson r.nodeRefs_original)
      ]

lockEdgesJson :: Map InputName LockEdge -> Aeson.Value
lockEdgesJson edges =
  Aeson.object [(Key.fromText $ unInputName name, lockEdgeJson edge) | (name, edge) <- Map.toList edges]

lockEdgeJson :: LockEdge -> Aeson.Value
lockEdgeJson = \case
  LockEdge_Node target -> Aeson.String $ unInputName target
  LockEdge_Follows followsPath -> Aeson.toJSON $ unInputName <$> unFollowsPath followsPath

flakeRefJson :: FlakeRef -> Aeson.Value
flakeRefJson flakeRef =
  Aeson.object [(Key.fromText $ unAttrName name, refValueJson value) | (name, value) <- Map.toList $ unFlakeRef flakeRef]

refValueJson :: FlakeRefValue -> Aeson.Value
refValueJson = \case
  FlakeRefValue_String s -> Aeson.String s
  FlakeRefValue_Bool b -> Aeson.Bool b
  FlakeRefValue_Int n -> Aeson.Number $ fromInteger n

-- | Two-space indentation with sorted keys and a trailing newline, which is
-- how Nix writes a lock itself. Nothing reads the file back by content, but a
-- generated file that a person may open should not look foreign next to the
-- ones Nix wrote.
lockJsonConfig :: Config
lockJsonConfig =
  defConfig
    { confIndent = Spaces 2
    , confCompare = compare
    , confTrailingNewline = True
    }

-- | The lock format this writes. Nix has kept this at 7 since flakes were
-- introduced, and refuses anything newer than it knows.
lockVersion :: Aeson.Value
lockVersion = Aeson.Number 7

-- | The name a thunk of a repository that is not a flake gives its one input.
sourceOnlyInputName :: InputName
sourceOnlyInputName = InputName "src"

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
flattenContext :: NodeRefs -> FlakeLock -> FlattenContext
flattenContext srcRefs lock = ctx
  where
    names = nodeInputNames lock
    ctx =
      FlattenContext
        { flattenContext_sourceName = sourceInputName $ takenInputNames lock names
        , flattenContext_sourceRefs = srcRefs
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
    , flattenedInput_edges = overridableEdges ctx $ nodeEdgesOf ctx.flattenContext_lock nodeId
    }

-- | The references the thunk should bind a single node to.
nodeRef :: FlattenContext -> NodeId -> FlakeNode -> Either FollowsPath NodeRefs
nodeRef ctx nodeId node = fromMaybe (Left $ aliasPath ctx nodeId) $ case relativePathOf node of
  Nothing -> Right <$> (fetchableNodeRefs =<< node.flakeNode_locked)
  Just rel -> do
    parent <- parentRefsOf ctx nodeId
    Right <$> withRelativeDirRefs parent rel

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
  guard $
    Map.lookup (AttrName "type") (unFlakeRef locked)
      /= Just (FlakeRefValue_String "path")
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
    <$> withRelativeDir parent.nodeRefs_original rel
    <*> withRelativeDir parent.nodeRefs_locked rel

parentRefsOf :: FlattenContext -> NodeId -> Maybe NodeRefs
parentRefsOf ctx nodeId = do
  origin <- Map.lookup nodeId ctx.flattenContext_origins
  site <- origin.nodeOrigin_declaredIn
  case site of
    DeclarationSite_Source -> Just ctx.flattenContext_sourceRefs
    DeclarationSite_Node parentId -> rightToMaybe =<< Map.lookup parentId ctx.flattenContext_refs

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

nodeEdgesOf :: FlakeLock -> NodeId -> Map InputName NodeId
nodeEdgesOf lock nodeId = Map.mapMaybe (resolveEdge lock) $ edgesOf lock nodeId

nonRootNodes :: FlakeLock -> Map NodeId FlakeNode
nonRootNodes lock = Map.delete lock.flakeLock_root lock.flakeLock_nodes

-- | The name the thunk's own source is bound to. Upstream's inputs occupy
-- root-level names too, so on the off chance one of them is already called
-- @upstream@, pick another name rather than colliding.
sourceInputName :: Set InputName -> InputName
sourceInputName taken = freshInputName taken $ InputName "upstream"

-- | Every root-level name the generated flake binds for upstream: the name
-- each node is exposed under, plus the alias names upstream also used.
takenInputNames :: FlakeLock -> Map NodeId InputName -> Set InputName
takenInputNames lock names =
  Set.fromList (toFlakeId <$> Map.keys (rootEdges lock)) <> Set.fromList (toList names)

-- | Where each node of upstream's lock was first reached from.
--
-- Two walks, because only a direct edge is a declaration. A @follows@ rebinds
-- a node that some other flake declared, so treating the flake that follows it
-- as its parent would resolve a relative path against the wrong repository:
-- the first walk therefore crosses direct edges only. The second crosses every
-- edge, so that a node reachable no other way still has a path to be aliased
-- by, but it contributes no declaration site.
discoverOrigins :: FlakeLock -> Map NodeId NodeOrigin
discoverOrigins lock =
  Map.union
    (originsVia declaredEdgesOf lock)
    (undeclared $ originsVia edgesOf lock)

-- | Breadth-first walk of upstream's lock from its root over a chosen subset
-- of each node's edges, recording where each node was first reached from.
originsVia
  :: (FlakeLock -> NodeId -> Map InputName FlakeEdge)
  -> FlakeLock
  -> Map NodeId NodeOrigin
originsVia edges lock = go Map.empty $ childrenVia edges lock lock.flakeLock_root []
  where
    go acc [] = acc
    go acc ((site, prefix, nodeId) : rest)
      | nodeId `Map.member` acc || nodeId == lock.flakeLock_root = go acc rest
      | otherwise =
          go
            (Map.insert nodeId (NodeOrigin (Just site) prefix) acc)
            (rest <> childrenVia edges lock nodeId prefix)

childrenVia
  :: (FlakeLock -> NodeId -> Map InputName FlakeEdge)
  -> FlakeLock
  -> NodeId
  -> [InputName]
  -> [(DeclarationSite, [InputName], NodeId)]
childrenVia edges lock nodeId prefix =
  [ (site, prefix <> [name], child)
  | (name, edge) <- Map.toList $ edges lock nodeId
  , Just child <- [resolveEdge lock edge]
  ]
  where
    -- The root's own children are declared by the thunk's source, which is not
    -- itself part of upstream's lock.
    site
      | nodeId == lock.flakeLock_root = DeclarationSite_Source
      | otherwise = DeclarationSite_Node nodeId

-- | Drop the declaration sites of a walk that was allowed to cross @follows@
-- edges. Whatever it recorded as a parent may not be one.
undeclared :: Map NodeId NodeOrigin -> Map NodeId NodeOrigin
undeclared = fmap $ \origin -> origin {nodeOrigin_declaredIn = Nothing}

-- | The edges of a node that declare what they point at, which a @follows@
-- does not.
declaredEdgesOf :: FlakeLock -> NodeId -> Map InputName FlakeEdge
declaredEdgesOf lock = Map.filter declares . edgesOf lock
  where
    declares = \case
      FlakeEdge_Node _ -> True
      FlakeEdge_Follows _ -> False

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
    , nodeRefs_locked = FlakeRef $ Map.union (unFlakeRef original) (unFlakeRef discovered)
    }
  where
    original = fetchableRef srcRef

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
