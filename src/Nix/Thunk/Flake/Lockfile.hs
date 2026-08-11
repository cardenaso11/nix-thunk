-- | This module renders the @flake.lock@ of a packed thunk.
--
-- This module writes the lock, and it does not run @nix flake lock@ on the
-- generated flake. A lock resolves every input that a flake declares. The
-- generated flake declares one input for each node of upstream's lock, so a
-- lock run would fetch upstream's whole transitive closure. The run would also
-- fail on any single pin that it could not reach. Upstream's own lock already
-- holds every reference and hash that our lock needs, so this module relabels
-- upstream's lock, and it adds no new information.
--
-- The types here describe a lock that we write. The types in
-- "Nix.Thunk.Flake.Upstream" describe a lock that we read.
module Nix.Thunk.Flake.Lockfile where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (Config (confCompare, confIndent, confTrailingNewline), Indent (Spaces), defConfig, encodePretty')
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

import Nix.Thunk.Flake.Flatten
import Nix.Thunk.Flake.Name
import Nix.Thunk.Flake.Ref

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | One node of a @flake.lock@.
--
-- This type takes the key of its edges as a parameter, because the two kinds of
-- node use different keys. The lock's root binds our own inputs, so it uses a
-- 'FlakeId'. Every other node repeats the names that upstream used, so it uses
-- an 'InputName'.
data LockNode name = LockNode
  { lockNode_edges :: Map name LockEdge
  , lockNode_refs :: Maybe NodeRefs
  -- ^ 'Nothing' for the root node, which is the flake that we lock.
  , lockNode_isFlake :: Bool
  }
  deriving stock (Eq, Show)

-- | An edge of a @flake.lock@: a node id, or a path from the root.
--
-- We chose both names. A node id in a lock can hold any string, and our node
-- ids hold the input names. Nix walks a path as flake identifiers, whatever
-- code wrote the lock.
data LockEdge
  = LockEdge_Node FlakeId
  | LockEdge_Follows (FollowsPath FlakeId)
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Lock nodes
--------------------------------------------------------------------------------

-- | The @flake.lock@ of a packed thunk whose upstream is itself a flake.
--
-- The result is valid only when 'flattenedFlake_complete' holds. The generated
-- flake can leave an input partly declared, and Nix would then have to resolve
-- that input itself. A lock without that input is not a complete lock.
renderFlakeLock :: FlattenedFlake -> LBS.ByteString
renderFlakeLock flake = renderLockNodes (rootLockNode flake) (lockNodes flake)

-- | The root node of the generated flake's lock. The node holds an edge to the
-- thunk's source, and one edge to every input. Each alias holds the path that
-- it follows.
rootLockNode :: FlattenedFlake -> LockNode FlakeId
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

rootLockEdge :: FlakeId -> FlattenedInput -> LockEdge
rootLockEdge name input = case input.flattenedInput_ref of
  Left followsPath -> LockEdge_Follows followsPath
  Right _ -> LockEdge_Node name

-- | Every node of the generated flake's lock, with a node id as the key. A node
-- id is an input name, and the input names are unique. An input that holds only
-- a @follows@ gets no node, because the edge that reaches it carries the whole
-- @follows@.
lockNodes :: FlattenedFlake -> Map FlakeId (LockNode InputName)
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

inputLockNode :: FlattenedInput -> Maybe (LockNode InputName)
inputLockNode input = case input.flattenedInput_ref of
  Left _ -> Nothing
  Right refs ->
    Just
      LockNode
        { lockNode_edges = LockEdge_Follows . rootFollows <$> input.flattenedInput_edges
        , lockNode_refs = Just refs
        , lockNode_isFlake = input.flattenedInput_isFlake
        }

-- | Every edge of a non-root node points at one of our root-level inputs. From
-- inside the lock, such an input is a one-step walk from the root.
rootFollows :: FlakeId -> FollowsPath FlakeId
rootFollows name = FollowsPath [name]

-- | The @flake.lock@ of a packed thunk whose upstream is not a flake. The lock
-- holds the one input that the flake declares, and it holds nothing else.
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
        { -- This node uses the same key as every non-root node, and this node
          -- has no edges.
          lockNode_edges = Map.empty :: Map InputName LockEdge
        , lockNode_refs = Just srcRefs
        , lockNode_isFlake = False
        }

--------------------------------------------------------------------------------
-- JSON
--------------------------------------------------------------------------------

renderLockNodes
  :: (IsInputName rootKey, IsInputName key)
  => LockNode rootKey
  -> Map FlakeId (LockNode key)
  -> LBS.ByteString
renderLockNodes root nodes = encodePretty' lockJsonConfig $ lockJson root nodes

lockJson
  :: (IsInputName rootKey, IsInputName key)
  => LockNode rootKey
  -> Map FlakeId (LockNode key)
  -> Aeson.Value
lockJson root nodes =
  Aeson.object
    [ ("nodes", Aeson.object $ nodeEntry rootId root : (uncurry nodeEntry <$> Map.toList nodes))
    , ("root", Aeson.String $ nameText rootId)
    , ("version", lockVersion)
    ]
  where
    rootId = rootLockNodeId nodes
    -- The root node and the other nodes use different key types for their
    -- edges, so `nodeEntry` is polymorphic in both keys.
    nodeEntry :: (IsInputName name, IsInputName edgeKey) => name -> LockNode edgeKey -> Aeson.Pair
    nodeEntry name node = (nameKey name, lockNodeJson node)

-- | The id of the lock's root node. Nix reads this id from the top-level @root@
-- field, and not from the name. So this function can choose another name when
-- upstream already has an input called @root@.
rootLockNodeId :: Map FlakeId (LockNode key) -> FlakeId
rootLockNodeId nodes = freshInputName (Map.keysSet nodes) $ toFlakeId $ InputName "root"

lockNodeJson :: IsInputName name => LockNode name -> Aeson.Value
lockNodeJson node = Aeson.object $ edges <> refs <> flake
  where
    edges, refs, flake :: [Aeson.Pair]
    edges = [("inputs", lockEdgesJson node.lockNode_edges) | not $ Map.null node.lockNode_edges]
    refs = foldMap refsJson node.lockNode_refs
    -- Nix treats an input as a flake by default.
    flake = [("flake", Aeson.Bool False) | not node.lockNode_isFlake]
    refsJson r =
      [ ("locked", flakeRefJson r.nodeRefs_locked)
      , ("original", flakeRefJson $ unFetchableRef r.nodeRefs_original)
      ]

lockEdgesJson :: IsInputName name => Map name LockEdge -> Aeson.Value
lockEdgesJson edges =
  Aeson.object [(nameKey name, lockEdgeJson edge) | (name, edge) <- Map.toList edges]

lockEdgeJson :: LockEdge -> Aeson.Value
lockEdgeJson = \case
  LockEdge_Node target -> Aeson.String $ nameText target
  LockEdge_Follows followsPath -> Aeson.toJSON $ nameText <$> unFollowsPath followsPath

flakeRefJson :: FlakeRef -> Aeson.Value
flakeRefJson flakeRef =
  Aeson.object [(Key.fromText $ unAttrName name, refValueJson value) | (name, value) <- Map.toList $ unFlakeRef flakeRef]

refValueJson :: FlakeRefValue -> Aeson.Value
refValueJson = \case
  FlakeRefValue_String s -> Aeson.String s
  FlakeRefValue_Bool b -> Aeson.Bool b
  FlakeRefValue_Int n -> Aeson.Number $ fromInteger n

-- | A name as a JSON object key.
nameKey :: IsInputName name => name -> Key.Key
nameKey = Key.fromText . nameText

-- | Two-space indentation, sorted keys, and a trailing newline. Nix writes a
-- lock in the same shape, so this file matches the locks that Nix wrote.
lockJsonConfig :: Config
lockJsonConfig =
  defConfig
    { confIndent = Spaces 2
    , confCompare = compare
    , confTrailingNewline = True
    }

-- | The lock format that this module writes. Nix introduced version 7 with the
-- first release of flakes, and Nix still uses it. Nix refuses a version that it
-- does not know.
lockVersion :: Aeson.Value
lockVersion = Aeson.Number 7

-- | The lock of a flake with no inputs. This module writes the text directly,
-- and it does not call Nix. So a pack of a repository that is not a flake needs
-- no flakes feature.
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
