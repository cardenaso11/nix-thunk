-- | Writing the @flake.lock@ of a packed thunk.
--
-- Written here rather than by running @nix flake lock@ on the generated flake.
-- Locking resolves every input a flake declares, and the generated one
-- declares an input per node of upstream's lock, so packing would fetch
-- upstream's whole transitive closure and fail on any single pin it could not
-- reach. Every reference and hash the lock needs is already in upstream's own
-- lock, which makes this a relabelling of that rather than new information.
--
-- The types here describe a lock being written, and are deliberately not the
-- ones in "Nix.Thunk.Flake.Upstream", which describe one being read.
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
-- Parameterised by how its edges are keyed, because the two kinds of node key
-- them differently: the lock's root binds our own inputs, so 'FlakeId', while
-- every other node reproduces the names upstream used, so 'InputName'.
data LockNode name = LockNode
  { lockNode_edges :: Map name LockEdge
  , lockNode_refs :: Maybe NodeRefs
  -- ^ 'Nothing' for the root node, which is the flake being locked.
  , lockNode_isFlake :: Bool
  }
  deriving stock (Eq, Show)

-- | An edge of a @flake.lock@: a node id, or a path walked from the root.
--
-- Both are names we chose. Node ids in a lock may be any string, but ours are
-- the input names, and a path is walked as flake identifiers whoever wrote it.
data LockEdge
  = LockEdge_Node FlakeId
  | LockEdge_Follows (FollowsPath FlakeId)
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Lock nodes
--------------------------------------------------------------------------------

-- | Render the @flake.lock@ of a packed thunk whose upstream is itself a flake.
--
-- Only valid when 'flattenedFlake_complete' holds: an input the generated
-- flake did not fully declare is one Nix would have had to work out for
-- itself, and a lock missing it is not a lock.
renderFlakeLock :: FlattenedFlake -> LBS.ByteString
renderFlakeLock flake = renderLockNodes (rootLockNode flake) (lockNodes flake)

-- | The root node of the generated flake's lock: an edge to the thunk's source
-- and one to every input, with the aliases recorded as the paths they follow.
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

-- | Every node of the generated flake's lock, keyed by node id. Node ids are
-- the input names, which are unique by construction, and an input that is only
-- a @follows@ has no node at all: it lives on the edge that reaches it.
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

-- | Every edge of a non-root node points at one of our root-level inputs,
-- which from inside the lock is a one-step walk from the root.
rootFollows :: FlakeId -> FollowsPath FlakeId
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
        { -- Keyed as every non-root node is, though this one has no edges to key.
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
    -- The root node and the rest key their edges differently, so this is
    -- polymorphic in both, and the two calls above instantiate it each way.
    nodeEntry :: (IsInputName name, IsInputName edgeKey) => name -> LockNode edgeKey -> Aeson.Pair
    nodeEntry name node = (nameKey name, lockNodeJson node)

-- | The id of the lock's root node. Nix takes it from the top-level @root@
-- field rather than by name, so it can step aside on the off chance upstream
-- has an input called @root@.
rootLockNodeId :: Map FlakeId (LockNode key) -> FlakeId
rootLockNodeId nodes = freshInputName (Map.keysSet nodes) $ toFlakeId $ InputName "root"

lockNodeJson :: IsInputName name => LockNode name -> Aeson.Value
lockNodeJson node = Aeson.object $ edges <> refs <> flake
  where
    edges, refs, flake :: [Aeson.Pair]
    edges = [("inputs", lockEdgesJson node.lockNode_edges) | not $ Map.null node.lockNode_edges]
    refs = foldMap refsJson node.lockNode_refs
    -- Nix takes an input to be a flake unless told otherwise.
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
