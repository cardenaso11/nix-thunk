-- | This module defines upstream's @flake.lock@, the way to walk it, and the
-- origin of each node.
--
-- Every type here describes a lock that somebody else wrote. The lock that this
-- package /writes/ has a different shape, and "Nix.Thunk.Flake.Lockfile"
-- defines it.
module Nix.Thunk.Flake.Upstream where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Nix.Thunk.Flake.Name
import Nix.Thunk.Flake.Ref

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The id of a node in a @flake.lock@, e.g. @nixpkgs@ or @nixpkgs_2@.
--
-- This type stays separate from 'InputName', and both hold a 'Text'.
-- 'nodeIdToInputName' is the only function that converts one to the other.
newtype NodeId = NodeId {unNodeId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

-- | A @flake.lock@: a graph of pinned inputs, and the id of the node where the
-- graph starts.
--
-- > {
-- >   "nodes": {
-- >     "root": {
-- >       "inputs": {
-- >         "sub": "sub",
-- >         "subAlias": ["sub"]
-- >       }
-- >     },
-- >     "sub": {
-- >       "locked":   { "path": "./sub", "type": "path" },
-- >       "original": { "path": "./sub", "type": "path" }
-- >     }
-- >   },
-- >   "root": "root",
-- >   "version": 7
-- > }
--
-- The top-level @root@ field names the entry node, so the node itself does not
-- have to carry the name @root@. This type ignores the @version@ field.
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

-- | One node of a lock: a pinned input, with edges to the inputs that the flake
-- gave it.
--
-- The JSON can omit every field, and each absence has its own meaning:
--
-- * The root node has no reference.
-- * A leaf has no edges.
-- * A lock writes @flake@ only when the value is false, because Nix treats an
--   input as a flake by default.
data FlakeNode = FlakeNode
  { flakeNode_inputs :: Map InputName FlakeEdge
  , flakeNode_locked :: Maybe FlakeRef
  -- ^ The reference that the input resolved to, with the hashes that a fetch
  -- discovered.
  , flakeNode_original :: Maybe FlakeRef
  -- ^ The reference that the declaring flake asked for.
  , flakeNode_isFlake :: Bool
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON FlakeNode where
  parseJSON = Aeson.withObject "FlakeNode" $ \o ->
    -- `fold` collapses a missing `inputs` to the empty map. This absence is the
    -- only one with a sensible default rather than a meaning.
    FlakeNode . fold
      <$> o Aeson..:? "inputs"
      <*> o Aeson..:? "locked"
      <*> o Aeson..:? "original"
      <*> (fromMaybe True <$> o Aeson..:? "flake")

-- | An edge in a lock graph: either a node id, or a @follows@ path that starts
-- at the root node.
--
-- Only the type of the JSON value separates the two. A string is a node id, and
-- an array is a @follows@ path. The two differ: @"sub"@ names a node, and
-- @["sub"]@ is a walk from the root that can reach a different node.
data FlakeEdge
  = FlakeEdge_Node NodeId
  | FlakeEdge_Follows (FollowsPath InputName)
  deriving stock (Eq, Ord, Show)

instance Aeson.FromJSON FlakeEdge where
  parseJSON = \case
    v@(Aeson.String _) -> FlakeEdge_Node <$> Aeson.parseJSON v
    v@(Aeson.Array _) -> FlakeEdge_Follows <$> Aeson.parseJSON v
    v -> Aeson.typeMismatch "String or Array" v

-- | The way that a walk first reached a node from upstream's root. This record
-- lets us resolve a relative path input against its parent. When we cannot
-- resolve such an input, this record gives us the path for an alias.
data NodeOrigin = NodeOrigin
  { nodeOrigin_declaredIn :: Maybe DeclarationSite
  -- ^ 'Nothing' when a walk reached the node only across a @follows@ edge. Such
  -- an edge says nothing about the origin of the node.
  , nodeOrigin_path :: [InputName]
  -- ^ Edge names from upstream's root down to this node.
  }
  deriving stock (Eq, Show)

-- | The flake that declared a node. A relative path in that node is relative to
-- this flake.
data DeclarationSite
  = -- | The repository that the thunk points at. The node sits directly under
    -- upstream's root.
    DeclarationSite_Source
  | -- | Another node of upstream's lock.
    DeclarationSite_Node NodeId
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Edges
--------------------------------------------------------------------------------

rootEdges :: FlakeLock -> Map InputName NodeId
rootEdges lock = Map.mapMaybe (resolveEdge lock) $ edgesOf lock lock.flakeLock_root

nodeEdgesOf :: FlakeLock -> NodeId -> Map InputName NodeId
nodeEdgesOf lock nodeId = Map.mapMaybe (resolveEdge lock) $ edgesOf lock nodeId

-- | The edges of a node that declare what they point at. A @follows@ edge does
-- not declare that.
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

-- | Walks a @follows@ path from the root of the lock. The walk does not start
-- at the node that holds the edge, and the depth of that node makes no
-- difference.
followPath :: Int -> FlakeLock -> NodeId -> [InputName] -> Maybe NodeId
followPath _ _ nodeId [] = Just nodeId
followPath fuel lock nodeId (name : rest) = do
  edge <- Map.lookup name $ edgesOf lock nodeId
  next <- resolveEdgeWithin fuel lock edge
  followPath fuel lock next rest

-- | The limit on the length of a @follows@ chain that we walk. Without this
-- limit, a malformed lock hangs the caller.
followsFuel :: Int
followsFuel = 64

--------------------------------------------------------------------------------
-- Nodes
--------------------------------------------------------------------------------

nonRootNodes :: FlakeLock -> Map NodeId FlakeNode
nonRootNodes lock = Map.delete lock.flakeLock_root lock.flakeLock_nodes

-- | The node ids of upstream's lock become root-level input names of the
-- thunk's flake. That match is the reason why @mythunk\/nixpkgs@ resolves.
nodeIdToInputName :: NodeId -> InputName
nodeIdToInputName = InputName . unNodeId

--------------------------------------------------------------------------------
-- Origins
--------------------------------------------------------------------------------

-- | The origin of each node of upstream's lock.
--
-- This function makes two walks, because only a direct edge is a declaration. A
-- @follows@ edge rebinds a node that another flake declared. So the flake that
-- follows a node is not its parent, and that flake would resolve a relative
-- path against the wrong repository. The first walk therefore crosses direct
-- edges only. The second walk crosses every edge and records no declaration
-- site, so every reachable node gets a path for an alias.
discoverOrigins :: FlakeLock -> Map NodeId NodeOrigin
discoverOrigins lock =
  Map.union
    (originsVia declaredEdgesOf lock)
    (undeclared $ originsVia edgesOf lock)

-- | Walks upstream's lock in breadth-first order from the root. The walk
-- crosses a chosen subset of each node's edges, and it records the origin of
-- each node.
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
    -- The thunk's source declares the root's own children. That source is not
    -- part of upstream's lock.
    site
      | nodeId == lock.flakeLock_root = DeclarationSite_Source
      | otherwise = DeclarationSite_Node nodeId

-- | Drops the declaration sites of a walk that crossed @follows@ edges. Such a
-- walk can record a node that is not a parent.
undeclared :: Map NodeId NodeOrigin -> Map NodeId NodeOrigin
undeclared = fmap $ \origin -> origin {nodeOrigin_declaredIn = Nothing}
