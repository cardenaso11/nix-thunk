-- | Upstream's @flake.lock@: the model of one, how to walk it, and where each
-- node came from.
--
-- Everything here is about a lock somebody else wrote. The lock this package
-- /writes/ is a different shape and lives in "Nix.Thunk.Flake.Lockfile".
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
-- Held apart from 'InputName' although both are 'Text' underneath, because
-- much of this package is about not confusing the two: see
-- 'Nix.Thunk.Flake.Flatten.nodeInputNames' for what goes wrong when they are.
-- 'nodeIdToInputName' is the only crossing.
newtype NodeId = NodeId { unNodeId :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.FromJSON, Aeson.FromJSONKey)

-- | A @flake.lock@: a graph of pinned inputs, and the id of the node the graph
-- is entered at.
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
-- The entry node is named by the top-level @root@ field rather than by being
-- called @root@, which is what lets 'Nix.Thunk.Flake.Lockfile.rootLockNodeId'
-- step aside when writing one of these. The @version@ is read past;
-- 'Nix.Thunk.Flake.Lockfile.lockVersion' is what gets written back.
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

-- | One node of a lock: an input, pinned, with edges to the inputs it was
-- given in turn.
--
-- Every field can be missing from the JSON, and each absence means something
-- of its own. The root node has no reference, a leaf has no edges, and @flake@
-- is written only when it is false, since Nix takes an input to be a flake
-- unless told otherwise. 'Nix.Thunk.Flake.Lockfile.lockNodeJson' reproduces
-- that same asymmetry on the way back out.
data FlakeNode = FlakeNode
  { flakeNode_inputs :: Map InputName FlakeEdge
  , flakeNode_locked :: Maybe FlakeRef
  -- ^ What the input resolved to, carrying the hashes a fetch discovered. Left
  -- as a 'Maybe' rather than defaulted because a node without one is a node
  -- 'Nix.Thunk.Flake.Flatten.nodeRef' has to make a decision about.
  , flakeNode_original :: Maybe FlakeRef
  -- ^ What the flake that declared the input asked for. Only consulted to find
  -- out whether that was a relative path, by 'relativePathOf'.
  , flakeNode_isFlake :: Bool
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON FlakeNode where
  parseJSON = Aeson.withObject "FlakeNode" $ \o ->
    -- `fold` collapses a missing `inputs` to the empty map, which is the only
    -- absence with a sensible default rather than a meaning.
    FlakeNode . fold
      <$> o Aeson..:? "inputs"
      <*> o Aeson..:? "locked"
      <*> o Aeson..:? "original"
      <*> (fromMaybe True <$> o Aeson..:? "flake")

-- | An edge in a lock graph: either a node id, or a @follows@ path interpreted
-- from the root node.
--
-- Nothing in the JSON distinguishes the two but the type of the value, a
-- string against an array, and the two mean quite different things: @"sub"@
-- names a node, while @["sub"]@ is a walk from the root that may well land
-- somewhere else. Reading that distinction into a type here is what keeps
-- 'resolveEdge' the only code that has to know how to follow one.
data FlakeEdge
  = FlakeEdge_Node NodeId
  | FlakeEdge_Follows (FollowsPath InputName)
  deriving stock (Eq, Ord, Show)

instance Aeson.FromJSON FlakeEdge where
  parseJSON = \case
    v@(Aeson.String _) -> FlakeEdge_Node <$> Aeson.parseJSON v
    v@(Aeson.Array _) -> FlakeEdge_Follows <$> Aeson.parseJSON v
    v -> Aeson.typeMismatch "String or Array" v

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

--------------------------------------------------------------------------------
-- Edges
--------------------------------------------------------------------------------

rootEdges :: FlakeLock -> Map InputName NodeId
rootEdges lock = Map.mapMaybe (resolveEdge lock) $ edgesOf lock lock.flakeLock_root

nodeEdgesOf :: FlakeLock -> NodeId -> Map InputName NodeId
nodeEdgesOf lock nodeId = Map.mapMaybe (resolveEdge lock) $ edgesOf lock nodeId

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

-- | A @follows@ path is walked from the root of the lock, not from the node
-- that holds the edge, however deep that node sits.
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
-- Nodes
--------------------------------------------------------------------------------

nonRootNodes :: FlakeLock -> Map NodeId FlakeNode
nonRootNodes lock = Map.delete lock.flakeLock_root lock.flakeLock_nodes

-- | Upstream's lock node ids become root-level input names of the thunk's
-- flake. That identification is what makes @mythunk\/nixpkgs@ resolve, so it is
-- spelled out rather than left implicit in a shared 'Text'.
nodeIdToInputName :: NodeId -> InputName
nodeIdToInputName = InputName . unNodeId

--------------------------------------------------------------------------------
-- Origins
--------------------------------------------------------------------------------

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
undeclared = fmap $ \origin -> origin { nodeOrigin_declaredIn = Nothing }
