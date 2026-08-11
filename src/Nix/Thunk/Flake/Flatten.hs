-- | This module translates upstream's lock graph into the inputs of the thunk's
-- own flake.
--
-- A consumer writes this:
--
-- > inputs.someinput.follows = "mythunk/nixpkgs";       # read
-- > inputs.mythunk.inputs.nixpkgs.follows = "nixpkgs";  # override
--
-- Nix resolves both lines against the lock graph. So the thunk's flake
-- reproduces upstream's graph as its own:
--
-- * It binds one root-level input for each node of upstream's lock.
-- * It pins each input to the revision that upstream locked.
-- * It restates every edge as a @follows@.
--
-- A consumer can read a name because the flake binds that name. A consumer can
-- override a name because the flake binds a real node, not an alias.
module Nix.Thunk.Flake.Flatten where

import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

import Nix.Thunk.Flake.Name
import Nix.Thunk.Flake.Ref
import Nix.Thunk.Flake.Upstream

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The generated flake as a value. It holds everything that the renderers
-- need, and it says nothing about the syntax.
--
-- Each comment below names the field that the part came from.
--
-- > {
-- >   inputs = {
-- >     "upstream" = {                                  # sourceName
-- >       rev = "513fbe...";                            # sourceRefs
-- >       type = "git";
-- >       url = "file:///.../upstream";
-- >       inputs = {                                    # sourceEdges
-- >         "sub" = { follows = "sub"; };
-- >         "subAlias" = { follows = "sub"; };
-- >       };
-- >     };
-- >     "sub" = {                                       # inputs, with a ref
-- >       dir = "sub";
-- >       rev = "513fbe...";
-- >       type = "git";
-- >       url = "file:///.../upstream";
-- >     };
-- >     "subAlias" = { follows = "sub"; };              # inputs, aliased
-- >   };
-- >   outputs = inputs: inputs."upstream".outputs;
-- > }
data FlattenedFlake = FlattenedFlake
  { flattenedFlake_sourceName :: FlakeId
  -- ^ The name of the repository that the thunk points at. This name is not a
  -- constant, because one of upstream's own inputs can already hold it.
  , flattenedFlake_sourceRefs :: NodeRefs
  , flattenedFlake_sourceEdges :: Map InputName FlakeId
  -- ^ Upstream's own inputs, redirected onto ours. The key is upstream's name,
  -- and an override must match it exactly. The value is our name, and Nix must
  -- accept it in a @follows@.
  , flattenedFlake_inputs :: Map FlakeId FlattenedInput
  -- ^ The key is the name that a consumer writes. Upstream's lock uses a node
  -- id as its key, and the two keys are not the same.
  , flattenedFlake_complete :: Bool
  -- ^ 'True' when every node of upstream's lock became an input with a
  -- reference of its own, and this flake restated every edge. This flake then
  -- declares every one of its own inputs, and
  -- 'Nix.Thunk.Flake.Lockfile.renderFlakeLock' can describe it without a call
  -- to Nix.
  }
  deriving stock (Eq, Show)

-- | One root-level input of the generated flake.
--
-- The rendered output above shows the 'Either' in the first field: @sub@ has a
-- @rev@, and @subAlias@ has a @follows@.
data FlattenedInput = FlattenedInput
  { flattenedInput_ref :: Either (FollowsPath FlakeId) NodeRefs
  -- ^ 'Right' is a real node, and a consumer can read /and/ override it. 'Left'
  -- holds a name for another input, and this module uses it when the node has
  -- no fetchable reference. A consumer can read that name, and a consumer
  -- cannot override it.
  , flattenedInput_isFlake :: Bool
  , flattenedInput_edges :: Map InputName FlakeId
  -- ^ The edges of this node, redirected onto our names. The key is upstream's
  -- name, and the value is our name.
  }
  deriving stock (Eq, Show)

-- | The values that the per-node translation reads. A relative path node
-- resolves against its parent, and this module can rewrite the parent's own
-- reference.
data FlattenContext = FlattenContext
  { flattenContext_sourceName :: FlakeId
  , flattenContext_sourceRefs :: NodeRefs
  , flattenContext_lock :: FlakeLock
  , flattenContext_origins :: Map NodeId NodeOrigin
  , flattenContext_refs :: Map NodeId NodeRefs
  -- ^ Only the nodes that got a reference of their own. A node that is absent
  -- here needs an alias.
  , flattenContext_names :: Map NodeId FlakeId
  }

--------------------------------------------------------------------------------
-- Flattening
--------------------------------------------------------------------------------

-- | Translates upstream's lock graph into the inputs of the thunk's own flake.
--
-- 'flattenedFlake_complete' walks every node of the lock, and not
-- 'nonRootNodes' only. The root's edges are 'flattenedFlake_sourceEdges', and
-- this module can drop one of those edges as well.
flattenLock
  :: NodeRefs
  -- ^ The reference to the repository that the thunk points at
  -> FlakeLock
  -- ^ Upstream's @flake.lock@
  -> FlattenedFlake
flattenLock srcRefs lock =
  FlattenedFlake
    { flattenedFlake_sourceName = ctx.flattenContext_sourceName
    , flattenedFlake_sourceRefs = ctx.flattenContext_sourceRefs
    , flattenedFlake_sourceEdges = overridableEdges ctx $ rootEdges lock
    , flattenedFlake_inputs = flattenedInputs ctx
    , flattenedFlake_complete = all (edgesFullyDeclared ctx) $ Map.keys lock.flakeLock_nodes
    }
  where
    ctx = flattenContext srcRefs lock

-- | Every root-level input that the generated flake binds for upstream.
--
-- The union with 'rootAliases' is left-biased, so a node always wins against an
-- alias of the same name. 'nodeInputNames' reserves the alias names, so no such
-- collision can happen.
flattenedInputs :: FlattenContext -> Map FlakeId FlattenedInput
flattenedInputs ctx =
  Map.union
    ( Map.mapKeys (inputNameOf ctx) $
        Map.mapMaybeWithKey (flattenedInput ctx) $
          nonRootNodes ctx.flattenContext_lock
    )
    (aliasInput <$> rootAliases ctx)

-- | 'True' when the generated flake declares every input that a node of
-- upstream's lock declares.
--
-- 'overridableEdges' drops an edge that it cannot restate:
--
-- * The edge points at a node with no reference of its own.
-- * The edge holds a @follows@ that leaves the graph.
-- * Nix does not accept the edge's name in an override.
--
-- Nix must settle every dropped edge from upstream's own lock. This package
-- cannot write a lock that records upstream's inputs, so this flake is then
-- incomplete.
edgesFullyDeclared :: FlattenContext -> NodeId -> Bool
edgesFullyDeclared ctx nodeId =
  Map.size (overridableEdges ctx $ nodeEdgesOf lock nodeId) == Map.size (edgesOf lock nodeId)
  where
    lock = ctx.flattenContext_lock

-- | 'True' when a node has a reference of its own.
--
-- 'overridableEdges' drops the edges of a node without a reference. Upstream's
-- lock then records the origin of that node, and our lock does not. The aliases
-- that 'rootAliases' adds are different: each alias is an extra name for a node
-- that has a reference.
nodeHasRef :: FlattenContext -> NodeId -> Bool
nodeHasRef ctx nodeId = Map.member nodeId ctx.flattenContext_refs

-- | The names that upstream bound at its root, mapped to the name of their
-- node. This map holds only the names that differ from the node's own name.
--
-- Several of upstream's root inputs can resolve to one node, because a
-- root-level @follows@ is another name for the node that it points at. Only one
-- of those names can become the name of the node. This module emits the other
-- names as aliases, so every name that upstream used still resolves through the
-- thunk.
--
-- This module drops a name that Nix cannot parse. An alias exists for a
-- @follows@ to point at, and a @follows@ cannot name such an alias.
rootAliases :: FlattenContext -> Map FlakeId FlakeId
rootAliases ctx =
  Map.fromList
    [ (aliasName, exposed)
    | (edgeName, target) <- Map.toList $ rootEdges ctx.flattenContext_lock
    , let exposed = inputNameOf ctx target
    , Just aliasName <- [flakeId edgeName]
    , aliasName /= exposed
    ]

-- | An input that is only a second name for a sibling input.
aliasInput :: FlakeId -> FlattenedInput
aliasInput target =
  FlattenedInput
    { flattenedInput_ref = Left $ FollowsPath [target]
    , flattenedInput_isFlake = True
    , flattenedInput_edges = Map.empty
    }

-- | The edges of a node that the generated flake restates as a @follows@ onto
-- one of its own inputs.
--
-- This function leaves two kinds of edge for upstream's own lock to resolve.
-- The first kind points at an aliased node. Such a node already carries a
-- @follows@. An override of that edge would make the alias follow itself, and
-- Nix rejects that as a follow cycle. The second kind carries a name that
-- is not a flake identifier. Nix writes an override as an attribute path, so
-- nobody can override such an edge.
--
-- The key stays an 'InputName'. The key must hold the name that upstream
-- declared, or the override names nothing. So this function drops an unusable
-- name, and it does not repair the name.
overridableEdges :: FlattenContext -> Map InputName NodeId -> Map InputName FlakeId
overridableEdges ctx = fmap (inputNameOf ctx) . Map.filterWithKey overridable
  where
    overridable edgeName target = isFlakeId edgeName && nodeHasRef ctx target

-- | Defines the context and the references together, and each one refers to the
-- other.
--
-- 'nodeRef' builds @flattenContext_refs@, and 'nodeRef' also reads
-- @flattenContext_refs@. A relative path node needs the reference of the node
-- that declared it, and this module can rewrite that reference. Laziness allows
-- this definition, and 'parentRefsOf' explains why it terminates.
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
        , flattenContext_refs = Map.mapMaybeWithKey (nodeRef ctx) $ nonRootNodes lock
        , flattenContext_names = names
        }

-- | The decision for one node of upstream's lock:
--
-- * The reference to bind the node to, when the node can have one.
-- * An alias, when the node cannot have a reference.
-- * The edges of the node that this function redirects.
--
-- This function returns 'Nothing' when the node can have neither a reference
-- nor an alias. The flake then omits the node and its edges. The flake loses a
-- readable name, and it does not lose correctness.
flattenedInput :: FlattenContext -> NodeId -> FlakeNode -> Maybe FlattenedInput
flattenedInput ctx nodeId node = do
  ref <- case Map.lookup nodeId ctx.flattenContext_refs of
    Just refs -> Just $ Right refs
    Nothing -> Left <$> aliasPath ctx nodeId
  pure
    FlattenedInput
      { flattenedInput_ref = ref
      , flattenedInput_isFlake = node.flakeNode_isFlake
      , flattenedInput_edges = overridableEdges ctx $ nodeEdgesOf ctx.flattenContext_lock nodeId
      }

-- | The references that the thunk binds a single node to, when the node can
-- have any. 'Nothing' means that 'aliasPath' handles the node.
nodeRef :: FlattenContext -> NodeId -> FlakeNode -> Maybe NodeRefs
nodeRef ctx nodeId node = case relativePathOf =<< node.flakeNode_original of
  Nothing -> fetchableNodeRefs =<< node.flakeNode_locked
  Just rel -> do
    parent <- parentRefsOf ctx nodeId
    withRelativeDirRefs parent rel

-- | The references of the node that declared this node. A relative path in this
-- node is relative to those references.
--
-- A node with no known declaration site fails the lookup, and this module then
-- aliases the node. It does not resolve the node against a guess. See
-- 'discoverOrigins'.
--
-- The second branch reads the map that this function helps to build. The walk
-- terminates, because a declaration site always sits strictly nearer the root
-- than the node that it declared. 'discoverOrigins' walks breadth-first, so the
-- chain always ends at 'DeclarationSite_Source'.
parentRefsOf :: FlattenContext -> NodeId -> Maybe NodeRefs
parentRefsOf ctx nodeId = do
  origin <- Map.lookup nodeId ctx.flattenContext_origins
  site <- origin.nodeOrigin_declaredIn
  case site of
    DeclarationSite_Source -> Just ctx.flattenContext_sourceRefs
    DeclarationSite_Node parentId -> Map.lookup parentId ctx.flattenContext_refs

-- | The position of a node relative to the thunk's source, for a node that
-- cannot have a reference of its own.
--
-- This function returns 'Nothing' when any step of the path holds a name that
-- Nix does not walk. The path must name upstream's inputs with upstream's own
-- names, and this module cannot repair a name that must match. The flake can
-- then offer the node under no name at all.
aliasPath :: FlattenContext -> NodeId -> Maybe (FollowsPath FlakeId)
aliasPath ctx nodeId = do
  origin <- Map.lookup nodeId ctx.flattenContext_origins
  steps <- traverse flakeId origin.nodeOrigin_path
  pure $ FollowsPath $ ctx.flattenContext_sourceName : steps

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

inputNameOf :: FlattenContext -> NodeId -> FlakeId
inputNameOf ctx nodeId =
  fromMaybe (toFlakeId $ nodeIdToInputName nodeId) $ Map.lookup nodeId ctx.flattenContext_names

-- | The name that the flake gives each node.
--
-- A lock node id is not an input name. Nix adds @_2@ and @_3@ suffixes to node
-- ids as it walks the graph. So upstream's own @nixpkgs@ often takes the key
-- @nixpkgs_2@, and a transitive dependency takes the plain key. A flake that
-- exposed node ids would therefore bind @mythunk\/nixpkgs@ to the wrong flake.
-- It would also retarget that name whenever upstream's input set changed.
--
-- This function gives upstream's root inputs the names that upstream used. A
-- node keeps its node id when only a transitive path reaches it. This function
-- adds a suffix to a collision. Nix must accept the result in every case, so
-- 'toFlakeId' makes the final decision.
nodeInputNames :: FlakeLock -> Map NodeId FlakeId
nodeInputNames lock = nameNodes reserved (rootInputNames lock) $ preferNodeId <$> Map.keys (nonRootNodes lock)
  where
    -- This set reserves every name that upstream bound at its root, and it also
    -- reserves the names that 'rootInputNames' dropped. 'rootAliases' claims
    -- those names, so a transitive node must not take one of them.
    reserved = rootBoundNames lock
    preferNodeId nodeId = (nodeId, nodeIdToInputName nodeId)

-- | The name that the flake gives each node that upstream binds at its root. A
-- root-level @follows@ can bind one node twice, and the node then keeps the
-- first of those names.
--
-- This function reserves no names, because these nodes compete only against
-- each other.
rootInputNames :: FlakeLock -> Map NodeId FlakeId
rootInputNames lock = nameNodes Set.empty Map.empty $ preferEdgeName <$> Map.toList (rootEdges lock)
  where
    preferEdgeName (edgeName, target) = (target, edgeName)

-- | Gives each node the first name that a caller offers for it.
--
-- A node that already has a name keeps that name. The callers offer names in
-- ascending order, so "first" means the alphabetically first name. That choice
-- is arbitrary. The answer lands in a committed file, so the answer must be the
-- same every time.
--
-- 'toFlakeId' can return a name that collides with a name that this function
-- already gave out. 'freshInputName' then adds a suffix to the second name.
-- This function recomputes the set of taken names at every step. That cost is
-- quadratic, and it does not matter, because a lock has tens of nodes.
nameNodes
  :: Set FlakeId
  -- ^ Names that something other than a node holds
  -> Map NodeId FlakeId
  -- ^ Names that this function gave out already
  -> [(NodeId, InputName)]
  -- ^ Each node, and the name that it prefers, with upstream's spelling
  -> Map NodeId FlakeId
nameNodes reserved = foldl' name
  where
    name acc (nodeId, preferred)
      | Map.member nodeId acc = acc
      | otherwise =
          Map.insert
            nodeId
            (freshInputName (reserved <> Set.fromList (toList acc)) $ toFlakeId preferred)
            acc

-- | The name that the flake gives the thunk's own source. Upstream's inputs
-- occupy root-level names as well, so one of them can already hold the name
-- @upstream@. This function then chooses another name.
sourceInputName :: Set FlakeId -> FlakeId
sourceInputName taken = freshInputName taken $ toFlakeId $ InputName "upstream"

-- | Every root-level name that the generated flake binds for upstream. The set
-- holds the name of each node, and the alias names that upstream also used.
takenInputNames :: FlakeLock -> Map NodeId FlakeId -> Set FlakeId
takenInputNames lock names = rootBoundNames lock <> Set.fromList (toList names)

-- | The name that each root-level name of upstream becomes, whether the node
-- keeps that name or an alias claims it.
rootBoundNames :: FlakeLock -> Set FlakeId
rootBoundNames lock = Set.fromList $ toFlakeId <$> Map.keys (rootEdges lock)
