-- | Flattening upstream's lock graph into the inputs of the thunk's own flake.
--
-- The heart of the package. A consumer writing
--
-- > inputs.someinput.follows = "mythunk/nixpkgs";       # read
-- > inputs.mythunk.inputs.nixpkgs.follows = "nixpkgs";  # override
--
-- is resolving against the lock graph, so the thunk's flake has to reproduce
-- upstream's graph as its own: a root-level input per node of upstream's lock,
-- each pinned to the revision upstream locked, with every edge restated as a
-- @follows@. Reading works because the names are there; overriding works
-- because each one is a real node rather than an alias.
--
-- Pure throughout. Fetching upstream and running @nix flake lock@ afterwards
-- is left to "Nix.Thunk.Internal".
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

-- | The generated flake as a value: everything the renderers need, and nothing
-- about how any of it is spelled.
--
-- Rendered, the lock in 'FlakeLock' comes out as the following, with each part
-- labelled by the field it came from.
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
  -- ^ What to call the repository the thunk points at. Not a constant, since
  -- one of upstream's own inputs may already be called that.
  , flattenedFlake_sourceRefs :: NodeRefs
  , flattenedFlake_sourceEdges :: Map InputName FlakeId
  -- ^ Upstream's own inputs, redirected onto ours. The asymmetry of the two
  -- types is the whole of it: the key is upstream's name, which an override
  -- has to match exactly, and the value is ours, which a @follows@ has to be
  -- able to say.
  , flattenedFlake_inputs :: Map FlakeId FlattenedInput
  -- ^ Keyed by the name a consumer will write, which is the point of the map:
  -- upstream's lock is keyed by node id, and the two are not the same.
  , flattenedFlake_complete :: Bool
  -- ^ Whether every node of upstream's lock became an input with a reference
  -- of its own and every edge was reproduced. Only then does this flake say
  -- everything about its own inputs, and only then can
  -- 'Nix.Thunk.Flake.Lockfile.renderFlakeLock' describe it without asking Nix.
  -- Nothing renders this: it answers a different question, asked by
  -- "Nix.Thunk.Internal".
  }
  deriving stock (Eq, Show)

-- | One root-level input of the generated flake.
--
-- The 'Either' in the first field is the fork the rest of this package keeps
-- arriving at, and it is visible in the rendered output: @sub@ above has a
-- @rev@ and @subAlias@ has a @follows@.
data FlattenedInput = FlattenedInput
  { flattenedInput_ref :: Either (FollowsPath FlakeId) NodeRefs
  -- ^ 'Right' is a real node, which a consumer can read /and/ override.
  -- 'Left' is used when the node cannot be expressed as a fetchable
  -- reference, and all that can be offered is another name for something:
  -- readable, but not overridable.
  , flattenedInput_isFlake :: Bool
  , flattenedInput_edges :: Map InputName FlakeId
  -- ^ As 'flattenedFlake_sourceEdges', for a node rather than for upstream.
  }
  deriving stock (Eq, Show)

-- | What the per-node translation needs to consult. The references are tied
-- back to the context that produced them, since a relative path node has to be
-- resolved against a parent whose own reference may itself have been rewritten.
data FlattenContext = FlattenContext
  { flattenContext_sourceName :: FlakeId
  , flattenContext_sourceRefs :: NodeRefs
  , flattenContext_lock :: FlakeLock
  , flattenContext_origins :: Map NodeId NodeOrigin
  , flattenContext_refs :: Map NodeId NodeRefs
  -- ^ Only the nodes that got a reference of their own. Absence is the answer
  -- to 'nodeHasRef', and what sends 'flattenedInput' looking for an alias.
  , flattenContext_names :: Map NodeId FlakeId
  }

--------------------------------------------------------------------------------
-- Flattening
--------------------------------------------------------------------------------

-- | Flatten upstream's lock graph into the inputs of the thunk's own flake.
--
-- A function of its two arguments and nothing else, which is what makes the
-- translation testable: hand it a lock, read the result.
--
-- 'flattenedFlake_complete' walks every node of the lock rather than
-- 'nonRootNodes', because the root's edges are 'flattenedFlake_sourceEdges'
-- and they can be dropped like any others.
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
    , flattenedFlake_inputs = flattenedInputs ctx
    , flattenedFlake_complete = all (edgesFullyDeclared ctx) $ Map.keys lock.flakeLock_nodes
    }
  where
    ctx = flattenContext srcRefs lock

-- | Every root-level input the generated flake binds for upstream.
--
-- Reads inside out. 'nonRootNodes' is every node but the root, keyed by node
-- id; 'flattenedInput' decides what to do with each one; 'inputNameOf' rekeys
-- the lot to the names consumers write. The union with 'rootAliases' is
-- left-biased, so a node always beats an alias of the same name.
-- 'nodeInputNames' reserves the alias names, so that should never come up,
-- but it costs nothing to make the safe case the default.
flattenedInputs :: FlattenContext -> Map FlakeId FlattenedInput
flattenedInputs ctx =
  Map.union
    ( Map.mapKeys (inputNameOf ctx) $
        Map.mapMaybeWithKey (flattenedInput ctx) $
          nonRootNodes ctx.flattenContext_lock
    )
    (aliasInput <$> rootAliases ctx)

-- | Whether the generated flake declares every input a node of upstream's lock
-- declares.
--
-- 'overridableEdges' drops an edge it cannot restate: one pointing at a node
-- with no reference of its own, one whose @follows@ walks off the end of the
-- graph, and one whose name Nix will not accept in an override. Whatever is
-- dropped, Nix has to settle from upstream's own lock, and a lock that has to
-- say what upstream's inputs are is not one this package can write.
edgesFullyDeclared :: FlattenContext -> NodeId -> Bool
edgesFullyDeclared ctx nodeId =
  Map.size (overridableEdges ctx $ nodeEdgesOf lock nodeId) == Map.size (edgesOf lock nodeId)
  where
    lock = ctx.flattenContext_lock

-- | A node given no reference of its own is one 'overridableEdges' leaves the
-- edges to, so upstream's lock, not ours, is what says where it comes from.
-- The aliases 'rootAliases' adds are not these: they are extra names for a
-- node that does have one.
nodeHasRef :: FlattenContext -> NodeId -> Bool
nodeHasRef ctx nodeId = Map.member nodeId ctx.flattenContext_refs

-- | The names upstream bound at its root that are not the name their node is
-- exposed under, mapped to that name. Several of upstream's root inputs can
-- resolve to one node, since a root-level @follows@ is just another name for
-- what it points at, and only one of them can be the name the node itself is
-- given. The rest are emitted as aliases, so that every name upstream used
-- still resolves through the thunk.
--
-- Except the ones Nix cannot refer to at all: an alias exists to be followed,
-- and a name that cannot appear in a @follows@ would only be dead weight.
-- 'flakeId' failing is what says so, and is why the alias name is a 'FlakeId'
-- rather than something merely checked on the way past.
--
-- Emitting nothing for the losing names, which is what this replaced, meant
-- @mythunk\/nixpkgs-unstable@ did not resolve even though upstream calls its
-- input that. The condition below is the leftovers: the name that won is the
-- one excluded, and everything else reaching the same node is an alias.
rootAliases :: FlattenContext -> Map FlakeId FlakeId
rootAliases ctx =
  Map.fromList
    [ (aliasName, exposed)
    | (edgeName, target) <- Map.toList $ rootEdges ctx.flattenContext_lock
    , let exposed = inputNameOf ctx target
    , Just aliasName <- [flakeId edgeName]
    , aliasName /= exposed
    ]

-- | An input that is nothing but a second name for a sibling input.
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
-- Two kinds are left alone for upstream's own lock to resolve. An aliased node
-- is bound with a @follows@ of its own, so overriding the edge that reaches it
-- would make the alias follow itself, which Nix rejects as a follow cycle. And
-- an edge whose name is not a flake identifier cannot be overridden at all,
-- since the override is written as an attribute path.
--
-- Note the key stays an 'InputName'. It has to be the name upstream declared,
-- or the override names nothing, so an unusable one leaves rather than being
-- repaired: this is the half of the distinction a type cannot carry.
overridableEdges :: FlattenContext -> Map InputName NodeId -> Map InputName FlakeId
overridableEdges ctx = fmap (inputNameOf ctx) . Map.filterWithKey overridable
  where
    overridable edgeName target = isFlakeId edgeName && nodeHasRef ctx target

-- | Ties the knot between the context and the references it computes.
--
-- @flattenContext_refs@ is built by 'nodeRef', which consults
-- @flattenContext_refs@: a relative path node needs the reference of whatever
-- declared it, which may itself have been rewritten. Laziness allows it and
-- 'parentRefsOf' says why it terminates.
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

-- | What to do with one node of upstream's lock: which reference to bind it
-- to if it can have one, an alias if it cannot, and which of its own edges to
-- redirect.
--
-- 'Nothing' when neither is available, and the node goes unmentioned. Its
-- edges are dropped either way, so this costs a readable name and not a
-- correct answer.
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

-- | The references the thunk should bind a single node to, when it can be
-- given any: 'Nothing' is what puts a node in 'aliasPath'\'s hands.
nodeRef :: FlattenContext -> NodeId -> FlakeNode -> Maybe NodeRefs
nodeRef ctx nodeId node = case relativePathOf =<< node.flakeNode_original of
  Nothing -> fetchableNodeRefs =<< node.flakeNode_locked
  Just rel -> do
    parent <- parentRefsOf ctx nodeId
    withRelativeDirRefs parent rel

-- | The references of whatever declared this node, which is what a relative
-- path it was declared with is relative to.
--
-- The @declaredIn@ being a 'Maybe' inside a 'Maybe' block is the whole of the
-- @follows@ fix: a node with no known declaration site fails the lookup and
-- gets aliased, rather than being resolved against a guess. See
-- 'discoverOrigins'.
--
-- The second branch reads the map it is being used to build, and terminates
-- because a declaration site is always strictly nearer the root than the node
-- it declared, 'discoverOrigins' being breadth-first. So the chain always ends
-- at 'DeclarationSite_Source', and nested relative paths resolve by walking it.
parentRefsOf :: FlattenContext -> NodeId -> Maybe NodeRefs
parentRefsOf ctx nodeId = do
  origin <- Map.lookup nodeId ctx.flattenContext_origins
  site <- origin.nodeOrigin_declaredIn
  case site of
    DeclarationSite_Source -> Just ctx.flattenContext_sourceRefs
    DeclarationSite_Node parentId -> Map.lookup parentId ctx.flattenContext_refs

-- | Where a node sits relative to the thunk's source, for nodes that cannot be
-- given a reference of their own.
--
-- 'Nothing' when any step of the way is a name Nix will not walk, since the
-- path has to name upstream's inputs as upstream named them and there is no
-- sanitising a name that has to match. The node then cannot be offered under
-- any name at all. This is the residue of the same problem 'flakeId' exists
-- for, in the one place it is a whole path rather than a single name.
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
nodeInputNames :: FlakeLock -> Map NodeId FlakeId
nodeInputNames lock = nameNodes reserved (rootInputNames lock) $ preferNodeId <$> Map.keys (nonRootNodes lock)
  where
    -- Every name upstream bound at its root is reserved, including the ones
    -- 'rootInputNames' had to drop: 'rootAliases' will claim those, and a
    -- transitive node must not be given a name an alias is about to take.
    reserved = rootBoundNames lock
    preferNodeId nodeId = (nodeId, nodeIdToInputName nodeId)

-- | The name each node upstream binds at its root is exposed under. A node
-- bound more than once, which a root-level @follows@ does, keeps the first of
-- those names.
--
-- Nothing is reserved here, unlike in 'nodeInputNames': the names these nodes
-- are competing for are the very ones being handed out.
rootInputNames :: FlakeLock -> Map NodeId FlakeId
rootInputNames lock = nameNodes Set.empty Map.empty $ preferEdgeName <$> Map.toList (rootEdges lock)
  where
    preferEdgeName (edgeName, target) = (target, edgeName)

-- | Give each node the first name offered for it.
--
-- A node named already keeps the name it has, which is what makes "first"
-- mean anything: the callers offer names in ascending order, so it is the
-- alphabetically first. Arbitrary, but the answer ends up in a committed file,
-- so it has to be the same answer every time.
--
-- What is left of a name after 'toFlakeId' can collide with one already given
-- out, and 'freshInputName' suffixes the loser. The set of names already taken
-- is recomputed each step, which is quadratic and does not matter: locks have
-- tens of nodes.
nameNodes
  :: Set FlakeId
  -- ^ Names spoken for by something other than a node
  -> Map NodeId FlakeId
  -- ^ Names given out already
  -> [(NodeId, InputName)]
  -- ^ Each node, and the name it would like, as upstream spelled it
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

-- | The name the thunk's own source is bound to. Upstream's inputs occupy
-- root-level names too, so on the off chance one of them is already called
-- @upstream@, pick another name rather than colliding.
sourceInputName :: Set FlakeId -> FlakeId
sourceInputName taken = freshInputName taken $ toFlakeId $ InputName "upstream"

-- | Every root-level name the generated flake binds for upstream: the name
-- each node is exposed under, plus the alias names upstream also used.
takenInputNames :: FlakeLock -> Map NodeId FlakeId -> Set FlakeId
takenInputNames lock names = rootBoundNames lock <> Set.fromList (toList names)

-- | What each name upstream bound at its root comes out as, whether the node
-- keeps it or an alias claims it. One set consulted from both sides:
-- 'nodeInputNames' holds these back from the transitive nodes, and
-- 'takenInputNames' counts them against the source's own name.
rootBoundNames :: FlakeLock -> Set FlakeId
rootBoundNames lock = Set.fromList $ toFlakeId <$> Map.keys (rootEdges lock)
