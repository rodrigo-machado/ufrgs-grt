module Graph.Match
	(
	Mapping,
	findMatches,
	findMatchesR,
	isSurjective,
	isInjective,
	findIsoMorphisms,
    isIsomorphic
	)
	where

import Control.Monad -- foldM
import Data.Maybe
import Graph.Digraph
import qualified Data.IntMap as IM
import qualified Data.List as L

-- | Is a tuple of two relations regarding two graphs (possibly equal):
-- the first among their respective nodes, the other among their edges. Each
-- relation is described as a list of (Int, Int) tuples.
type Mapping = ([(Int, Int)], [(Int, Int)])

type Rule a b = Morphism a b
emptyRule = Morphism [] []

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Mapping]
findMatches l g = 
	let matches = matchEdges l g 
	in matches >>= \m -> (matchNodes emptyRule l g m)

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatchesR :: Rule a b -> TypedDigraph a b -> TypedDigraph a b -> [Mapping]
findMatchesR r l g = 
	let matches = matchEdges l g 
	in matches >>= \m -> (matchNodes r l g m)

-- Condition Generators
-- This functions are meant to be called from another one that has access to a 
-- global picture from the graph morphism structures (Rule, L graph, G Graph,
-- node from L being mapped etc.). They generate NodeConditions to be applied
-- to nodes from the G graph.

-- An EdgeCondition checks if a node satisfies it's internal requirements.
type EdgeCondition b = Edge b -> Bool

-- | Generate an edge condition that checks if both edges are from same type.
edgeTypeCondGen :: Edge b -> EdgeCondition b
edgeTypeCondGen le = (\ge -> edgeType le == edgeType ge)

-- | Generate an edge condition that checks if @le@ and @ge@ have source nodes 
-- from same type.
srcTypeCondGen :: TypedDigraph a b -> Edge b -> TypedDigraph a b -> EdgeCondition b
srcTypeCondGen l le g =
	(\ge -> srcType le l == srcType ge g)

-- | Generate an edge condition that checks if @le@ and @ge@ have target nodes 
-- from same type.
tarTypeCondGen :: TypedDigraph a b -> Edge b -> TypedDigraph a b -> EdgeCondition b
tarTypeCondGen l le g =
	(\ge -> tarType le l == tarType ge g)

-- | Generate a condition that tests if @le@'s source already occurs in @m@.
-- If that's the case, check if @ge@'s source is the same node to which @le@'s
-- source got mapped.  If so, @ge@ is a matching Edge. If @le@'s source doesn't
-- occur in @m@, any @ge@ will satisfy this condition.
srcIDCondGen
	:: TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Mapping
	-> EdgeCondition b
srcIDCondGen l le g m@(nmatches, _) =
	(\ge ->
		let
			lsrc = sourceID le
			gsrc = sourceID ge
			matched = (\(s, t) -> s == lsrc) `L.find` nmatches
		in case matched of	
			Just (_, n) -> gsrc == n
			otherwise -> True)

-- | Generate an edge condition that tests if @le@'s target already occurs in
-- @m@. If that's the case, check if @ge@'s target is the same node to which
-- @le@'s target got mapped.  If so, @ge@ is a matching Edge. If @le@'s target
-- doesn't occur in @m@, any @ge@ will satisfy this condition.
tarIDCondGen
	:: TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Mapping
	-> EdgeCondition b
tarIDCondGen l le g m@(nmatches, _) =
	(\ge -> 
		let ltar = targetID le
		    gtar = targetID ge
		    matched = (\(s, t) -> s == ltar) `L.find` nmatches
		in case matched of	
			Just (_, n) -> gtar == n
			otherwise -> True)

-- | Generate an edge condition that checks If @le@ is a loop edge, check if
-- @ge@ is also a loop in @g@.

-- This condition is due to the sequential nature of processEdges. Conditions 
-- that check node coincidence (like @srcIDCond@) rely on previously mappings,
-- so they aren't able to detect a mapping node in the current step. 
-- Without @loopCond@, a loop edge that, e.g., happens to be the first to be
-- mapped passes srcIDCond and tarIDCond.
loopCondGen :: TypedDigraph a b -> Edge b -> TypedDigraph a b -> EdgeCondition b
loopCondGen l le g =
	(\ge ->
		let
			lsrc = sourceID le
			ltar = targetID le
			gsrc = sourceID ge
			gtar = targetID ge
		in if lsrc == ltar
			then gsrc == gtar
			else True)

-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateEdgeConds
	:: TypedDigraph a b
	-> Edge b 
	-> TypedDigraph a b
	-> Mapping
	-> [EdgeCondition b]
generateEdgeConds l le g m =
	edgeTypeCondGen le	:
	srcTypeCondGen l le g :
	tarTypeCondGen l le g :
	srcIDCondGen l le g m :
	tarIDCondGen l le g m :
	loopCondGen l le g :
	[]


-- | If all conditions in @cl@ get satisfied by the given edge @ge@, return the
-- @m@ mapping with the new edge/nodes added. 
processEdge :: [EdgeCondition b] -> Edge b -> Bool
processEdge cl e =
	L.foldr (\c acc -> (c e) && acc) True cl 

-- | Given a list of edges from @l@ to be matched and a specific mapping @m@, 
-- return a list of all possible mappings between these edges and those
-- from graph @g@, taking @m@ as initial mapping.
mapEdges
	:: TypedDigraph a b	-- ^ @l@, the "left side" graph
	-> [Edge b]			-- ^ list of edges to be mapped
	-> TypedDigraph a b	-- ^ @g@, the "right side" graph
	-> Mapping			-- ^ @m@, what already got mapped
	-> [Mapping]
mapEdges l (le:les) g@(TypedDigraph dg _) m@(nmatch, ematch) =
	let
		conds = generateEdgeConds l le g m
		candidates = filter (processEdge conds) $ edges dg
		newMappings = fmap
			(\ge ->
				((sourceID le, sourceID ge) :
				 (targetID le, targetID ge) :
				 nmatch,
				 (edgeID le, edgeID ge) : ematch))
			candidates
	in mapEdgesAux l les g newMappings

-- | Given a list of edges from graph @l@ and a list of partial mappings @ml@, 
-- return a list of all possible mappings between these edges and those
-- from graph @g@, taking each mapping from @ml@ as initial mapping.
mapEdgesAux
	:: TypedDigraph a b	-- ^ @l@, the "left side" graph
	-> [Edge b]			-- ^ list of edges to be mapped
	-> TypedDigraph a b	-- ^ @g@, the "right side" graph
	-> [Mapping]		-- ^ @ml@, all mappings created so far
	-> [Mapping]
mapEdgesAux l@(TypedDigraph d _) les g ml =
	case les of
		[] -> ml
		otherwise -> ml >>= \m -> mapEdges l les g m

-- | Given two typed graph's, return a list of all possible mappings
-- considering only the subgraph inducted by the edges.
matchEdges :: TypedDigraph a b -> TypedDigraph a b -> [Mapping]
matchEdges l@(TypedDigraph dg _) g =
	mapEdgesAux l (edges dg) g [([], [])]


-------------------------------------------------------------------------
-- Matching from nodes

-- A NodeCondition checks if a node satisfies it's internal requirements.
type NodeCondition a = Node a -> Bool

-- Condition Generators
-- This functions are meant to be called from another one that has access to a 
-- global picture from the graph morphism structures (Rule, L graph, G Graph,
-- node from L being mapped etc.). They generate NodeConditions to be applied
-- to nodes from the G graph.

-- | Generate a node condition that checks if both nodes are from same type.
nodeTypeCondGen :: Node a -> NodeCondition a
nodeTypeCondGen ln =
	(\n -> nodeType ln == nodeType n)

-- | If @ln@ is marked to be deleted, create a NodeCondition that checks if
-- @gn@ has an edge pointed from/at it.
danglingCondGen ::
	Rule a b
	-> TypedDigraph a b
	-> Node a 
	-> NodeCondition a
danglingCondGen r g ln =
	if toBeDeleted r (nodeID ln)
		then (\gn -> not $ hasEdge g gn)
		else (\gn -> True)

-- | Generate a node condition that first checks if @gn@ was already mapped. If
-- so, let it pass. Otherwise, check if both @ln@ and @gn@ are to be deleted. 
-- If so, they can be mapped to each other.
delCondGen ::
	Rule a b
	-> Node a 
	-> Mapping
	-> NodeCondition a
delCondGen r ln m =
	(\gn ->	(not $ isMapped gn m) ||
			(toBeDeleted r (nodeID ln) == mappedToDel r m gn))
	

-- | Check if @gn@ is a right-side node in the given mapping.
isMapped :: Node a -> Mapping -> Bool
isMapped gn ([], _) = False
isMapped gn (nmaps, _) =
	let	found = L.find (\(_, gnode) -> gnode == nodeID gn) nmaps
	in case found of
		Just _ -> True
		otherwise -> False
		
		
-- | Check if @n@ was mapped to a L node marked to be deleted.
mappedToDel :: Rule a b -> Mapping -> Node a -> Bool
mappedToDel r (nmaps, _) n =
	case nmaps of
	[] -> False
	otherwise ->
		let nmap = L.find (\(_, gnid) -> gnid == nid) nmaps
		in case nmap of
			Just (lnid, _) -> toBeDeleted r lnid
			otherwise -> False
	where
		nid = nodeID n

-- | Check if the L node with id @nid@ is marked to be deleted.
toBeDeleted :: Rule a b -> Int -> Bool
toBeDeleted r@(Morphism nal _) nid =
	let naction = L.find (\na -> 
		case na of
			(Just ln, _) -> nodeID ln == nid
			otherwise	 -> False)
		nal
	in case naction of
		Just (_, Nothing)	-> True
		otherwise			-> False

		
-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateConds ::
	Rule a b
	-> TypedDigraph a b
	-> Node a 
	-> TypedDigraph a b
	-> Mapping
	-> [NodeCondition a]
generateConds r l ln g m =
	nodeTypeCondGen ln	:
	delCondGen r ln	m	:
	danglingCondGen	r g ln :
	[]

-- | Apply each condition from @nodecl@ to the node @n@. Return True if all
-- conditions got satisfied.
processNode :: [NodeCondition a] -> Node a -> Bool
processNode cl n =
	L.foldr (\c acc -> (c n) && acc) True cl
	
		
-- | Given a list of nodes from @l@ to be matched and a specific mapping @m@, 
-- return a list of all possible mappings between these nodes and those
-- from graph @g@, taking @m@ as initial mapping.

-- mapNodes finds the matching @gn@ nodes to each @ln@ by filtering them with
-- a list of conditions generated for each given @ln@.
mapNodes ::
	Rule a b
	-> TypedDigraph a b
	-> [Node a]
	-> TypedDigraph a b
	-> Mapping
	-> [Mapping]
mapNodes _ _ [] _ m = [m]
mapNodes r l (ln:lns) g@(TypedDigraph dg _) m@(nmatch, ematch) =
	let
		conds = (generateConds r l ln g m)
		candidates = filter (processNode conds) $ nodes dg
		newMappings = fmap
			(\gn ->
				((nodeID ln, nodeID gn) : nmatch, ematch))
			candidates
	in mapNodesAux r l lns g newMappings

-- | Given a list of nodes from graph @l@ and a list of partial mappings @ml@, 
-- return a list of all possible mappings between these nodes and those
-- from graph @g@, taking each mapping from @ml@ as initial mapping.
mapNodesAux ::
	Rule a b
	-> TypedDigraph a b
	-> [Node a]
	-> TypedDigraph a b
	-> [Mapping]
	-> [Mapping]
mapNodesAux r l lns g ml =
	case lns of
		[] -> ml
		otherwise -> ml >>= \m -> mapNodes r l lns g m

-- | Meant to be called after a matchEdges process, takes the list of nodes
-- from L that didn't get mapped and return all possible mappings between them
-- and the nodes from G, each taking @m@ as starting point.
matchNodes ::
	Rule a b
	-> TypedDigraph a b
	-> TypedDigraph a b
	-> Mapping
	-> [Mapping]
matchNodes r l@(TypedDigraph dl _) g m@(nmatches, _) =
	let 
	    lnl = nodes dl							-- all nodes from @l@
	    mlnl = foldr (\(ln, _) acc ->			-- all "left side" nodes mapped
			let node = findNode ln dl in
				case node of
				(Just n) -> n : acc
				otherwise -> acc) [] nmatches 
	    rlnl = lnl L.\\ mlnl					-- list of remaining nodes
	in case rlnl of
		[] -> [m]
		(x:xs) -> mapNodes r l (x:xs) g m

------------------------------------------------------------------------
-- Isomorphism related (helper) functions

-- | Check if mapping @m@ is surjective. 

-- Currently, @isSurjective@ relies on @L.nub@ to get the set of nodes and edges
-- mapped in @m@. The number of elements in this set is compared to those from
-- graph @g@ to see if all got mapped.
-- TODO: use Data.Set for efficiency reasons.
isSurjective :: TypedDigraph a b -> Mapping -> Bool
isSurjective (TypedDigraph (Digraph gnm gem) _) m@(nm, em) =
	let mNodeList =	L.nub $ foldr (\(_, n) acc -> n:acc) [] nm
	    mEdgeList = L.nub $ foldr (\(_, e) acc -> e:acc) [] em
	in
		if IM.size gnm == L.length mNodeList &&
		   IM.size gem == L.length mEdgeList
		then True
		else False

-- | Check if a mapping is injective.

-- If the mapping is empty, it's by  definition injective. Otherwise, test, by
-- calling the helper function @iter@ over all node and edge mappings, if they
-- represent an injective relation.  @iter@ does so by checking if any "right
-- side" node/edge got mapped twice, with help of @mem@ that "remembers" the
-- node/edges scanned so far.
isInjective :: Mapping -> Bool
isInjective ([], []) = True
isInjective (nms, ems) =
	iter nms [] &&
	iter ems []
	where
		iter xs mem =
			case xs of
			[] -> True
			((_, t):xs) -> if t `L.elem` mem
					  then False
					  else iter xs (t:mem)	

-- | List all isomorphisms (represented as mappings) between both graphs.
findIsoMorphisms :: TypedDigraph a b -> TypedDigraph a b -> [Mapping]
findIsoMorphisms l@(TypedDigraph (Digraph lnm lem) _) g@(TypedDigraph (Digraph gnm gem) _) =
	if IM.size lnm /= IM.size gnm ||
	   IM.size lem /= IM.size gem
	then []
	else filter isInjective $
		 	filter (isSurjective g) $
		 		findMatches l g

-- | Check if there's an isomorphism between two graphs.
isIsomorphic :: TypedDigraph a b -> TypedDigraph a b -> Bool
isIsomorphic a b = findIsoMorphisms a b /= []


