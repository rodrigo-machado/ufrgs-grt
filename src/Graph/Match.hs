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

	
-- | Insert a node mapping into the given one. If it's already there, do nothing.
insNodeMapping :: (Int, Int) -> Mapping -> Mapping
insNodeMapping newm m@(ns, es) =
	if newm `L.elem` ns
	then m
	else (newm:ns, es)

-- | Insert an edge mapping into the given one. If it's already there, do nothing.
insEdgeMapping :: (Int, Int) -> Mapping -> Mapping
insEdgeMapping newm m@(ns, es) =
	if newm `L.elem` es
	then m
	else (ns, newm:es)

-- | A Condition consists of a function that, given two typed graphs @l@ and
-- @g@, two edges @le@ ad @ge@ and a mapping between edges from both graphs,
-- checks if @ge@ satisfies this conditions internal criteria as a matching
-- edge.
type Condition a b =
	TypedDigraph a b	-- ^ @l@, the "left side" graph
	-> Edge b			-- ^ @le@, an edge from @l@
	-> TypedDigraph a b	-- ^ @g@, the "right side" graph
	-> Edge b			-- ^ @ge@, an edge from @g@
	-> Mapping			-- ^ @m@, what already got mapped
	-> Bool

-- | Check if both edges are from the same type.
edgeTypeCond :: Condition a b
edgeTypeCond l le g ge m =
	edgeType le == edgeType ge

-- | Check if @le@ and @ge@ have source nodes from same type.
srcTypeCond :: Condition a b
srcTypeCond l le g ge m =
	ltype == gtype
	where
		ltype = srcType le l
		gtype = srcType ge g

-- | Check if @le@ and @ge@ have target nodes from same type.
tarTypeCond :: Condition a b
tarTypeCond l le g ge m =
	ltype == gtype
	where
		ltype = tarType le l
		gtype = tarType ge g

-- | Test if @le@'s source already occurs in @m@. If that's the case, check
-- if @ge@'s source is the same node to which @le@'s source got mapped.  If so,
-- @ge@ is a matching Edge. If @le@'s source doesn't occur in @m@, any @ge@
-- will satisfy this condition.
srcIDCond :: Condition a b
srcIDCond l le g ge m@(nmatches, _) =
	let lsrc = sourceID le
	    gsrc = sourceID ge
	    matched = (\(s, t) -> s == lsrc) `L.find` nmatches
	in case matched of	
		Just (_, n) -> gsrc == n
		otherwise -> True

-- | Test if @le@'s target already occurs in @m@. If that's the case, check
-- if @ge@'s target is the same node to which @le@'s target got mapped.  If so,
-- @ge@ is a matching Edge. If @le@'s target doesn't occur in @m@, any @ge@
-- will satisfy this condition.
tarIDCond :: Condition a b
tarIDCond l le g ge m@(nmatches, _) =
	let ltar = targetID le
	    gtar = targetID ge
	    matched = (\(s, t) -> s == ltar) `L.find` nmatches
	in case matched of	
		Just (_, n) -> gtar == n
		otherwise -> True

-- | If @le@ is a loop edge, check if @ge@ is also a loop in @g@.

-- This condition is due to the sequential nature of processEdges. Conditions 
-- that check node coincidence (like @srcIDCond@) rely on previously mappings,
-- so they aren't able to detect a mapping node in the current step. 
-- Without @loopCond@, a loop edge that, e.g., happens to be the first to be
-- mapped passes srcIDCond and tarIDCond.
loopCond :: Condition a b
loopCond l le g ge m =
	let lsrc = sourceID le
	    ltar = targetID le
	    gsrc = sourceID ge
	    gtar = targetID ge
	in if lsrc == ltar
		then gsrc == gtar
		else True

conditionList = [edgeTypeCond, srcTypeCond, tarTypeCond, srcIDCond, tarIDCond,
	loopCond]

-- | If all conditions in @cl@ get satisfied by the given edge @ge@, return the
-- @m@ mapping with the new edge/nodes added. 
processEdges
	:: [Condition a b]
	-> TypedDigraph a b	-- ^ @l@, the "left side" graph
	-> Edge b			-- ^ @le@, an edge from @l@
	-> TypedDigraph a b	-- ^ @g@, the "right side" graph
	-> Edge b			-- ^ @ge@, an edge from @g@
	-> Mapping			-- ^ @m@, what already got mapped
	-> Maybe Mapping
processEdges cl l@(TypedDigraph ld _) le g@(TypedDigraph gd _) ge m =
	if foldr (\c acc -> (c l le g ge m) && acc) True cl 
	then Just $
		insNodeMapping (targetID le, targetID ge) $ -- adds node Mappings
		insNodeMapping (sourceID le, sourceID ge) $
		insEdgeMapping (edgeID le, edgeID ge)	  $ -- adds edge Mapping
		m
	else Nothing
		

-- | Given a list of edges from @l@ to be matched and a specific mapping @m@, 
-- return a list of all possible mappings between these edges and those
-- from graph @g@, taking @m@ as initial mapping.
applyCond
	:: [Edge b]			-- ^ list of edges to be mapped
	-> TypedDigraph a b	-- ^ @l@, the "left side" graph
	-> TypedDigraph a b	-- ^ @g@, the "right side" graph
	-> Mapping			-- ^ @m@, what already got mapped
	-> [Mapping]
applyCond (le:les) l g@(TypedDigraph dg _) m =
	let newMappings = mapMaybe 
		(\ge -> processEdges conditionList l le g ge m) $ edges dg
	in applyCondMult 
		les
		l
		g
		newMappings

-- | Given a list of edges from graph @l@ and a list of partial mappings @ml@, 
-- return a list of all possible mappings between these edges and those
-- from graph @g@, taking each mapping from @ml@ as initial mapping.
applyCondMult
	:: [Edge b]			-- ^ list of edges to be mapped
	-> TypedDigraph a b	-- ^ @l@, the "left side" graph
	-> TypedDigraph a b	-- ^ @g@, the "right side" graph
	-> [Mapping]		-- ^ @ml@, all mappings created so far
	-> [Mapping]
applyCondMult les l@(TypedDigraph d _) g ml =
	case les of
		[] -> ml
		otherwise -> ml >>= \m -> applyCond les l g m

-- | Given two typed graph's, return a list of all possible mappings
-- considering only the subgraph inducted by the edges.
matchEdges :: TypedDigraph a b -> TypedDigraph a b -> [Mapping]
matchEdges l@(TypedDigraph dg _) g =
	applyCondMult (edges dg) l g [([], [])]


-------------------------------------------------------------------------
-- Mapping from nodes

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

----------------------------------------------------------------------------
-- Matching functions that process rules


-- Trying here a new approach: conditions are generated dynamically and have
-- a much simpler signature. Previously we had fixed conditions that processed
-- the edges/nodes and, together with a whole picture from the graph structure,
-- were able to tell if they satisfied it. Now, this role is played by a
-- condition generator, a function that, based on whatever it needs from both
-- graphs and a original node from L graph, returns a function that consumes a 
-- node and returns a boolean value

newtype NodeCondition a =
	NodeCondition { condApply :: Node a -> Maybe [NodeCondition a] }

identCond :: NodeCondition a
identCond =	NodeCondition (\_ -> Just [])

nodeTypeCondGen :: Node a -> NodeCondition a
nodeTypeCondGen ln =
	NodeCondition
	(\n ->
		if nodeType ln == nodeType n
			then Just []
			else Nothing)

danglingCondGen ::
	Rule a b
	-> TypedDigraph a b
	-> Node a 
	-> NodeCondition a
danglingCondGen r g ln =
	if toBeDeleted r ln
		then NodeCondition
			(\gn -> if hasEdge g gn
				then Nothing
				else Just [])
		else identCond

delCondGen ::
	Rule a b
	-> Node a 
	-> NodeCondition a
delCondGen r ln =
	if toBeDeleted r ln
		then NodeCondition
			(\gn ->	Just [NodeCondition
				(\n -> if gn /= n
					then Just []
					else Nothing)])
		else identCond

toBeDeleted :: Rule a b -> Node a -> Bool
toBeDeleted r@(Morphism nal _) n =
	let naction =
		L.find (\na -> 
			case na of
				(Just ln, _) -> ln == n
				otherwise	 -> False)
			nal
	in case naction of
		Just (_, Nothing)	-> True
		otherwise			-> False

		
generateConds ::
	Rule a b
	-> TypedDigraph a b
	-> Node a 
	-> TypedDigraph a b
	-> Mapping
	-> [NodeCondition a]
generateConds r l ln g m =
	nodeTypeCondGen ln	:
	delCondGen r ln		:
	danglingCondGen	r g ln :
	[]

-- | Apply each condition from @nodecl@ to the node @n@. If it satisfies all of
-- them, return a new set (possibly empty) of new conditions that arose from
-- the multiple condition applications. Return Nothing otherwise.
processNode :: [NodeCondition a] -> Node a -> Maybe (Node a, [NodeCondition a])
processNode nodecl n =
	iter nodecl [] 
	where
		iter [] outcl = Just (n, outcl)
		iter (c:cl) outcl =
			let newconds = (condApply c) n
			in newconds >>= (\nc -> iter cl (outcl ++ nc))

mapNodes ::
	Rule a b
	-> TypedDigraph a b
	-> [Node a]
	-> TypedDigraph a b
	-> (Mapping, [NodeCondition a])
	-> [(Mapping, [NodeCondition a])]
mapNodes _ _ [] _ m = [m]
mapNodes r l (ln:lns) g@(TypedDigraph dg _) m@((nmatch, ematch), cl) =
	let
		candidates = mapMaybe (processNode cl) $ nodes dg
		newMappings = fmap
			(\(gn, newcl) ->
				(((nodeID ln, nodeID gn) : nmatch, ematch),
				 newcl))
			candidates
	in mapNodesAux r l lns g newMappings

mapNodesAux ::
	Rule a b
	-> TypedDigraph a b
	-> [Node a]
	-> TypedDigraph a b
	-> [(Mapping, [NodeCondition a])]
	-> [(Mapping, [NodeCondition a])]
mapNodesAux r l lns g ml =
	case lns of
		[] -> ml
		otherwise -> ml >>= \m -> mapNodes r l lns g m

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
		(x:xs) ->
			let pairs = mapNodes r l (x:xs) g (m, generateConds r l x g m)
			in fmap (\(mapping, cl) -> mapping) pairs

