module Graph.Match
	(
	Mapping
	, findMatches
	, findMatchesR
	, isSurjective
	--, isInjective
	, findIsoMorphisms
    , isIsomorphic
	, MorphismType (..)
	)
	where

import Control.Monad -- foldM
import Data.Maybe
import qualified Graph.Digraph as D
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Set as S

-- | Is a tuple of two relations regarding two graphs (possibly equal):
-- the first among their respective nodes, the other among their edges. Each
-- relation is described as a list of (Int, Int) tuples.
type Mapping = ([(Int, Int)], [(Int, Int)])
type MapSet = (S.Set (Int, Int), S.Set (Int, Int))

type Rule a b = D.Morphism a b
emptyRule = D.Morphism [] []

data MorphismType = Normal | Mono | Epi | Iso 
	deriving (Eq)

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatches :: MorphismType -> D.TypedDigraph a b -> D.TypedDigraph a b -> [Mapping]
findMatches mt l g = 
	findMatchesR emptyRule mt l g

-- | Given two typed graphs, return a list of mappings, each representing a
-- possible homomorphism between the graphs.
findMatchesR :: Rule a b -> MorphismType -> D.TypedDigraph a b -> D.TypedDigraph a b -> [Mapping]
findMatchesR r mt l g = 
	let matches = matchGraphs r mt l g
	in map (\(nm, em) -> (S.toList nm, S.toList em)) matches

----------------------------------------------------------------------------
-- D.Edge related condition functions

-- An EdgeCondition checks if a node satisfies it's internal requirements.
type EdgeCondition b = D.Edge b -> Bool

-- | Generate an edge condition that checks if both edges are from same type.
edgeTypeCondGen :: D.Edge b -> EdgeCondition b
edgeTypeCondGen le = (\ge -> D.edgeType le == D.edgeType ge)

-- | Generate an edge condition that checks if @le@ and @ge@ have source nodes 
-- from same type.
srcTypeCondGen :: D.TypedDigraph a b -> D.Edge b -> D.TypedDigraph a b -> EdgeCondition b
srcTypeCondGen l le g =
	(\ge -> D.srcType le l == D.srcType ge g)

-- | Generate an edge condition that checks if @le@ and @ge@ have target nodes 
-- from same type.
tarTypeCondGen :: D.TypedDigraph a b -> D.Edge b -> D.TypedDigraph a b -> EdgeCondition b
tarTypeCondGen l le g =
	(\ge -> D.tarType le l == D.tarType ge g)

-- | Generate a condition that tests if @le@'s source already occurs in @m@.
-- If that's the case, check if @ge@'s source is the same node to which @le@'s
-- source got mapped.  If so, @ge@ is a matching D.Edge. If @le@'s source doesn't
-- occur in @m@, any @ge@ will satisfy this condition.
srcIDCondGen
	:: D.Edge b
	-> MapSet
	-> EdgeCondition b
srcIDCondGen le m@(nmatches, _) =
	(\ge ->
		let
			lsrc = D.sourceID le
			gsrc = D.sourceID ge
			matched = L.find (\(s, t) -> s == lsrc) $ S.toList nmatches
		in case matched of	
			Just (_, n) -> gsrc == n
			otherwise -> True)

{-
			matched = S.toList $ S.filter (\(s, t) -> s == lsrc) S.toList nmatches
		in case matched of
			((_, n):ns) -> gsrc == n
			otherwise   -> True)
-}

-- | Generate an edge condition that tests if @le@'s target already occurs in
-- @m@. If that's the case, check if @ge@'s target is the same node to which
-- @le@'s target got mapped.  If so, @ge@ is a matching D.Edge. If @le@'s target
-- doesn't occur in @m@, any @ge@ will satisfy this condition.
tarIDCondGen
	:: D.Edge b
	-> MapSet
	-> EdgeCondition b
tarIDCondGen le m@(nmatches, _) =
	(\ge -> 
		let ltar = D.targetID le
		    gtar = D.targetID ge
		    matched = L.find (\(s, t) -> s == ltar) $ S.toList nmatches
		in case matched of	
			Just (_, n) -> gtar == n
			otherwise -> True)
{-
		    matched = S.toList $ S.filter (\(s, t) -> s == ltar) nmatches
		in case matched of
			((_, n):ns) -> gtar == n
			otherwise   -> True)
-}

-- | Generate an edge condition that checks If @le@ is a loop edge, check if
-- @ge@ is also a loop in @g@.

-- This condition is due to the sequential nature of processEdges. Conditions 
-- that check node coincidence (like @srcIDCond@) rely on previously mappings,
-- so they aren't able to detect a mapping node in the current step. 
-- Without @loopCond@, a loop edge that, e.g., happens to be the first to be
-- mapped passes srcIDCond and tarIDCond.
loopCondGen :: D.TypedDigraph a b -> D.Edge b -> D.TypedDigraph a b -> EdgeCondition b
loopCondGen l le g =
	(\ge ->
		let
			lsrc = D.sourceID le
			ltar = D.targetID le
			gsrc = D.sourceID ge
			gtar = D.targetID ge
		in if lsrc == ltar
			then gsrc == gtar
			else True)

-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateEdgeConds
	:: D.TypedDigraph a b
	-> D.Edge b 
	-> D.TypedDigraph a b
	-> MapSet
	-> [EdgeCondition b]
generateEdgeConds l le g m =
	edgeTypeCondGen le	:
	srcTypeCondGen l le g :
	tarTypeCondGen l le g :
	srcIDCondGen le m :
	tarIDCondGen le m :
	loopCondGen l le g :
	[]


-- | If all conditions in @cl@ get satisfied by the given edge @ge@, return the
-- @m@ mapping with the new edge/nodes added. 
processEdge :: [EdgeCondition b] -> D.Edge b -> Bool
processEdge cl e =
	L.foldr (\c acc -> (c e) && acc) True cl 

-------------------------------------------------------------------------
-- D.Node related condition functions

-- A NodeCondition checks if a node satisfies it's internal requirements.
type NodeCondition a = D.Node a -> Bool

-- Condition Generators
-- This functions are meant to be called from another one that has access to a 
-- global picture from the graph morphism structures (Rule, L graph, G Graph,
-- node from L being mapped etc.). They generate NodeConditions to be applied
-- to nodes from the G graph.

-- | Generate a node condition that checks if both nodes are from same type.
nodeTypeCondGen :: D.Node a -> NodeCondition a
nodeTypeCondGen ln =
	(\n -> D.nodeType ln == D.nodeType n)

-- | If @ln@ is marked to be deleted, create a NodeCondition that checks if
-- @gn@ has an edge pointed from/at it.
danglingCondGen ::
	Rule a b
	-> D.TypedDigraph a b
	-> D.Node a 
	-> NodeCondition a
danglingCondGen r g ln =
	if toBeDeleted r (D.nodeID ln)
		then (\gn -> not $ D.hasEdge g gn)
		else (\gn -> True)

-- | Generate a node condition that first checks if @gn@ was already mapped. If
-- so, let it pass. Otherwise, check if both @ln@ and @gn@ are to be deleted. 
-- If so, they can be mapped to each other.
delCondGen ::
	Rule a b
	-> D.Node a 
	-> MapSet
	-> NodeCondition a
delCondGen r ln m =
	(\gn ->	(not $ isMapped gn m) ||
			(toBeDeleted r (D.nodeID ln) == mappedToDel r m gn))

-- | Check if @gn@ is a right-side node in the given mapping.
isMapped :: D.Node a -> MapSet -> Bool
isMapped gn (nmaps, _) =
	let	found = S.filter (\(_, gnode) -> gnode == D.nodeID gn) nmaps
	in not $ S.null found
		
-- | Check if @n@ was mapped to a L node marked to be deleted.
mappedToDel :: Rule a b -> MapSet -> D.Node a -> Bool
mappedToDel r (nmaps, _) n =
	let nmap = S.filter (\(lnid, gnid) ->
			gnid == nid && toBeDeleted r lnid)
			nmaps
		in not $ S.null nmap
	where
		nid = D.nodeID n

-- | Check if the L node with id @nid@ is marked to be deleted.
toBeDeleted :: Rule a b -> Int -> Bool
toBeDeleted r@(D.Morphism nal _) nid =
	let naction = L.find (\na -> 
		case na of
			(Just ln, _) -> D.nodeID ln == nid
			otherwise	 -> False)
		nal
	in case naction of
		Just (_, Nothing)	-> True
		otherwise			-> False

		
-- | Feed each generator it's parameters and collect the resulting
-- nodeconditions in a list.
generateConds ::
	Rule a b
	-> D.TypedDigraph a b
	-> D.Node a 
	-> D.TypedDigraph a b
	-> MapSet
	-> [NodeCondition a]
generateConds r l ln g m =
	nodeTypeCondGen ln	:
	delCondGen r ln	m	:
	danglingCondGen	r g ln :
	[]

-- | Apply each condition from @nodecl@ to the node @n@. Return True if all
-- conditions got satisfied.
processNode :: [NodeCondition a] -> D.Node a -> Bool
processNode cl n =
	L.foldr (\c acc -> (c n) && acc) True cl
	
		
----------------------------------------------------------------------------
-- The matching algorithm

-- | Given a list of edges from @l@ to be matched and a specific mapping @m@, 
-- return a list of all possible mappings between these edges and those
-- from graph @g@, taking @m@ as initial mapping.
mapGraphs
	:: Rule a b
	-> MorphismType
	-> D.TypedDigraph a b	-- ^ @l@, the "left side" graph
	-> (MapSet, D.TypedDigraph a b, [D.Edge b], [D.Node a]) -- ^ @m@, what already got mapped
	-> [(MapSet, D.TypedDigraph a b, [D.Edge b], [D.Node a])]
mapGraphs _ mt _ ml@((nmap, emap), D.TypedDigraph dg@(D.Digraph gnm gem) _, _, []) =
	case mt of
	Epi -> let
		gMappedNodes = S.fold (\(ln, gn) acc -> S.insert gn acc) S.empty nmap
		gMappedEdges = S.fold (\(le, ge) acc -> S.insert ge acc) S.empty emap
		in 
		if S.size gMappedNodes == IM.size gnm && S.size gMappedEdges == IM.size gem
			then [ml]
			else []
	Iso -> if D.nullG dg
			then [ml]
			else []
	otherwise -> [ml]
mapGraphs r mt l (m@(nmatch, ematch),
	g@(D.TypedDigraph dg@(D.Digraph gnm gem) tg),
	(le:les), lns) =
	let
		conds = generateEdgeConds l le g m
		edgeList = D.edges dg
		candidates = filter (processEdge conds) $ edgeList
		newMapSets = fmap
			(\ge ->
				let
					sid = D.sourceID ge
					tid = D.targetID ge
					eid = D.edgeID ge
					newLNodeList = L.filter (\n ->
						let nid = D.nodeID n
						in (nid /= D.sourceID le) && (nid /= D.targetID le))
						lns
				in
				((S.insert (D.sourceID le, sid) $
				  S.insert (D.targetID le, tid) $
				  nmatch,
				  S.insert (D.edgeID le, eid) ematch),
				if mt == Normal || mt == Epi
				then g
				else D.TypedDigraph (D.Digraph (IM.delete sid $ IM.delete tid gnm)
					 			  		       (IM.delete eid gem))
							  	tg,
			 	les,
				newLNodeList))
			candidates
	in newMapSets >>= mapGraphs r mt l
mapGraphs r mt l (m@(nmatch, ematch),
	g@(D.TypedDigraph dg@(D.Digraph gnm gem) tg),
	[], (ln:lns)) =
	let
		conds = (generateConds r l ln g m)
		candidates = filter (processNode conds) $ D.nodes dg
		newMapSets = fmap
			(\gn ->
				let gid = D.nodeID gn
				in
				((S.insert (D.nodeID ln, gid) nmatch, ematch),
				 if mt == Normal || mt == Epi
				 then g
				 else D.TypedDigraph (D.Digraph (IM.delete gid gnm) gem) tg,
				 [],
				 lns))
			candidates
	in newMapSets >>= mapGraphs r mt l



-- | Given two typed graph's, return a list of all possible mappings
-- considering only the subgraph inducted by the edges.
matchGraphs :: Rule a b -> MorphismType -> D.TypedDigraph a b -> D.TypedDigraph a b -> [MapSet]
matchGraphs r mt l@(D.TypedDigraph dl _) g =
	map (\(m, _, _, _) -> m ) $ mapGraphs r mt l ((S.empty, S.empty), g, D.edges dl, D.nodes dl)


------------------------------------------------------------------------
-- Isomorphism related (helper) functions

-- | Check if mapping @m@ is surjective. 

-- Currently, @isSurjective@ relies on @L.nub@ to get the set of nodes and edges
-- mapped in @m@. The number of elements in this set is compared to those from
-- graph @g@ to see if all got mapped.
-- TODO: use Data.Set for efficiency reasons.
isSurjective :: D.TypedDigraph a b -> MapSet -> Bool
isSurjective (D.TypedDigraph (D.Digraph gnm gem) _) m@(nmaps, emaps) =
	IM.size gnm == S.size nmaps && IM.size gem == S.size emaps

-- | Check if a mapping is injective.

-- If the mapping is empty, it's by  definition injective. Otherwise, test, by
-- calling the helper function @iter@ over all node and edge mappings, if they
-- represent an injective relation.  @iter@ does so by checking if any "right
-- side" node/edge got mapped twice, with help of @mem@ that "remembers" the
-- node/edges scanned so far.
{-
isInjective :: MapSet -> Bool
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
-}

-- | List all isomorphisms (represented as mappings) between both graphs.
findIsoMorphisms :: D.TypedDigraph a b -> D.TypedDigraph a b -> [Mapping]
findIsoMorphisms l@(D.TypedDigraph (D.Digraph lnm lem) _) g@(D.TypedDigraph (D.Digraph gnm gem) _) =
	if IM.size lnm /= IM.size gnm ||
	   IM.size lem /= IM.size gem
	then []
	else findMatchesR emptyRule Iso l g

{-
	else filter isInjective $
		 	filter (isSurjective g) $
		 		findMatches Iso l g
-}

-- | Check if there's an isomorphism between two graphs.
isIsomorphic :: D.TypedDigraph a b -> D.TypedDigraph a b -> Bool
isIsomorphic a b = findIsoMorphisms a b /= []


