module Graph.Match
	(
	findMatches
	)
	where

import Control.Monad -- foldM
import Data.Maybe
import Graph.Digraph
import qualified Data.IntMap as IM
import qualified Data.List as L

-- | Mapping:  (Node matches, Edge matches)
type Mapping = ([(Int, Int)], [(Int, Int)])


findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Mapping]
findMatches l g = 
	let matches = matchEdges l g 
	in matches >>= \m -> (matchNodes l g m)

	
insNodeMapping :: (Int, Int) -> Mapping -> Mapping
insNodeMapping newm m@(ns, es) =
	if newm `L.elem` ns
	then m
	else (newm:ns, es)

insEdgeMapping :: (Int, Int) -> Mapping -> Mapping
insEdgeMapping newm m@(ns, es) =
	if newm `L.elem` es
	then m
	else (ns, newm:es)


{- | A Condition consists of a function that, given a Mapping 'm', two
TypedDigraphs 'l', 'g' and two Edges 'le', 'ge', checks if 'ge' satisfies it
according to internal rules
-}
type Condition a b =
	TypedDigraph a b
	-> Edge b 
	-> TypedDigraph a b
	-> Edge b
	-> Mapping
	-> Bool

{- | Checks if both edges are from the same type -}
edgeTypeCond :: Condition a b
edgeTypeCond l le g ge m =
	edgeType le == edgeType ge

{- | checks if 'le' and 'ge' have source nodes from same type -}
srcTypeCond :: Condition a b
srcTypeCond l le g ge m =
	ltype == gtype
	where
		ltype = srcType le l
		gtype = srcType ge g

{- | checks if 'le' and 'ge' have target nodes from same type -}
tarTypeCond :: Condition a b
tarTypeCond l le g ge m =
	ltype == gtype
	where
		ltype = tarType le l
		gtype = tarType ge g

{- | figures out if 'le's source already occurs in 'm'. If that's the case,
srcIDCond checks if 'ge's source is the same node to which 'le's source got
mapped.  If so, 'ge' is a matching Edge. If 'le's source doesn't occur in 'm',
any 'ge' will satisfy this condition
-}
srcIDCond :: Condition a b
srcIDCond l le g ge m@(nmatches, _) =
	let lsrc = sourceID le
	    gsrc = sourceID ge
	    matched = (\(s, t) -> s == lsrc) `L.find` nmatches
	in case matched of	
		Just (_, n) -> gsrc == n
		otherwise -> True

{- | figures out if 'le's target already occurs in 'm'. If that's the case,
tarIDCond checks if 'ge's target is the same node to which 'le's target got
mapped.  If so, 'ge' is a matching Edge. If 'le's target doesn't occur in 'm',
any 'ge' will satisfy this condition
-}
tarIDCond :: Condition a b
tarIDCond l le g ge m@(nmatches, _) =
	let ltar = targetID le
	    gtar = targetID ge
	    matched = (\(s, t) -> s == ltar) `L.find` nmatches
	in case matched of	
		Just (_, n) -> gtar == n
		otherwise -> True

{- | If 'le' is a loop edge, forces 'ge' to be a loop in 'g'. This is due the
sequential nature of processEdges. A loop edge being first matched, for
example, isn't able to detect a coincident node that didn't appear earlier in
the Mapping
-}
loopCond :: Condition a b
loopCond l le g ge m =
	let lsrc = sourceID le
	    ltar = targetID le
	    gsrc = sourceID ge
	    gtar = targetID ge
	in if lsrc == ltar
		then gsrc == gtar
		else True

conditionList = [edgeTypeCond, srcTypeCond, tarTypeCond, srcIDCond, tarIDCond, loopCond]

{- | if all conditions in 'cl' get satisfied by the given edge 'ge', returns the
'm' Mapping with the new source/target pairs added. 
-}
processEdges
	:: [Condition a b]
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Edge b
	-> Mapping
	-> Maybe Mapping
processEdges cl l@(TypedDigraph ld _) le g@(TypedDigraph gd _) ge m =
	if foldr (\c acc -> (c l le g ge m) && acc) True cl 
	then Just $
		insNodeMapping (targetID le, targetID ge) $ -- adds node Mappings
		insNodeMapping (sourceID le, sourceID ge) $
		insEdgeMapping (edgeID le, edgeID ge)	  $ -- adds edge Mapping
		m
	else Nothing
		

{- | given a Condition list, a Morphism 'm', two Graphs 'l', 'g' and an Edge
'le', searches for all edges from graph 'g' that satisfy the conditions in
this context.  Returns a list of Morphisms, each with the new possibility added
-}
applyCond
	:: [Edge b]
	-> TypedDigraph a b
	-> TypedDigraph a b
	-> Mapping
	-> [Mapping]
applyCond (le:les) l g@(TypedDigraph dg _) m =
	let newMappings = mapMaybe 
		(\ge -> processEdges conditionList l le g ge m) $ edges dg
	in applyCondMult 
		les
		l
		g
		newMappings

{- | given a list of Edges in graph 'l' and a list of partial morphisms, 
returns all possible morphisms with these Edges mapped -}
applyCondMult
	:: [Edge b]
	-> TypedDigraph a b
	-> TypedDigraph a b
	-> [Mapping]
	-> [Mapping]
applyCondMult les l@(TypedDigraph d _) g ml =
	case les of
		[] -> ml
		otherwise -> ml >>= \m -> applyCond les l g m

{- | given two TypedDigraph's, returns a list of all possible morphisms
considering only the subgraph's inducted by the edges -}
matchEdges :: TypedDigraph a b -> TypedDigraph a b -> [Mapping]
matchEdges l@(TypedDigraph dg _) g =
	applyCondMult (edges dg) l g [([], [])]

{- | given a list of Nodes @ln@, a TypedDigraph @g@ and a Mapping @m@, returns a list of 
Mappings, each containing a single possible node match added to @m@ -}
addNodeMapping
	:: [Node a]
	-> TypedDigraph a b
	-> Mapping
	-> [Mapping]
addNodeMapping (ln:lns) g@(TypedDigraph dg _) m@(nmatch, ematch) =
	let ltype = nodeType ln
	    candidates = filter (\n -> nodeType n == ltype) $ nodes dg
	    newMappings = 
		fmap (\c -> ((nodeID ln, nodeID c) : nmatch, ematch))
		     candidates
	in addNodeMappings
		lns
		g
		newMappings
--addNodeMapping [] _ m = return m

{- used in mutual recursion with addNodeMapping. -}
addNodeMappings
	:: [Node a]
	-> TypedDigraph a b
	-> [Mapping]
	-> [Mapping]
addNodeMappings lns g ml =
	case lns of
		[] -> ml
		otherwise -> ml >>= \m -> addNodeMapping lns g m

matchNodes :: TypedDigraph a b -> TypedDigraph a b -> Mapping -> [Mapping]
matchNodes l@(TypedDigraph dl _) g m@(nmatches, _) =
	let lnl = nodes dl
	    mlnl = foldr (\(ln, _) acc ->
                let node = findNode ln dl in
		case node of
			(Just n) -> n : acc
			otherwise -> acc) [] nmatches 
	    rlnl = lnl L.\\ mlnl
	in addNodeMapping rlnl g m

