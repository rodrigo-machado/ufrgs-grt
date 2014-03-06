module Graph.Match
	(
	findMatches
	)
	where

import Control.Monad -- foldM
import Data.Maybe
import Graph.Graph
import qualified Data.IntMap as IM
import qualified Data.List as L

type Match = [(Int, Int)]

findMatches :: TypedGraph a b -> TypedGraph a b -> [Match]
findMatches l g = 
	let matches = matchEdges l g 
	in matches >>= \m -> (matchNodes l g m)

	
{- | A Condition consists of a function that, given a Morphism 'm', two
TypedGraphs 'l', 'g' and two Edges 'le', 'ge', checks if 'ge' satisfies it
-}
type Condition a b =
	TypedGraph a b
	-> Edge b 
	-> TypedGraph a b
	-> Edge b
	-> Match
	-> Bool

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
srcIDCond l le g ge m =
	let lsrc = sourceID le
	    gsrc = sourceID ge
	    matched = (\(s, t) -> s == lsrc) `L.find` m
	in case matched of	
		Just (_, n) -> gsrc == n
		otherwise -> True

{- | figures out if 'le's target already occurs in 'm'. If that's the case,
tarIDCond checks if 'ge's target is the same node to which 'le's target got
mapped.  If so, 'ge' is a matching Edge. If 'le's target doesn't occur in 'm',
any 'ge' will satisfy this condition
-}
tarIDCond :: Condition a b
tarIDCond l le g ge m =
	let ltar = targetID le
	    gtar = targetID ge
	    matched = (\(s, t) -> s == ltar) `L.find` m
	in case matched of	
		Just (_, n) -> gtar == n
		otherwise -> True

{- | If 'le' is a loop edge, forces 'ge' to be a loop in 'g' -}
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
'm' Morphism with the new source/target pairs added. 
-}
processEdges
	:: [Condition a b]
	-> TypedGraph a b
	-> Edge b
	-> TypedGraph a b
	-> Edge b
	-> Match
	-> Maybe Match
processEdges cl l@(TypedGraph ld _) le g@(TypedGraph gd _) ge m =
	if foldr (\c acc -> (c l le g ge m) && acc) True cl 
	then Just $
		(targetID le, targetID ge) :
		(sourceID le, sourceID ge) :
	--TODO: ops, Match e edge?
		(edgeID le, edgeID ge) :
		m
	else Nothing
		

{- | given a Condition list, a Morphism 'm', two Graphs 'l', 'g' and an Edge
'le', searches for all edges from graph 'g' that satisfy the conditions in
this context.  Returns a list of Morphisms, each with the new possibility added
-}
applyCond
	:: [Edge b]
	-> TypedGraph a b
	-> TypedGraph a b
	-> Match
	-> [Match]
applyCond (le:les) l g@(TypedGraph dg _) m =
	let newMatches = mapMaybe 
		(\ge -> processEdges conditionList l le g ge m) $ edges dg
	in applyCondMult 
		les
		l
		g
		newMatches

{- | given a list of Edges in graph 'l' and a list of partial morphisms, 
returns all possible morphisms with these Edges mapped -}
applyCondMult
	:: [Edge b]
	-> TypedGraph a b
	-> TypedGraph a b
	-> [Match]
	-> [Match]
applyCondMult les l@(TypedGraph d _) g ml =
	case les of
		[] -> ml
		otherwise -> ml >>= \m -> applyCond les l g m

{- | given to TypedGraph's, returns a list of all possible morphisms
considering only the subgraph's inducted by the edges -}
matchEdges :: TypedGraph a b -> TypedGraph a b -> [Match]
matchEdges l@(TypedGraph dg _) g =
	applyCondMult (edges dg) l g []

addNodeMatch
	:: [Node a]
	-> TypedGraph a b
	-> Match
	-> [Match]
addNodeMatch (ln:lns) g@(TypedGraph dg _) m =
	let ltype = nodeType ln
	    candidates = filter (\n -> nodeType n == ltype) $ nodes dg
	    newMatches = fmap (\c -> (nodeID ln, nodeID c) : m) candidates
	in addNodeMatches
		lns
		g
		newMatches
--addNodeMatch [] _ m = return m

addNodeMatches
	:: [Node a]
	-> TypedGraph a b
	-> [Match]
	-> [Match]
addNodeMatches lns g ml =
	case lns of
		[] -> ml
		otherwise -> ml >>= \m -> addNodeMatch lns g m

matchNodes :: TypedGraph a b -> TypedGraph a b -> Match -> [Match]
matchNodes l@(TypedGraph dl _) g m =
	let lnl = nodes dl
	    mlnl = foldr (\(ln, _) acc ->
                let node = findNode ln dl in
		case node of
			(Just n) -> n : acc
			otherwise -> acc) [] m
	    rlnl = lnl L.\\ mlnl
	in addNodeMatch rlnl g m
