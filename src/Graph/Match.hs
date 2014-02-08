module Graph.Match
	(
	matchEdges
	, findMatches
	)
	where

import Control.Monad -- foldM
import Data.Maybe
import Graph.Digraph
import qualified Data.IntMap as IM
import qualified Data.List as L


findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches l g = 
	let matches = matchEdges l g 
	in matches >>= \m -> (matchRemNodes l g m)

	
{- | A Condition consists of a function that, given a Morphism 'm', two
TypedDigraphs 'l', 'g' and two Edges 'le', 'ge', checks if 'ge' satisfies it
-}
type Condition a b =
	TypedDigraph a b
	-> Edge b 
	-> TypedDigraph a b
	-> Edge b
	-> Morphism a b
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
srcIDCond l le g ge m@(Morphism nal _) =
	let lsrc = sourceID le
	    gsrc = sourceID ge
	    matched = (\na ->
		case na of
			(Just n, _) -> lsrc == nodeID n
			otherwise -> False
			) `L.find` nal
	in case matched of	
		Just (_, Just n) -> gsrc == nodeID n
		otherwise -> True

{- | figures out if 'le's target already occurs in 'm'. If that's the case,
tarIDCond checks if 'ge's target is the same node to which 'le's target got
mapped.  If so, 'ge' is a matching Edge. If 'le's target doesn't occur in 'm',
any 'ge' will satisfy this condition
-}
tarIDCond :: Condition a b
tarIDCond l le g ge m@(Morphism nal _) =
	let ltar = targetID le
	    gtar = targetID ge
	    matched = (\na ->
		case na of
			(Just n, _) -> ltar == nodeID n
			otherwise -> False
			) `L.find` nal
	in case matched of	
		Just (_, Just n) -> gtar == nodeID n
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
satisfiesCond
	:: [Condition a b]
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Edge b
	-> Morphism a b
	-> Maybe (Morphism a b)
satisfiesCond cl l@(TypedDigraph ld _) le g@(TypedDigraph gd _) ge m =
	if foldr (\c acc -> (c l le g ge m) && acc) True cl 
	then Just $
		addNodeAction (target le ld) (target ge gd)
		$ addNodeAction (source le ld) (source ge gd)
		$ addEdgeAction	le ge
		$ m
	else Nothing
		

{- | given a Condition list, a Morphism 'm', two Graphs 'l', 'g' and an Edge
'le', searches for all edges from graph 'g' that satisfies the conditions in
this context.  Returns a list of Morphisms, each with the new possibility added
-}
applyCond
	:: [Edge b]
	-> TypedDigraph a b
	-> TypedDigraph a b
	-> Morphism a b
	-> [Morphism a b]
applyCond (le:les) l@(TypedDigraph dl tl) g@(TypedDigraph dg _) m =
	let candidates = mapMaybe 
		(\ge -> satisfiesCond conditionList l le g ge m) $ edges dg
	    newMorphisms = foldr (\c acc -> c : acc) [] candidates
	in applyCondMult 
		les
		(TypedDigraph dl tl)
		g
		newMorphisms

applyCondMult
	:: [Edge b]
	-> TypedDigraph a b
	-> TypedDigraph a b
	-> [Morphism a b]
	-> [Morphism a b]
applyCondMult les l@(TypedDigraph d _) g ml =
	case les of
		[] -> ml
		otherwise -> ml >>= \m -> applyCond les l g m

{- | given to TypedDigraph's, returns a list of all possible morphisms
considering only the subgraph's inducted by the edges -}
matchEdges :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
matchEdges l@(TypedDigraph dg _) g =
	applyCondMult (edges dg) l g [Morphism [] []]

{- | given a Morphism and two lists of nodes, add's NodeActions where the nodes
are sequencially taken from the lists -}
addNodeMatch
	:: Maybe (Morphism a b)
	-> [Node a]
	-> [Node a]
	-> Maybe (Morphism a b)
addNodeMatch m [] _ = m
addNodeMatch m _ [] = Nothing
addNodeMatch Nothing _ _ = Nothing
addNodeMatch (Just m) (x:xs) (y:ys) =
	if nodeType x == nodeType y
		then addNodeMatch (Just $ addNodeAction x y m) xs ys
		else Nothing

{- | given two lists of nodes and a morphism, returns a list of all morphisms,
each corresponding of a unique combination of all nodes from both lists and
without repetition
-}
addNodeMatches
	:: [Node a]
	-> [Node a]
	-> Morphism a b
	-> [Morphism a b]
addNodeMatches xs ys m =
	mapMaybe (addNodeMatch (Just m) xs) $ L.permutations ys
	
{- | Creates a lists of morphisms, each adding to the given Morphism 'm' a
unique combination of the matches considering the nodes not already contained
in 'm' -}
matchRemNodes
	:: TypedDigraph a b
	-> TypedDigraph a b
	-> Morphism a b
	-> [Morphism a b]
matchRemNodes
	l@(TypedDigraph dl _)
	g@(TypedDigraph dg _)
	m@(Morphism nal _) =
	let lnl = nodes dl
	    gnl = nodes dg
	    mlnl = foldr (\(Just ln, _) acc -> ln : acc) [] nal
	    mgnl = foldr (\(_, Just gn) acc -> gn : acc) [] nal
	    rlnl = L.deleteFirstsBy
		(\x y -> nodeID x == nodeID y) lnl mlnl
	    rgnl = L.deleteFirstsBy
		(\x y -> nodeID x == nodeID y) gnl mgnl
	in addNodeMatches rlnl rgnl m


