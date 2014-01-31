module Graph.Match
	(
	matchEdges
	)
	where

import Control.Monad -- foldM
import Data.Maybe
import Graph.Digraph
import Graph.TypedDigraph
import qualified Data.IntMap as IM
import qualified Data.List as L


findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches l g = undefined
	
{- | A Condition consists of a function that, given a Morphism 'm', two
TypedDigraphs 'l', 'g' and two Edges 'le', 'ge', checks if 'ge' satisfies it
-}
type Condition a b =
	TypedDigraph a b
	-> Edge b 
	-> TypedDigraph a b
	-> Edge b
	-> Morphism (TypeInfo a) b
	-> Bool

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

conditionList = [srcTypeCond, tarTypeCond, srcIDCond, tarIDCond, loopCond]

{- | if all conditions in 'cl' get satisfied by the given edge 'ge', returns the
'm' Morphism with the new source/target pairs added. 
-}
satisfiesCond
	:: [Condition a b]
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Edge b
	-> Morphism (TypeInfo a) b
	-> Maybe (Morphism (TypeInfo a) b)
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
	-> Morphism (TypeInfo a) b
	-> [Morphism (TypeInfo a) b]
applyCond (le:les) l@(TypedDigraph dl tl) g@(TypedDigraph dg _) m =
	let candidates = mapMaybe 
		(\ge -> satisfiesCond conditionList l le g ge m) $ edges dg
	    newMorphisms = foldr (\c acc -> c : acc) [] candidates
	    Just newDigraph = removeEdge le dl
	in applyCondMult 
		les
		(TypedDigraph newDigraph tl)
		g
		newMorphisms

applyCondMult
	:: [Edge b]
	-> TypedDigraph a b
	-> TypedDigraph a b
	-> [Morphism (TypeInfo a) b]
	-> [Morphism (TypeInfo a) b]
applyCondMult les l@(TypedDigraph d _) g ml =
	case les of
		[] -> ml
		otherwise -> ml >>= \m -> applyCond les l g m

emptyMorphism = Morphism [] [] :: Morphism (TypeInfo a) b

matchEdges :: TypedDigraph a b -> TypedDigraph a b -> [Morphism (TypeInfo a) b]
matchEdges l@(TypedDigraph dg _) g =
	applyCondMult (edges dg) l g [emptyMorphism]
