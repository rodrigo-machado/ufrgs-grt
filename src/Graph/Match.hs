module Graph.Match
	(
	conditionList
	, applyCond
	, testFunc
	)
	where

import Data.Maybe
import Graph.Digraph
import Graph.TypedDigraph
import qualified Data.IntMap as IM
import qualified Data.List as L


findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches l g = undefined
	
{- | given two Graphs 'l' and 'g', an Edge id from Graph 'l' 'eid' and 
   a Morphism 's' (representing the current state), returns a list of valid 
   Morphisms where the given edge and it's source/target nodes are mapped
   to all valid members (i.e. those that satisfy the CSP) from graph 'g'.
-}
matchEdge :: TypedDigraph a b -> TypedDigraph a b -> Int -> Morphism a b -> [Morphism a b]
matchEdge = undefined

{- | A Condition consists of a function that, given a Morphism 'm', two
TypedDigraphs 'l', 'g' and two Edges 'le', 'ge', checks if 'ge' satisfies it
-}
type Condition a b =
	Morphism (TypeInfo a) b
	-> TypedDigraph a b
	-> Edge b 
	-> TypedDigraph a b
	-> Edge b
	-> Bool

{- | checks if 'le' and 'ge' have source nodes from same type -}
srcTypeCond :: Condition a b
srcTypeCond m l le g ge =
	ltype == gtype
	where
		ltype = srcType le l
		gtype = srcType ge g

{- | checks if 'le' and 'ge' have target nodes from same type -}
tarTypeCond :: Condition a b
tarTypeCond m l le g ge =
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
srcIDCond m@(Morphism nal _) l le g ge =
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
tarIDCond m@(Morphism nal _) l le g ge =
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


conditionList = [srcTypeCond, tarTypeCond, srcIDCond, tarIDCond]

{- | if all conditions in 'cl' get satisfied by the given edge 'ge', returns the
'm' Morphism with the new source/target pairs added. 
-}
satisfiesCond
	:: [Condition a b]
	-> Morphism (TypeInfo a) b
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> Edge b
	-> Maybe (Morphism (TypeInfo a) b)
satisfiesCond cl m l@(TypedDigraph ld _) le g@(TypedDigraph gd _) ge =
	if foldr (\c acc -> (c m l le g ge) && acc) True cl 
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
	:: [Condition a b]
	-> Morphism (TypeInfo a) b
	-> TypedDigraph a b
	-> Edge b
	-> TypedDigraph a b
	-> [Morphism (TypeInfo a) b]
applyCond cl m l le g@(TypedDigraph dg _) =
	let candidates = mapMaybe (\ge -> satisfiesCond cl m l le g ge) $ edges dg
	in foldr (\c acc -> c : acc) [] candidates 

{- | written for test purposes in Example.hs.  -}
testFunc :: Int -> TypedDigraph a b -> TypedDigraph a b -> Maybe [Morphism (TypeInfo a) b]
testFunc id l@(TypedDigraph (Digraph _ em) _) g =
	let le = IM.lookup id em
	in case le of
		Just e -> Just $ applyCond conditionList (Morphism [] []) l e g
		Nothing -> Nothing
