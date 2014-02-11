module Graph.Rewriting where

import Data.Maybe
import Data.List
import Data.IntMap (IntMap,keys)

import Graph.Digraph

import Control.Monad
import Control.Arrow

type Rule a b = Morphism a b

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches = undefined

rewrite :: (Monad m, Eq a, Eq b) => Rule a b -> TypedDigraph a b -> Morphism a b -> m (TypedDigraph a b)
rewrite rule tGraph@(TypedDigraph graph types) match = applyTypedMorphism (rename ns es rule match) tGraph
	where
		keyList :: IntMap a -> [Int]
		keyList = keys
		ns = 1 + (maximum $ map nodeID $ nodes graph)
		es = 1 + (maximum $ map edgeID $ edges graph)

renameNode :: [(Int, Int)] -> Node a -> Node a
renameNode namemap (Node id x) = Node (fromJust $ lookup id namemap) x

renameEdge nnamemap enamemap (Edge id x y) = Edge (fromJust $ lookup id enamemap) 
	(double (fromJust . flip lookup nnamemap) x) y

-- | Applies f to both elements in a tuple
double :: (a -> b) -> (a, a) -> (b, b)
double f = f *** f
 
rename :: (Eq a, Eq b) => Int -> Int -> Rule a b -> Morphism a b -> Rule a b
rename ns es r@(Morphism nr er) m@(Morphism nm em) = Morphism 
	(nub $ map (double (liftM (renameNode nodeIdMap))) nr) 
	(nub $ map (double (liftM (renameEdge nodeIdMap edgeIdMap))) er)
	where
		cNodeIdMap = zip (map (nodeID . fromJust . snd) $ filter newNames nr) [ns..]
		cEdgeIdMap = zip (map (edgeID . fromJust . snd) $ filter newNames er) [es..]
		nodeIdMap = cNodeIdMap ++ map (double (nodeID . fromJust)) nm
		edgeIdMap = cEdgeIdMap ++ map (double (edgeID . fromJust)) em
		newNames, renames :: Eq a => (Maybe a, b) -> Bool
		newNames = (== Nothing) . fst
		renames  = (/= Nothing) . fst
