module Graph.Rewriting ( Rule
                       , rewrite
                       , rewriteAll
                       ) where

import Data.Maybe
import Data.List
import Data.IntMap (IntMap,keys,fromList)

import Graph.Digraph
import Graph.Match (findMatches)

import Control.Monad
import Control.Arrow hiding (left)

type Rule a b = Morphism a b

left :: (Eq a, Eq b) => Rule a b -> TGraph a b -> TypedDigraph a b
left (Morphism nr er) t = flip TypedDigraph t $ Digraph (fromList . toNodeKeyPair $ left' nr) (fromList . toEdgeKeyPair $ left' er)
	where
		left' :: Eq a => [(Maybe a, Maybe a)] -> [a]
		left' = map (fromJust . fst) . filter (\e -> fst e /= Nothing)
		toNodeKeyPair = map (\n -> (nodeID n, n))
		toEdgeKeyPair = map (\e -> (edgeID e, e))

rewrite :: (Monad m, Eq a, Eq b) => Rule a b -> TypedDigraph a b -> [m (TypedDigraph a b)]
rewrite rule@(Morphism nr er) graph = liftM (doRewrite rule graph) $ findMatches alpha graph
	where
		tGraph (TypedDigraph g t) = t
		alpha = left rule $ tGraph graph

rewriteAll :: (Monad m, Eq a, Eq b) => [Rule a b] -> [TypedDigraph a b] -> [m (TypedDigraph a b)]
rewriteAll rs = concat . map (\g -> concat $ map (flip rewrite g) rs)


doRewrite :: (Monad m, Eq a, Eq b) => Rule a b -> TypedDigraph a b -> Morphism a b -> m (TypedDigraph a b)
doRewrite rule tGraph@(TypedDigraph graph types) match = applyTypedMorphism (rename ns es rule match) tGraph
	where
		keyList :: IntMap a -> [Int]
		keyList = keys
		ns = 1 + (maximum $ map nodeID $ nodes graph)
		es = 1 + (maximum $ map edgeID $ edges graph)

renameNode :: [(Int, Int)] -> Node a -> Node a
renameNode namemap (Node id t p) = Node (fromJust $ lookup id namemap) t p

renameEdge nnamemap enamemap (Edge id st t p) = Edge (fromJust $ lookup id enamemap) 
	(double (fromJust . flip lookup nnamemap) st) t p

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
