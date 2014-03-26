module Graph.Rewriting ( Rule
                       , left
                       , rewrite
                       ) where

import Data.Maybe
import Data.List
import Data.IntMap (IntMap,keys,fromList)

import Graph.Digraph
import Graph.Match (findMatches,Mapping)

import Control.Monad
import Control.Arrow hiding (left)

type Rule a b = Morphism a b

left :: (Eq a, Eq b) => Rule a b -> TGraph a b -> TypedDigraph a b
left (Morphism nr er) t = flip TypedDigraph t $ Digraph (fromList . toKeyPair $ left' nr) (fromList . toKeyPair $ left' er)
    where
        left' :: Eq a => [(Maybe a, Maybe a)] -> [a]
        left' = map (fromJust . fst) . filter (\e -> fst e /= Nothing)
        toKeyPair :: Element e => [e] -> [(Int, e)]
        toKeyPair = map (\n -> (elemId n, n))

rewrite :: (Monad m, Eq a, Eq b) => Rule a b -> TypedDigraph a b -> Mapping -> m (TypedDigraph a b)
rewrite rule tGraph@(TypedDigraph graph _) match = applyTypedMorphism (rename ns es rule match) tGraph
    where
        keyList :: IntMap a -> [Int]
        keyList = keys
        ns = 1 + (maximum $ map elemId $ nodes graph)
        es = 1 + (maximum $ map elemId $ edges graph)


renameNode :: [(Int, Int)] -> Node a -> Node a
renameNode namemap (Node id t p) = Node (fromJust $ lookup id namemap) t p

renameEdge nnamemap enamemap (Edge id st t p) = Edge (fromJust $ lookup id enamemap) 
	(double (fromJust . flip lookup nnamemap) st) t p

-- | Applies f to both elements in a tuple
double :: (a -> b) -> (a, a) -> (b, b)
double f = f *** f
 
rename :: (Eq a, Eq b) => Int -> Int -> Rule a b -> Mapping -> Rule a b
rename ns es r@(Morphism nr er) m@((nm, em)) = Morphism 
    (nub $ map (double (liftM (renameNode nodeIdMap))) nr)
    (nub $ map (double (liftM (renameEdge nodeIdMap edgeIdMap))) er)
    where newElems f r         = map (f . fromJust . snd) $ (filter newNames r)
          cNodeIdMap           = zip (newElems elemId nr) [ns..]
          cEdgeIdMap           = zip (newElems elemId er) [es..]
          nodeIdMap            = cNodeIdMap ++ nm
          edgeIdMap            = cEdgeIdMap ++ em
          newNames, renames :: Eq a => (Maybe a, b) -> Bool
          newNames             = (== Nothing) . fst
          renames              = (/= Nothing) . fst
