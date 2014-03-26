{-# LANGUAGE DoAndIfThenElse #-}
module Graph.StateSpace ( StateSpace (..) 
                        , runSpaceState
                        ) where

import Prelude hiding (Empty)

import Graph.Builder.Digraph
import Graph.Match
import Graph.Rewriting
import Assorted.PrettyPrint

import Data.List
import Data.Maybe
import Data.IntMap (fromList,toList,elems)
import qualified Data.IntMap as IM

import Control.Monad
import Control.Monad.IO.Class

{- Yo dawg, I heard you like transforming graphs, so we put a graph inside a graph
   so you can transform while you transform. -- Xibit -}
{- Actually, fits better on second order transofrmation. -}
type StateSpace a b = Digraph (TypedDigraph a b) (Rule a b, Mapping)
type SSBuilder a b m r = GraphBuilder (TypedDigraph a b) (Rule a b, Mapping) m r

data St a b = St [TypedDigraph a b] (StateSpace a b) [Rule a b] Int 

tpg (TypedDigraph _ t) = t

runSpaceState :: (Eq a, Eq b, MonadIO m) => Int -> TypedDigraph a b -> [Rule a b] -> m (Digraph (TypedDigraph a b) (Rule a b, Mapping))
runSpaceState n g0 rs = buildGraph $ do n' <- createNode g0 1
                                        stateSpace n g0 n' rs

stateSpace :: (Eq a, Eq b, MonadIO m) => Int -> TypedDigraph a b -> Int -> [Rule a b] -> SSBuilder a b m ()
stateSpace 0 _ _ _  = return ()
stateSpace n g i rs = do forM_ rs $ \r ->
                           forM_ (findMatches g $ left r $ tpg g) $ \m -> do
                               t <- rewrite r g m
                               (Digraph ns' es) <- getG
                               let ns = map snd $ toList ns'
                                   isos = filter (isIsomorphic t . payload) ns
                               if isos == [] then do
                                   n' <- createNode t 1
                                   createEdge (r, m) 1 (i, n')
                                   stateSpace (n - 1) t n' rs
                               else
                                   return () 

{-| Eliminates all isomorphic graphs in a list except the first -}
nubIsomorphics :: [TypedDigraph a b] -> [TypedDigraph a b]
nubIsomorphics = flip niso []
    where niso (g:gs) [] = niso gs [g]
          niso []     hs = hs
          niso (g:gs) hs | any (isIsomorphic g) hs = niso gs hs
                         | otherwise               = niso gs (g:hs)

{-| check if a graph is isomorphic to another -}
{- Basically, check if the morphism between two graphs is total for both sides. -}
isIsomorphic :: TypedDigraph a b -> TypedDigraph a b -> Bool
isIsomorphic t1 t2 = findIsoMorphisms t1 t2 /= []

{- Verify if a morphism is total, by checking if the domain and pre-image are equal. -}
total :: Eq a => [a] -> [a] -> Bool
total as bs = length as == length bs &&
    all (`elem` bs) as && all (`elem` as) bs

tGraph (TypedDigraph _ t) = t
for = flip map
