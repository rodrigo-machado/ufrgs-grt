{-# LANGUAGE DoAndIfThenElse #-}
module Graph.StateSpace ( StateSpace (..)
                        , runStateSpace
                        ) where

import Prelude

import Graph.Builder.Digraph
import Graph.Digraph
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
type SSBuilder a b m r = GraphBuilder (TypedDigraph a b) (Rule a b, Mapping)  m r 

tpg (TypedDigraph _ t) = t

ns (Digraph n _) = n

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (x, y) = (x, f y)

runStateSpace :: (Eq a, Eq b, MonadIO m) => Int -> TypedDigraph a b -> [Rule a b] -> m (StateSpace a b)
runStateSpace n g r = buildGraph $ do i <- createNode g 1
                                      mkStateSpace n i g r


mkStateSpace :: (Eq a, Eq b, MonadIO m) => Int -> Int -> TypedDigraph a b -> [Rule a b] -> SSBuilder a b m ()
mkStateSpace 0 _ _ _ = return ()
mkStateSpace n i g r = do forM_ r $ \r' ->
                            do forM_ (findMatchesR r' Normal (left r' $ tpg g) g) $ \m -> do
                                t <- rewrite r' g m
                                i' <- putState i t (r', m)
                                if i' == 0
                                  then return ()
                                  else mkStateSpace (n - 1) i' t r
                                
putState :: (Eq a, Eq b, MonadIO m) => Int -> TypedDigraph a b -> (Rule a b, Mapping) -> SSBuilder a b m Int
putState i g e = do states <- liftM (map (onSnd payload) . toList . ns) getG
                    let isos = filter snd $ map (onSnd (isIsomorphic g)) states
                    case isos of 
                        [] -> do i' <- createNode g 1
                                 createEdge e 1 (i, i')
                                 return i'
                        [(i', True)] -> do createEdge e 1 (i, i')
                                           return 0
                        _ -> error $ "Undefined case for " ++ show isos

