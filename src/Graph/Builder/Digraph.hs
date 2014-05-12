{-# LANGUAGE GADTs #-}
module Graph.Builder.Digraph ( module Graph.Digraph
                             , GraphBuilderT (..)
                             , GraphBuilder (..)
                             , buildGraphT
                             , buildGraphTFrom
                             , buildGraph
                             , buildGraphFrom
                             , createNode
                             , createEdge
                             , getG
                             , putG
                             , getS
                             , putS
                             ) where
import Prelude hiding (null,lookup,elem)

import Control.Monad.State
import Control.Monad.Identity

import Data.IntMap hiding (empty,map,filter,(\\))
import Data.Maybe
import Data.List hiding (lookup,null)

import Graph.Digraph hiding (nodes,edges,node,edge)
import Graph.Rewriting

import Assorted.Maybe

{-| A graph builder that carries extra state (s). -}

{- allow me to help you parse the type. a is the node payload type, b is the edge payload type, s is the carried state. Also
   m is the embadded monad and r is the return type. -}
type GraphBuilderT a b s m r = StateT (Digraph a b, s) m r

type GraphBuilder a b m r = GraphBuilderT a b () m r

{-| Builds from an empty graph -}
buildGraphT :: (Monad m) => s -> GraphBuilderT a b s m r -> m (Digraph a b)
buildGraphT = buildGraphTFrom empty

{-| Builds from an specified graph -}
buildGraphTFrom :: (Monad m) => Digraph a b -> s -> GraphBuilderT a b s m r -> m (Digraph a b)
buildGraphTFrom g s b =  do { (a, (g', s')) <- runStateT b (g, s); return g' }

buildGraphFrom :: Monad m => Digraph a b -> GraphBuilder a b m r -> m (Digraph a b)
buildGraphFrom g = buildGraphTFrom g ()

buildGraph :: Monad m => GraphBuilder a b m r -> m (Digraph a b)
buildGraph = buildGraphFrom empty

nodes (Digraph g _) = g
edges (Digraph _ e) = e

nextId m = if null m then 0 else 1 + fst (findMax m)
nextNodeId = nextId . nodes
nextEdgeId = nextId . edges

getG :: Monad m => GraphBuilderT a b s m (Digraph a b)
getG = do { s <- get; return $ fst s }

putG :: Monad m => Digraph a b -> GraphBuilderT a b s m ()
putG g = do s <- get
            put (g, snd s)

getS :: Monad m => GraphBuilderT a b s m s
getS = do { s <- get; return $ snd s }

putS :: Monad m => s -> GraphBuilderT a b s m ()
putS s = do s' <- get
            put (fst s', s)

createNode :: Monad m => a -> Int -> GraphBuilderT a b s m Int
createNode p t = do g <- getG
                    let k = nextId $ nodes g
                    g' <- flip addNode g $ Node k t p
                    putG g'
                    return k

createEdge :: Monad m => b -> Int -> (Int, Int) -> GraphBuilderT a b s m Int
createEdge p t c = do g <- getG
                      let k = nextId $ edges g
                      g' <- flip addEdge g $ Edge k c t p
                      putG g'
                      return k


