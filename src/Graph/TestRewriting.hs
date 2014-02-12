module Graph.TestRewriting where

import Control.Monad.State

import Data.Maybe

import Graph.Match
import Graph.Rewriting
import Graph.Digraph
import Graph.Builder

-- Prelude's Either monad is broken, since fail only accepts Strings as arguments
data Error r = Err String | OK r deriving (Eq, Show)

instance Monad Error where
    return x = OK x
    (Err y) >>= _ = Err y
    (OK x) >>= f = (f x)
    fail = Err

rewriteOnError :: (Eq a, Eq b) => Rule a b -> TypedDigraph a b -> [Error (TypedDigraph a b)]
rewriteOnError = rewrite

tGraph :: Monad m => GraphBuilder m (Int, Int, Int, Int, Int, Int)
tGraph = do
    n1 <- typeNode
    n2 <- typeNode
    e1 <- typeEdge (n1, n1)
    e2 <- typeEdge (n2, n2)
    e3 <- typeEdge (n1, n2)
    e4 <- typeEdge (n2, n1)
    return (n1, n2, e1, e2, e3, e4)

beta :: Monad m => m (TypedDigraph () ())
beta = buildGraph $ do (tn1, tn2, te1, te2, te3, te4) <- tGraph
                       n1 <- graphNode tn1
                       n2 <- graphNode tn2
                       n3 <- graphNode tn1
                       n4 <- graphNode tn2
                       e1 <- graphEdge te1 (n1, n3)
                       -- etc.
                       return ()
