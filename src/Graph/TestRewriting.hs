module Graph.TestRewriting where

import Control.Monad.State

import Data.Maybe

import Assorted.PrettyPrint

import Graph.Match
import Graph.Rewriting
import Graph.Digraph
import Graph.Builder

-- Prelude's Either monad is broken, since fail only accepts Strings as arguments
data Error r = Err String | OK r deriving (Eq, Show)

instance Monad Error where
    return = OK
    (Err y) >>= _ = Err y
    (OK x) >>= f = (f x)
    fail = Err

instance PrettyPrint a => PrettyPrint (Error a) where
    prettyPrint (OK v) = prettyPrint v
    prettyPrint (Err m) = m

rewriteOnError :: (Eq a, Eq b) => Rule a b -> TypedDigraph a b -> [Error (TypedDigraph a b)]
rewriteOnError = rewrite

rewriteAllOnError :: (Eq a, Eq b) => [Rule a b] -> [TypedDigraph a b] -> [Error (TypedDigraph a b)]
rewriteAllOnError = rewriteAll

tGraph :: Monad m => GraphBuilder m [(String, Int)]
tGraph = do n1 <- typeNode
            n2 <- typeNode
            e1 <- typeEdge (n1, n1)
            e2 <- typeEdge (n2, n2)
            e3 <- typeEdge (n1, n2)
            e4 <- typeEdge (n2, n1)
            return [("n1", n1), ("n2", n2), ("e1", e1), ("e2", e2), ("e3", e3), ("e4", e4)]

alpha :: Monad m => m (TypedDigraph () ())
alpha = buildGraph $ do types <- tGraph
                        graphNode $ fromJust $ lookup "n1" types

beta :: Monad m => m (TypedDigraph () ())
beta = buildGraph $ do types <- tGraph
                       let tn1 = fromJust $ lookup "n1" types
                           te1 = fromJust $ lookup "e1" types
                       n1 <- graphNode tn1
                       n2 <- graphNode tn1
                       n3 <- graphNode tn1
                       n4 <- graphNode tn1
                       e1 <- graphEdge te1 (n1, n3)
                       return ()

rule1 :: Monad m => m (Rule () ())
rule1 = alpha >>= flip buildRule (deleteNode 0)

rule2 :: Monad m => m (Rule () ())
rule2 = alpha >>= flip buildRule (createNode () 0)

rules :: Monad m => m [Rule () ()]
rules = sequence [rule1, rule2]

rewriteBeta = do b <- beta
                 r <- rule1
                 rewriteOnError r b
