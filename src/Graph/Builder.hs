module Graph.Builder ( GraphBuilder
               , buildGraph
               , typeNode
               , typeEdge
               , graphNode
               , graphEdge
               ) where

import Prelude hiding (null,lookup)

import Control.Monad.State

import Data.IntMap hiding (empty)

import Graph.Digraph
import Graph.Rewriting

type UnitDigraph = TypedDigraph () ()
type GraphBuilder = StateT UnitDigraph

emptyUnit :: Digraph () ()
emptyUnit = empty

buildGraph :: Monad m => GraphBuilder m a -> m UnitDigraph
buildGraph = liftM snd . flip runStateT (TypedDigraph emptyUnit emptyUnit)

nextId m = if null m then 0 else 1 + fst (findMax m)

typeNode :: (Monad m) => GraphBuilder m Int
typeNode = do
    (TypedDigraph g t@(Digraph n e)) <- get
    let k = nextId n
    t' <- addNode (Node k 0 ()) t
    put $ TypedDigraph g t'
    return k

typeEdge :: Monad m => (Int, Int) -> GraphBuilder m Int
typeEdge c = do
    (TypedDigraph g t@(Digraph n e)) <- get
    let k = nextId e
    t' <- flip addEdge t $ Edge k c 0 ()
    put $ TypedDigraph g t'
    return k

graphNode :: (Monad m) => Int -> GraphBuilder m Int
graphNode t = do
    (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
    if lookup t tn == Nothing
        then fail $ unwords ["Node type", show t, "not in graph"]
        else do let k = nextId gn
                g' <- flip addNode g $ Node k t ()
                put $ TypedDigraph g' tg
                return k

graphEdge :: Monad m => Int -> (Int, Int) -> GraphBuilder m Int
graphEdge t c = do
    (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
    if lookup t te == Nothing
        then fail $ unwords ["Edge type", show t, "not in graph"]
        else do let k = nextId ge
                g' <- flip addEdge g $ Edge k c t ()
                put $ TypedDigraph g' tg
                return k

type UnitRule = Rule () ()
type RuleBuilder = StateT (UnitRule, UnitDigraph)

-- Must also create keep rules for all nodes and edges that are not
-- created or deleted
buildRule :: Monad m => UnitDigraph -> RuleBuilder m a -> m UnitRule
buildRule g = liftM (fst . snd) . flip runStateT (Morphism [] [], g)

createNode :: Monad m => RuleBuilder m Int
createNode = undefined

deleteNode :: Monad m => Int -> RuleBuilder m ()
deleteNode = undefined

createEdge :: Monad m => (Int, Int) -> RuleBuilder m Int
createEdge = undefined

deleteEdge :: Monad m => Int -> RuleBuilder m ()
deleteEdge = undefined