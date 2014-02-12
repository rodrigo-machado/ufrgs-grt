module Graph.Builder ( GraphBuilder
                     , buildGraph
                     , buildGraphFrom
                     , typeNode
                     , typeEdge
                     , graphNode
                     , graphEdge
                     , deleteTypeNode
                     , deleteTypeEdge
                     , deleteGraphNode
                     , deleteGraphEdge
                     ) where

import Prelude hiding (null,lookup)

import Control.Monad.State

import Data.IntMap hiding (empty)

import Graph.Digraph
import Graph.Rewriting

type UnitGraph = TypedDigraph () ()
type GraphBuilder = StateT UnitGraph

liftMaybe :: Monad m => Maybe a -> m a
liftMaybe Nothing = fail "Nothing"
liftMaybe (Just x) = return x

emptyUnit :: Digraph () ()
emptyUnit = empty

buildGraph :: Monad m => GraphBuilder m a -> m UnitGraph
buildGraph = buildGraphFrom (TypedDigraph emptyUnit emptyUnit)

buildGraphFrom :: Monad m => UnitGraph -> GraphBuilder m c -> m UnitGraph
buildGraphFrom g = liftM snd . flip runStateT g

nextId m = if null m then 0 else 1 + fst (findMax m)

typeNode :: (Monad m) => GraphBuilder m Int
typeNode = do (TypedDigraph g t@(Digraph n e)) <- get
              let k = nextId n
              t' <- addNode (Node k 0 ()) t
              put $ TypedDigraph g t'
              return k

typeEdge :: Monad m => (Int, Int) -> GraphBuilder m Int
typeEdge c = do (TypedDigraph g t@(Digraph n e)) <- get
                let k = nextId e
                t' <- flip addEdge t $ Edge k c 0 ()
                put $ TypedDigraph g t'
                return k

graphNode :: (Monad m) => Int -> GraphBuilder m Int
graphNode t = do (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
                 if lookup t tn == Nothing
                     then fail $ unwords ["Node type", show t, "not in graph"]
                     else do let k = nextId gn
                             g' <- flip addNode g $ Node k t ()
                             put $ TypedDigraph g' tg
                             return k

graphEdge :: Monad m => Int -> (Int, Int) -> GraphBuilder m Int
graphEdge t c = do (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
                   if lookup t te == Nothing
                       then fail $ unwords ["Edge type", show t, "not in graph"]
                       else do let k = nextId ge
                               g' <- flip addEdge g $ Edge k c t ()
                               put $ TypedDigraph g' tg
                               return k

deleteTypeNode :: Monad m => Int -> GraphBuilder m ()
deleteTypeNode i = do (TypedDigraph g t) <- get
                      n <- liftMaybe $ node i t
                      t' <- removeNode n t
                      put $ TypedDigraph g t'
                      return ()


deleteTypeEdge :: Monad m => Int -> GraphBuilder m ()
deleteTypeEdge i = do (TypedDigraph g t) <- get
                      e <- liftMaybe $ edge i t
                      t' <- removeEdge e t
                      put $ TypedDigraph g t'
                      return ()

deleteGraphNode :: Monad m => Int -> GraphBuilder m ()
deleteGraphNode i = do (TypedDigraph g t) <- get
                       n <- liftMaybe $ node i g
                       g' <- removeNode n g
                       put $ TypedDigraph g' t
                       return ()

deleteGraphEdge :: Monad m => Int -> GraphBuilder m ()
deleteGraphEdge i = do (TypedDigraph g t) <- get
                       e <- liftMaybe $ edge i g
                       g' <- removeEdge e g
                       put $ TypedDigraph g' t
                       return ()

type UnitRule = Rule () ()
type RuleBuilder = StateT (UnitRule, UnitGraph)

-- Must also create keep rules for all nodes and edges that are not
-- created or deleted
buildRule :: Monad m => UnitGraph -> RuleBuilder m a -> m UnitRule
buildRule g = liftM (fst . snd) . flip runStateT (Morphism [] [], g)

createNode :: Monad m => RuleBuilder m Int
createNode = undefined

deleteNode :: Monad m => Int -> RuleBuilder m ()
deleteNode = undefined

createEdge :: Monad m => (Int, Int) -> RuleBuilder m Int
createEdge = undefined

deleteEdge :: Monad m => Int -> RuleBuilder m ()
deleteEdge = undefined