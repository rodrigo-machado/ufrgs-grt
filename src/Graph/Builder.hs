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
                     , buildRule
                     , buildRuleFrom
                     , createNode
                     , createEdge
                     , deleteNode
                     , deleteEdge
                     ) where

import Prelude hiding (null,lookup,member)

import Control.Monad.State

import Data.IntMap hiding (empty)

import Graph.Digraph
import Graph.Rewriting

import Assorted.Maybe

type UnitGraph = TypedDigraph () ()
type GraphBuilder = StateT UnitGraph

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

guardKey t m = unless (member t m) $ fail $ unwords ["Key", show t, "not in map"]

graphNode :: (Monad m) => Int -> GraphBuilder m Int
graphNode t = do (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
                 guardKey t tn
                 let k = nextId gn
                 g' <- flip addNode g $ Node k t ()
                 put $ TypedDigraph g' tg
                 return k

graphEdge :: Monad m => Int -> (Int, Int) -> GraphBuilder m Int
graphEdge t c = do (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
                   guardKey t te
                   let k = nextId ge
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
buildRuleFrom :: Monad m => UnitRule -> UnitGraph -> RuleBuilder m a -> m UnitRule
buildRuleFrom r g = liftM (fst . snd) . flip runStateT (r, g)

buildRule :: Monad m => UnitGraph -> RuleBuilder m a -> m UnitRule
buildRule g = buildRuleFrom (Morphism [] []) g

createNode :: Monad m => Int -> RuleBuilder m Int
createNode t = do (r, g) <- get
                  let (TypedDigraph g' t') = g
                      (Digraph n e) = g'
                      (Digraph tn te) = t'
                      (Morphism nr er) = r
                      k = nextId n
                  guardKey t tn
                  let r' = (Nothing, Just $ Node k t ())
                  put (Morphism (r':nr) er, g)
                  return k

deleteNode :: Monad m => Int -> RuleBuilder m ()
deleteNode i = do (r, g) <- get
                  let (TypedDigraph g' t') = g
                      (Digraph n e) = g'
                      (Morphism nr er) = r
                  n' <- liftMaybeMsg (unwords $ ["Node", show i, "not in graph"]) $
                      lookup i n
                  let r' = (Just n', Nothing)
                  put (Morphism (r':nr) er, g)
                  return ()

createEdge :: Monad m => Int -> (Int, Int) -> RuleBuilder m Int
createEdge t c = do (r, g) <- get
                    let (TypedDigraph g' t') = g
                        (Digraph n e) = g'
                        (Digraph tn te) = t'
                        (Morphism nr er) = r
                        k = nextId e
                    guardKey t te
                    let r' = (Nothing, Just $ Edge k c t ())
                    put (Morphism nr (r':er), g)
                    return k

deleteEdge :: Monad m => Int -> RuleBuilder m ()
deleteEdge i = do (r, g) <- get
                  let (TypedDigraph g' t') = g
                      (Digraph n e) = g'
                      (Morphism nr er) = r
                  e' <- liftMaybeMsg (unwords $ ["Edge", show i, "not in graph"]) $
                      lookup i e
                  let r' = (Just e', Nothing)
                  put (Morphism nr (r':er), g)
                  return ()
