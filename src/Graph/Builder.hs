module Graph.Builder {-( GraphBuilder
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
                     )-} where

import Prelude hiding (null,lookup,member)

import Control.Monad.State

import Data.IntMap hiding (empty,map,filter,(\\))
import Data.Maybe
import Data.List hiding (lookup,null)

import Graph.Digraph
import Graph.Rewriting

import Assorted.Maybe

type GraphBuilder a b = StateT (TypedDigraph a b)

buildGraph :: Monad m => GraphBuilder a b m r -> m (TypedDigraph a b)
buildGraph = buildGraphFrom (TypedDigraph empty empty)

buildGraphFrom :: Monad m => TypedDigraph a b -> GraphBuilder a b m c -> m (TypedDigraph a b)
buildGraphFrom g = liftM snd . flip runStateT g

nextId m = if null m then 0 else 1 + fst (findMax m)

typeNode :: (Monad m) => a -> GraphBuilder a b m Int
typeNode p = do (TypedDigraph g t@(Digraph n e)) <- get
                let k = nextId n
                t' <- addNode (Node k k p) t
                put $ TypedDigraph g t'
                return k

typeEdge :: Monad m => b -> (Int, Int) -> GraphBuilder a b m Int
typeEdge p c = do (TypedDigraph g t@(Digraph n e)) <- get
                  let k = nextId e
                  t' <- flip addEdge t $ Edge k c k p
                  put $ TypedDigraph g t'
                  return k

guardKey t m = unless (member t m) $ fail $ unwords ["Key", show t, "not in map"]

graphNode :: (Monad m) => a -> Int -> GraphBuilder a b m Int
graphNode p t = do (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
                   guardKey t tn
                   let k = nextId gn
                   g' <- flip addNode g $ Node k t p
                   put $ TypedDigraph g' tg
                   return k

graphEdge :: Monad m => b -> Int -> (Int, Int) -> GraphBuilder a b m Int
graphEdge p t c = do (TypedDigraph g@(Digraph gn ge) tg@(Digraph tn te)) <- get
                     guardKey t te
                     let k = nextId ge
                     g' <- flip addEdge g $ Edge k c t p
                     put $ TypedDigraph g' tg
                     return k

deleteTypeNode :: Monad m => Int -> GraphBuilder a b m ()
deleteTypeNode i = do (TypedDigraph g t) <- get
                      n <- liftMaybe $ node i t
                      t' <- removeNode n t
                      put $ TypedDigraph g t'
                      return ()


deleteTypeEdge :: Monad m => Int -> GraphBuilder a b m ()
deleteTypeEdge i = do (TypedDigraph g t) <- get
                      e <- liftMaybe $ edge i t
                      t' <- removeEdge e t
                      put $ TypedDigraph g t'
                      return ()

deleteGraphNode :: Monad m => Int -> GraphBuilder a b m ()
deleteGraphNode i = do (TypedDigraph g t) <- get
                       n <- liftMaybe $ node i g
                       g' <- removeNode n g
                       put $ TypedDigraph g' t
                       return ()

deleteGraphEdge :: Monad m => Int -> GraphBuilder a b m ()
deleteGraphEdge i = do (TypedDigraph g t) <- get
                       e <- liftMaybe $ edge i g
                       g' <- removeEdge e g
                       put $ TypedDigraph g' t
                       return ()

type RuleBuilder a b = StateT (Rule a b, TypedDigraph a b)

-- Must also create keep rules for all nodes and edges that are not
-- created or deleted
buildRuleFrom :: Monad m => Rule a b -> TypedDigraph a b -> RuleBuilder a b m c -> m (Rule a b)
buildRuleFrom r g rb = liftM (fst . snd) $ runStateT (keepUnmapped rb) (r, g)

buildRule :: Monad m => TypedDigraph a b -> RuleBuilder a b m c -> m (Rule a b)
buildRule g = buildRuleFrom (Morphism [] []) g

createNode :: Monad m => a -> Int -> RuleBuilder a b m Int
createNode p t = do (r, g) <- get
                    let (TypedDigraph g' t') = g
                        (Digraph n e) = g'
                        (Digraph tn te) = t'
                        (Morphism nr er) = r
                        k = nextId n
                    guardKey t tn
                    let r' = (Nothing, Just $ Node k t p)
                    put (Morphism (r':nr) er, g)
                    return k

deleteNode :: Monad m => Int -> RuleBuilder a b m ()
deleteNode i = do (r, g) <- get
                  let (TypedDigraph g' t') = g
                      (Digraph n e) = g'
                      (Morphism nr er) = r
                  n' <- liftMaybeMsg (unwords $ ["Node", show i, "not in graph"]) $
                      lookup i n
                  let r' = (Just n', Nothing)
                  put (Morphism (r':nr) er, g)
                  return ()

createEdge :: Monad m => b -> Int -> (Int, Int) -> RuleBuilder a b m Int
createEdge p t c = do (r, g) <- get
                      let (TypedDigraph g' t') = g
                          (Digraph n e) = g'
                          (Digraph tn te) = t'
                          (Morphism nr er) = r
                          k = nextId e
                      guardKey t te
                      let r' = (Nothing, Just $ Edge k c t p)
                      put (Morphism nr (r':er), g)
                      return k

deleteEdge :: Monad m => Int -> RuleBuilder a b m ()
deleteEdge i = do (r, g) <- get
                  let (TypedDigraph g' t') = g
                      (Digraph n e) = g'
                      (Morphism nr er) = r
                  e' <- liftMaybeMsg (unwords $ ["Edge", show i, "not in graph"]) $
                      lookup i e
                  let r' = (Just e', Nothing)
                  put (Morphism nr (r':er), g)
                  return ()

keepUnmapped :: Monad m => RuleBuilder a b m c -> RuleBuilder a b m ()
keepUnmapped rb = do rb
                     (r, g) <- get
                     let (TypedDigraph g' t) = g
                         (Digraph n e) = g'
                         (Morphism nr er) = r
                         rNodeIDs = map (nodeID . fromJust . fst) $ filter ((/=Nothing) . fst) nr
                         rEdgeIDs = map (edgeID . fromJust . fst) $ filter ((/=Nothing) . fst) er
                         kn = (keys n) \\ rNodeIDs
                         ke = (keys e) \\ rEdgeIDs
                         keepNode m x = (Just $ fromJust $ lookup x m, Just $ fromJust $ lookup x m)
                         lift ks m = map (keepNode m) ks
                         nrn = lift kn n ++ nr
                         ern = lift ke e ++ er
                     put $ (Morphism nrn ern, g)

                     return ()
