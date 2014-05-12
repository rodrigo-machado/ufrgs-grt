module Graph.Builder.Instance ( InstanceBuilder
                              , Target (..)
                              , Elem (..)
                              , RuleOp (..)
                              , build
                              , newRule
                              , newNode
                              , newEdge
                              , addRuleNode
                              , addRuleEdge
                              , getGraph
                              , putGraph
                              , getRules
                              , putRules
                              , getType
                              , putType
                              , getCurrentRule
                              , setCurrentRule
                              , getCurrentRuleId
                              , setCurrentRuleId
                              , setRuleOperation
                              ) where

import Data.List
import Data.Maybe
import Data.IntMap (keys)
import qualified Data.IntMap as M

import Control.Monad.State
import Control.Monad.IO.Class

import Graph.Digraph 
import Graph.Rewriting

type St a b = (Digraph a b, Digraph a b, [(Int, Rule a b)], Int)
type InstanceBuilder a b = StateT (St a b)

data Target = Inst -- Instance graph
            | Type -- Type graph
            | Rule -- Rule span

data RuleOp = Preserve -- Element will be preserved
            | Delete   -- Element will be deleted
            | Create   -- Element will be created

data Elem = N -- Acts on Nodes
          | E -- Acts on Edges

type TypeId = Int

-- | Returns the current graph
getGraph :: Monad m => InstanceBuilder a b m (Digraph a b)
getGraph = do (g, t, rs, r) <- get
              return g

-- | Sets the current graph
putGraph :: Monad m => Digraph a b -> InstanceBuilder a b m ()
putGraph g = do (_, t, rs, r) <- get
                put $ (g, t, rs, r)

-- | Gets the current type graph
getType :: Monad m => InstanceBuilder a b m (Digraph a b)
getType = do (g, t, rs, r) <- get
             return t

-- | Saves the current type graph
putType :: Monad m => Digraph a b -> InstanceBuilder a b m ()
putType t = do (g, _, rs, r) <- get
               put $ (g, t, rs, r)

-- | Get the current list of rules
getRules :: Monad m => InstanceBuilder a b m [(Int, Rule a b)]
getRules = do (g, t, rs, r) <- get
              return rs

-- | Puts a new list of rules
putRules :: Monad m => [(Int, Rule a b)] -> InstanceBuilder a b m ()
putRules rs = do (g, t, _, r) <- get
                 put $ (g, t, rs, r)

-- | Gets the id of the currently selected rule
getCurrentRuleId :: Monad m => InstanceBuilder a b m Int
getCurrentRuleId = do (g, t, rs, r) <- get
                      return r

-- | Selects a new rule id
setCurrentRuleId :: Monad m => Int -> InstanceBuilder a b m ()
setCurrentRuleId r = do (g, t, rs, _) <- get
                        put $ (g, t, rs, r)

-- | Gets the currently selected rule
getCurrentRule :: Monad m => InstanceBuilder a b m (Rule a b)
getCurrentRule = do rs <- getRules
                    i <- getCurrentRuleId
                    return $ fromJust $ lookup i rs

-- | Puts a new rule on the current id
setCurrentRule :: Monad m => Rule a b -> InstanceBuilder a b m ()
setCurrentRule r = do rs <- getRules
                      i <- getCurrentRuleId
                      let rs' = filter ((/= i) . fst) rs
                      putRules $ (i, r):rs'

-- | Builds the instance
build :: Monad m => InstanceBuilder a b m r -> m (TypedDigraph a b, [Rule a b])
build i = do
    (g, t, rs, _) <- liftM snd $ runStateT i (empty, empty, [], -1)
    return (TypedDigraph g t, map snd rs)

-- | Creates a new rule, returns the id
newRule :: Monad m => InstanceBuilder a b m Int
newRule = do rs <- getRules
             let newId = length rs
             putRules $ (newId, Morphism [] []):rs
             return $ newId

-- | Deletes the selected rule.
deleteRule :: Monad m => Int -> InstanceBuilder a b m ()
deleteRule i = do rs <- getRules
                  let rs' = filter ((/= i) . fst) rs
                  putRules rs'

-- | Checks if the graph and all rules are consistent, calls fail if not.
checkConsistency :: Monad m => InstanceBuilder a b m ()
checkConsistency = undefined

-- | Creates a new node on the selected target
newNode :: Monad m => Target -> TypeId -> a -> InstanceBuilder a b m Int
newNode Rule t p = do (Morphism ns es) <- getCurrentRule
                      let ks = (map nodeID $ catMaybes $ map fst ns)
                          newNode = (Just $ Node (newId ks) t p)
                          newRule = (newNode, newNode)
                      setCurrentRule $ Morphism (newRule:ns) es
                      return $ newId ks

newNode Type t p =  do (Digraph ns es) <- getType
                       let ks = keys ns
                           newNode = Node (newId ks) (newId ks) p
                           newGraph = Digraph (M.insert (newId ks) newNode ns) es
                       putType newGraph
                       return $ newId ks -- thank you, lazyness
                       
newNode Inst t p =  do (Digraph ns es) <- getGraph
                       let ks = keys ns
                           newNode = Node (newId ks) t p
                           newGraph = Digraph (M.insert (newId ks) newNode ns) es
                       putGraph newGraph
                       return $ newId ks -- thank you, lazyness

-- | Creates a new edge on the selected target
newEdge :: Monad m => Target -> TypeId -> (Int, Int) -> b -> InstanceBuilder a b m Int
newEdge Inst t c p = do (Digraph ns es) <- getGraph
                        let ks = keys es
                            newNode = Edge (newId ks) c t p
                            newGraph = Digraph ns (M.insert (newId ks) newNode es)
                        putGraph newGraph
                        return $ newId ks -- thank you, lazyness

newEdge Type t c p = do (Digraph ns es) <- getType
                        let ks = keys es
                            newNode = Edge (newId ks) c (newId ks) p
                            newGraph = Digraph ns (M.insert (newId ks) newNode es)
                        putType newGraph
                        return $ newId ks -- thank you, lazyness

newEdge Rule t c p = do (Morphism ns es) <- getCurrentRule
                        let kext f = map edgeID $ catMaybes $ map f es
                            ks =  kext fst `union` kext snd
                            newEdge = (Just $ Edge (newId ks) c t p)
                            newRule = (newEdge, newEdge)
                        setCurrentRule $ Morphism ns (newRule:es)
                        return $ newId ks

addRuleNode :: Monad m => RuleOp -> TypeId -> a -> InstanceBuilder a b m Int
addRuleNode o t p = do nid <- newNode Rule t p
                       setRuleOperation o N nid
                       return nid

addRuleEdge :: Monad m => RuleOp -> TypeId -> (Int, Int) -> b -> InstanceBuilder a b m Int
addRuleEdge o t c p = do eid <- newEdge Rule t c p
                         setRuleOperation o E eid
                         return eid

-- | Sets the operation on the selected rule node.
setRuleOperation :: Monad m => RuleOp -> Elem -> Int -> InstanceBuilder a b m ()
setRuleOperation o N i = do (Morphism ns es) <- getCurrentRule
                            let rl = filter (not . selectAction (byElementId i)) ns
                                [rc] = filter (selectAction (byElementId i)) ns
                            setCurrentRule $ Morphism (ruleOP o rc:rl) es

setRuleOperation o E i = do (Morphism ns es) <- getCurrentRule
                            let rl = filter (not . selectAction (byElementId i)) es
                                [rc] = filter (selectAction (byElementId i)) es
                            setCurrentRule $ Morphism ns (ruleOP o rc:rl)

-- Internal functions
byElementId :: Element a => Int -> Maybe a -> Bool
byElementId _ Nothing  = False
byElementId i (Just x) = elemId x == i

selectAction :: Element a => (Maybe a -> Bool) -> (Maybe a, Maybe a) -> Bool
selectAction f (x, y) = f x || f y

ruleOP :: RuleOp -> (Maybe a, Maybe a) -> (Maybe a, Maybe a)
ruleOP Preserve (Just x, Nothing) = (Just x, Just x)
ruleOP Preserve (Nothing, Just x) = (Just x, Just x)
ruleOP Preserve x                 = x

ruleOP Delete   (_, Just x)       = (Just x, Nothing)
ruleOP Delete   x                 = x

ruleOP Create   (Just x, _)       = (Nothing, Just x)
ruleOP Create   x                 = x


ids :: [Int]
ids = [1..]

newId :: [Int] -> Int
newId = head . (ids \\)
