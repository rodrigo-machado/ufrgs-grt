module Graph.Morphism2
    ( NodeAction
    , EdgeAction
    , Morphism
    , Rule
    , nodeActions
    , edgeActions
    , addNodeAction
    , addEdgeAction
    , applyActions
    , applyMorphism
    , applyTypedMorphism
    , emptyRule
    ) where

import Control.Monad
import qualified Data.List as L
import Assorted.PrettyPrint
import Graph.Graph

type NodeAction a = (Maybe (Node a), Maybe (Node a))
type EdgeAction a = (Maybe (Edge a), Maybe (Edge a))

data Morphism a b = Morphism {
      nodeActions         :: [NodeAction a]
    , edgeActions         :: [EdgeAction b]
} deriving (Show,Read)

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Morphism a b) where
    prettyPrint (Morphism a b) = unlines [prettyPrint a, ";", prettyPrint b]

type Rule a b = Morphism a b
emptyRule = Morphism [] []

showMorphism (Morphism nal eal) = 
    "\nNode mappings:\n"
    ++ (L.foldr (\s acc -> (showNodeAction s) ++ acc) [] nal)
    ++ "\nEdge mappings:\n"
    ++ (L.foldr (\s acc -> (showEdgeAction s) ++ acc) [] eal)
    ++ "\n"

showNodeAction :: (Show a) => NodeAction a -> String
showNodeAction na = case na of
    (Nothing, Nothing) -> "__ ==> __\n"
    (Just n, Nothing) -> show (nodeID n) ++ " ==> __\n"
    (Nothing, Just n) -> show (nodeID n) ++ " ==> __\n"
    (Just ln, Just gn) ->
        show (nodeID ln) ++ " ==> " ++ show (nodeID gn) ++ "\n"

showEdgeAction :: (Show a) => EdgeAction a -> String
showEdgeAction na = case na of
    (Nothing, Nothing) -> "__ ==> __"
    (Just e, Nothing)-> show (edgeType e, edgePayload e) ++ " ==> __"
    (Nothing, Just e) -> show (edgeType e, edgePayload e) ++ " __ ==> p"
    (Just le, Just ge) ->
        show (edgeType le, edgePayload le) ++ " ==> "
             ++ show (edgeType ge, edgePayload ge) ++ "\n"


-- | Checks if NodeAction already exists. If so, adds it. Otherwise, returns
-- the original Morphism
addNodeAction :: Node a -> Node a -> Morphism a b -> Morphism a b
addNodeAction ln rn m@(Morphism nal eal)
    | (Just ln, Just rn) `L.notElem` nal =
        Morphism ((Just ln, Just rn) : nal) eal
    | otherwise = m

                
addEdgeAction :: Edge b -> Edge b -> Morphism a b -> Morphism a b
addEdgeAction le re m@(Morphism nal eal) =
    Morphism nal ((Just le, Just re) : eal)


nodeAction :: (Monad m, Eq a) => NodeAction a -> (Graph a b -> m (Graph a b))
nodeAction (Nothing, Just n) = addNode n
nodeAction (Just n, Nothing) = removeNode n 
nodeAction (Just n, Just n')
    | n /= n'   = const $ fail "Node transformation is unhandled"
    | otherwise = keepNode n
nodeAction (Nothing, Nothing) = return

edgeAction :: (Monad m, Eq b) => EdgeAction b -> (Graph a b -> m (Graph a b))
edgeAction (Nothing, Just e) = addEdge e
edgeAction (Just e, Nothing) = removeEdge e
edgeAction (Just e, Just e')
    | e /= e'    = const $ fail "Edge transformation is unhandled"
    | otherwise  = keepEdge e
edgeAction (Nothing, Nothing) = return

addAction (Nothing, Just t) = True
addAction _ = False

removeAction (Just s, Nothing) = True
removeAction _ = False

keepAction (Just s, Just t) = True
keepAction _ = False

actionSet :: (Monad m, Eq a, Eq b) => Morphism a b -> [Graph a b -> m (Graph a b)]
actionSet (Morphism na ea) = let
    nodeActions f = map nodeAction . filter f
    edgeActions f = map edgeAction . filter f
    knSet = nodeActions keepAction na
    keSet = edgeActions keepAction ea
    anSet = nodeActions addAction na
    aeSet = edgeActions addAction ea
    dnSet = nodeActions removeAction na
    deSet = edgeActions removeAction ea
    in deSet ++ dnSet ++ knSet ++ keSet ++ anSet ++ aeSet

applyActions :: Monad m => a -> [a -> m a] -> m a
applyActions = foldM (\g f -> f g)

applyMorphism :: (Monad m, Eq a, Eq b) => Morphism a b -> Graph a b -> m (Graph a b)
applyMorphism m g = applyActions g $ actionSet m

applyTypedMorphism :: (Monad m, Eq a, Eq b) => Morphism a b -> TypedGraph a b -> m (TypedGraph a b)
applyTypedMorphism m (TypedGraph g t) = liftM (flip TypedGraph t) $ applyMorphism m g

