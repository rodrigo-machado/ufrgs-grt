{-# LANGUAGE TypeFamilies #-}
module Graph.Digraph 
    ( Edge
    , Node
    , Digraph
    , TypedDigraph
    , numNodes
    , numEdges
    , Morphism (..)
    , EdgeAction (..)
    , empty
    , nullG
    , node
    , edge
    , Element (..)
    , addNode
    , addEdge
    , removeNode
    , removeEdge
    , keepNode
    , keepEdge
    , insNode
    , insEdge
    , delNode
    , delEdge
    , nodes
    , edges
    , findNode
    , source
    , target
    , hasEdge
    , nodeID
    , edgeID
    , sourceID
    , targetID
    , nodePayload
    , addNodeAction 
    , addEdgeAction 
    , applyActions
    , applyMorphism
    , applyTypedMorphism
    , TGraph
    , nodeType
    , edgeType
    , srcType
    , tarType
    ) where

import Control.Monad

import Data.IntMap (IntMap)
import qualified Data.IntMap        as IM
import qualified Data.List        as L
import Assorted.PrettyPrint


-- Edge idE (idSrc, idTgt) t payload
data Edge a = Edge {
      edgeID        :: Int
    , sourceID      :: Int
    , targetID      :: Int
    , edgeType      :: Int
    , edgePayload   :: a
} deriving (Show, Read)

edge :: Int -> (Int, Int) -> Int -> a -> Edge a
edge id (src, tgt) tp p = Edge id src tgt tp p

instance Eq (Edge a) where
    le == re = edgeID le == edgeID re

-- Node idN t payload
data Node a = Node {
      nodeID        :: Int
    , nodeType      :: Int
    , nodePayload   :: a
} deriving (Show, Read)

node :: Int -> Int -> a -> Node a
node id tp p = Node id tp p

instance Eq (Node a) where
    ln == rn = nodeID ln == nodeID rn
        
instance PrettyPrint a => PrettyPrint (Node a) where
    prettyPrint n = concat [nid, "(", ntype, ")"]
      where
        nid     = show $ nodeID n
        ntype   = show $ nodeType n

instance PrettyPrint a => PrettyPrint (Edge a) where
    prettyPrint e = concat [sid, "->", tid, " (", etype, ")"]
      where
        sid     = show $ sourceID e
        tid     = show $ targetID e
        etype   = show $ edgeType e

class Element a where
    type Payload a :: *
    payload :: a -> Payload a
    elemId :: a -> Int
    typeId :: a -> Int

instance Element (Node a) where
    type Payload (Node a) = a
    payload = nodePayload
    elemId = nodeID
    typeId = nodeType

instance Element (Edge a) where
    type Payload (Edge a) = a
    payload = edgePayload
    elemId = edgeID
    typeId = edgeType

data Digraph a b = Digraph (IntMap (Node a)) (IntMap (Edge b))
    deriving (Show, Read, Eq)

instance PrettyPrint a => PrettyPrint (IntMap a) where
    prettyPrint m =
        concat $ L.intersperse ", " $ IM.foldl (\s e -> prettyPrint e:s) [] m

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Digraph a b) where
    prettyPrint (Digraph n e) = unlines [prettyPrint n, prettyPrint e]

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (TypedDigraph a b) where
    prettyPrint (TypedDigraph g t) =
        unlines ["Type", prettyPrint t, "Graph", prettyPrint g]

nodes :: Digraph a b -> [(Node a)]
nodes (Digraph nm _) = map snd $ IM.toList nm

edges :: Digraph a b -> [(Edge b)]
edges (Digraph _ em) = map snd $ IM.toList em


data TypedDigraph a b = TypedDigraph (Digraph a b) (TGraph a b)
        deriving (Show, Read, Eq)


numNodes :: TypedDigraph a b -> Int
numNodes (TypedDigraph (Digraph nm _) _) = IM.size nm

numEdges :: TypedDigraph a b -> Int
numEdges (TypedDigraph (Digraph _ em) _) = IM.size em

type TGraph a b = Digraph a b

-- |Create an empty Digraph
empty :: Digraph a b
empty = Digraph (IM.empty) (IM.empty)

nullG :: Digraph a b -> Bool
nullG (Digraph nm em) =
    IM.null nm && IM.null em

getNode :: Int -> Digraph a b -> Maybe (Node a)
getNode i (Digraph n _) = IM.lookup i n

getEdge :: Int -> Digraph a b -> Maybe (Edge b)
getEdge i (Digraph _ e) = IM.lookup i e

addNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
addNode n@(Node id _ _) g@(Digraph nm em)
    | id `IM.member` nm =
        fail $ "addNode: node " ++ show id ++ " already in digraph"
    | otherwise         = return $ Digraph (IM.insert id n nm) em

addEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
addEdge e@(Edge id s t _ _) g@(Digraph nm em)
    | id `IM.member` em = 
        fail $ "addEdge: edge " ++ show id ++ " already in digraph"
    | s `IM.member` nm && t `IM.member` nm =
        return $ Digraph nm (IM.insert id e em)
    | otherwise =
        fail $ "addEdge: edge points to nodes not found in digraph"

removeNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
removeNode n@(Node id _ _) g@(Digraph nm em)
    | id `IM.notMember` nm =
            fail $ "removeNode: node " ++ show id ++ " not in digraph"
    | IM.fold
        (\e acc -> acc || (sourceID e) == id || (targetID e) == id) False em =
            fail $ "removeNode: node " ++ show id
                 ++ " has some edge pointing to it"
    | otherwise = return $ Digraph (IM.delete id nm) em

removeEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
removeEdge e g@(Digraph nm em) =
    if id `IM.member` em 
        then return $ Digraph nm (IM.delete id em)
        else fail $ "removeEdge: edge " ++ show id ++ " not in digraph"
      where
        id = edgeID e

keepNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
keepNode (Node nid _ _) g@(Digraph ns es) = 
    if nid `IM.member` ns
        then return g
        else fail $ "keepNode: node " ++ show nid ++ " doesn't exist"

keepEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
keepEdge e g@(Digraph ns es)
    | id `IM.member` es = return g
    | otherwise         = fail $ "keepEdge: edge "
                                ++ show id ++ " doesn't exist"
      where
        id = edgeID e

findNode :: Int -> Digraph a b -> Maybe (Node a)
findNode id (Digraph nm _) = IM.lookup id nm

{- if 'e' is an Edge, it's valid so findNode will never fail -}
source :: Edge b -> Digraph a b -> Node a
source e d =
    let (Just n) = findNode (sourceID e) d
    in n
        

target :: Edge b -> Digraph a b -> Node a
target e d =
    let (Just n ) = findNode (targetID e) d
    in n

hasEdge :: TypedDigraph a b -> Node a -> Bool
hasEdge (TypedDigraph dg _) n =
    let nid   = nodeID n
        found = L.find (\e -> (sourceID e) == nid || (targetID e) == nid)
                       $ edges dg
    in case found of
        Just _ -> True
        Nothing -> False

----------------------------------------------------------------------------
-- | Non-monadic interface
-- | default approach: keep original Graph unchanged when function "fails"

insNode :: Node a -> Digraph a b -> Digraph a b
insNode n@(Node id _ _) g@(Digraph nm em)
    | id `IM.member` nm = g
    | otherwise         = Digraph (IM.insert id n nm) em

insEdge :: Edge b -> Digraph a b -> Digraph a b
insEdge e@(Edge id s t _ _) g@(Digraph nm em)
    | id `IM.member` em =  g
    | s `IM.member` nm && t `IM.member` nm =
        Digraph nm (IM.insert id e em)
    | otherwise = g

delNode :: Node a -> Digraph a b -> Digraph a b
delNode n@(Node id _ _) g@(Digraph nm em)
    | id `IM.notMember` nm = g
    | IM.fold 
        (\(Edge eid s t _ _) acc -> acc || s == id || t == id) 
        False em = g
    | otherwise = Digraph (IM.delete id nm) em

delEdge :: Edge b -> Digraph a b -> Digraph a b
delEdge e g@(Digraph nm em)
    | id `IM.member` em = Digraph nm (IM.delete id em)
    | otherwise         = g
      where
        id = edgeID e

----------------------------------------------------------------------------
-- | Morphism related functions

type NodeAction a = (Maybe (Node a), Maybe (Node a))
type EdgeAction a = (Maybe (Edge a), Maybe (Edge a))

data Morphism a b = Morphism [NodeAction a] [EdgeAction b] deriving (Show,Read)

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Morphism a b) where
    prettyPrint (Morphism a b) = unlines [prettyPrint a, ";", prettyPrint b]

showMorphism (Morphism nal eal) = 
    "\nNode mappings:\n"
    ++ (L.foldr (\s acc -> (showNodeAction s) ++ acc) [] nal)
    ++ "\nEdge mappings:\n"
    ++ (L.foldr (\s acc -> (showEdgeAction s) ++ acc) [] eal)
    ++ "\n"

showNodeAction :: (Show a) => NodeAction a -> String
showNodeAction na = case na of
    (Nothing, Nothing) -> "__ ==> __\n"
    (Just (Node id _ _), Nothing)-> show id ++ " ==> __\n"
    (Nothing, Just (Node id _ _)) -> show id ++ " ==> __\n"
    (Just (Node lid _ _), Just (Node gid _ _)) ->
        show lid ++ " ==> " ++ show gid ++ "\n"

showEdgeAction :: (Show a) => EdgeAction a -> String
showEdgeAction na = case na of
    (Nothing, Nothing) -> "__ ==> __"
    (Just (Edge _ _ _ t p), Nothing)-> show (t, p) ++ " ==> __"
    (Nothing, Just (Edge _ _ _ t p)) -> show (t, p) ++ " __ ==> p"
    (Just (Edge _ _ _ lt lp), Just (Edge _ _ _ gt gp)) ->
        show (lt, lp) ++ " ==> " ++ show (gt, gp) ++ "\n"


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


nodeAction :: (Monad m, Eq a) => NodeAction a -> (Digraph a b -> m (Digraph a b))
nodeAction (Nothing, Just n) = addNode n
nodeAction (Just n, Nothing) = removeNode n 
nodeAction (Just n, Just n')
    | n /= n'   = const $ fail "Node transformation is unhandled"
    | otherwise = keepNode n
nodeAction (Nothing, Nothing) = return

edgeAction :: (Monad m, Eq b) => EdgeAction b -> (Digraph a b -> m (Digraph a b))
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

actionSet :: (Monad m, Eq a, Eq b) => Morphism a b -> [Digraph a b -> m (Digraph a b)]
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

applyMorphism :: (Monad m, Eq a, Eq b) => Morphism a b -> Digraph a b -> m (Digraph a b)
applyMorphism m g = applyActions g $ actionSet m

applyTypedMorphism :: (Monad m, Eq a, Eq b) => Morphism a b -> TypedDigraph a b -> m (TypedDigraph a b)
applyTypedMorphism m (TypedDigraph g t) = liftM (flip TypedDigraph t) $ applyMorphism m g

findNodeType :: Int -> TypedDigraph a b -> Maybe Int
findNodeType id td@(TypedDigraph (Digraph nm em) _) =
    let n = IM.lookup id nm
        in case n of
            Nothing -> Nothing
            Just (Node _ tid _) -> Just tid

srcType :: Edge b -> TypedDigraph a b -> Maybe Int
srcType e l = findNodeType s l
    where
      s = sourceID e

tarType :: Edge b -> TypedDigraph a b -> Maybe Int
tarType e l = findNodeType t l
    where
      t = targetID e
