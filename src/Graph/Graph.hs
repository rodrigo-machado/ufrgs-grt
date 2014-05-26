{-# LANGUAGE TypeFamilies #-}
module Graph.Graph 
    ( Edge
    , Node
    , Graph
    , TGraph
    , TypedGraph (..)
    , Element (..)
    , numNodes
    , numEdges
    , empty
    , nullG
    , node
    , edge
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
    , edgePayload
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

data Graph a b = Graph (IntMap (Node a)) (IntMap (Edge b))
    deriving (Show, Read, Eq)

instance PrettyPrint a => PrettyPrint (IntMap a) where
    prettyPrint m =
        concat $ L.intersperse ", " $ IM.foldl (\s e -> prettyPrint e:s) [] m

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Graph a b) where
    prettyPrint (Graph n e) = unlines [prettyPrint n, prettyPrint e]

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (TypedGraph a b) where
    prettyPrint (TypedGraph g t) =
        unlines ["Type", prettyPrint t, "Graph", prettyPrint g]

nodes :: Graph a b -> [(Node a)]
nodes (Graph nm _) = map snd $ IM.toList nm

edges :: Graph a b -> [(Edge b)]
edges (Graph _ em) = map snd $ IM.toList em

data TypedGraph a b = TypedGraph (Graph a b) (TGraph a b)
        deriving (Show, Read, Eq)

numNodes :: Graph a b -> Int
numNodes (Graph nm _) = IM.size nm

numEdges :: Graph a b -> Int
numEdges (Graph _ em) = IM.size em

type TGraph a b = Graph a b

-- |Create an empty Graph
empty :: Graph a b
empty = Graph (IM.empty) (IM.empty)

nullG :: Graph a b -> Bool
nullG (Graph nm em) =
    IM.null nm && IM.null em

getNode :: Int -> Graph a b -> Maybe (Node a)
getNode i (Graph n _) = IM.lookup i n

getEdge :: Int -> Graph a b -> Maybe (Edge b)
getEdge i (Graph _ e) = IM.lookup i e

addNode :: (Monad m) => Node a -> Graph a b -> m (Graph a b)
addNode n@(Node id _ _) g@(Graph nm em)
    | id `IM.member` nm =
        fail $ "addNode: node " ++ show id ++ " already in digraph"
    | otherwise         = return $ Graph (IM.insert id n nm) em

addEdge :: (Monad m) => Edge b -> Graph a b -> m (Graph a b)
addEdge e@(Edge id s t _ _) g@(Graph nm em)
    | id `IM.member` em = 
        fail $ "addEdge: edge " ++ show id ++ " already in digraph"
    | s `IM.member` nm && t `IM.member` nm =
        return $ Graph nm (IM.insert id e em)
    | otherwise =
        fail $ "addEdge: edge points to nodes not found in digraph"

removeNode :: (Monad m) => Node a -> Graph a b -> m (Graph a b)
removeNode n@(Node id _ _) g@(Graph nm em)
    | id `IM.notMember` nm =
            fail $ "removeNode: node " ++ show id ++ " not in digraph"
    | IM.fold
        (\e acc -> acc || (sourceID e) == id || (targetID e) == id) False em =
            fail $ "removeNode: node " ++ show id
                 ++ " has some edge pointing to it"
    | otherwise = return $ Graph (IM.delete id nm) em

removeEdge :: (Monad m) => Edge b -> Graph a b -> m (Graph a b)
removeEdge e g@(Graph nm em) =
    if id `IM.member` em 
        then return $ Graph nm (IM.delete id em)
        else fail $ "removeEdge: edge " ++ show id ++ " not in digraph"
      where
        id = edgeID e

keepNode :: (Monad m) => Node a -> Graph a b -> m (Graph a b)
keepNode (Node nid _ _) g@(Graph ns es) = 
    if nid `IM.member` ns
        then return g
        else fail $ "keepNode: node " ++ show nid ++ " doesn't exist"

keepEdge :: (Monad m) => Edge b -> Graph a b -> m (Graph a b)
keepEdge e g@(Graph ns es)
    | id `IM.member` es = return g
    | otherwise         = fail $ "keepEdge: edge "
                                ++ show id ++ " doesn't exist"
      where
        id = edgeID e

findNode :: Int -> Graph a b -> Maybe (Node a)
findNode id (Graph nm _) = IM.lookup id nm


findNodeType :: Int -> Graph a b -> Maybe Int
findNodeType id (Graph nm em) =
    let n = IM.lookup id nm
        in case n of
            Nothing -> Nothing
            Just (Node _ tid _) -> Just tid

srcType :: Edge b -> Graph a b -> Maybe Int
srcType e l = findNodeType s l
    where
      s = sourceID e

tarType :: Edge b -> Graph a b -> Maybe Int
tarType e l = findNodeType t l
    where
      t = targetID e

{- if 'e' is an Edge, it's valid so findNode will never fail -}
source :: Edge b -> Graph a b -> Node a
source e d =
    let (Just n) = findNode (sourceID e) d
    in n
        

target :: Edge b -> Graph a b -> Node a
target e d =
    let (Just n ) = findNode (targetID e) d
    in n

hasEdge :: Graph a b -> Node a -> Bool
hasEdge g n =
    let nid   = nodeID n
        found = L.find (\e -> (sourceID e) == nid || (targetID e) == nid)
                       $ edges g
    in case found of
        Just _ -> True
        Nothing -> False

----------------------------------------------------------------------------
-- | Non-monadic interface
-- | default approach: keep original Graph unchanged when function "fails"

insNode :: Node a -> Graph a b -> Graph a b
insNode n@(Node id _ _) g@(Graph nm em)
    | id `IM.member` nm = g
    | otherwise         = Graph (IM.insert id n nm) em

insEdge :: Edge b -> Graph a b -> Graph a b
insEdge e@(Edge id s t _ _) g@(Graph nm em)
    | id `IM.member` em =  g
    | s `IM.member` nm && t `IM.member` nm =
        Graph nm (IM.insert id e em)
    | otherwise = g

delNode :: Int -> Graph a b -> Graph a b
delNode id g@(Graph nm em)
    | id `IM.notMember` nm = g
    | IM.fold 
        (\(Edge eid s t _ _) acc -> acc || s == id || t == id) 
        False em = g
    | otherwise = Graph (IM.delete id nm) em

delEdge :: Int -> Graph a b -> Graph a b
delEdge id g@(Graph nm em)
    | id `IM.member` em = Graph nm (IM.delete id em)
    | otherwise         = g

