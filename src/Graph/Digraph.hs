module Graph.Digraph where

import Data.IntMap

-- Edge idE (idSrc, idTgt) payload
data Edge a = Edge Int (Int, Int) a deriving (Show,Eq,Read)
-- Node idN payload
data Node a = Node Int a deriving (Show,Eq,Read)

data Digraph a b = Digraph (IntMap (Node a)) (IntMap (Edge b))

addNode :: Node a -> Digraph a b -> Maybe (Digraph a b)
addNode n@(Node id _) g@(Digraph nm em)
    | id `member` nm = Nothing
    | otherwise = Just $ Digraph (insert id n nm) em

addEdge :: Edge b -> Digraph a b -> Maybe (Digraph a b)
addEdge e@(Edge id _ _) (Digraph nm em)
    | id `member` em = Nothing
    | otherwise = Just $ Digraph nm (insert id e em)

delNode :: Node a -> Digraph a b -> Maybe (Digraph a b)
delNode n@(Node id _) (Digraph nm em)
    | id `member` nm = Just $ Digraph (delete id nm) em
    | otherwise = Nothing

delEdge :: Edge b -> Digraph a b -> Maybe (Digraph a b)
delEdge e@(Edge id _ _) (Digraph nm em) 
    | id `member` em = Just $ Digraph nm (delete id em)
    | otherwise = Nothing

nodes :: Digraph a b -> [Node a]
nodes (Digraph nm _) = toList nm

edges :: Digraph a b -> [Edge b]
edges (Digraph _ em) = toList em

conn :: Edge b -> Digraph a b -> Maybe (Node a, Node a)
conn e (Digraph nm em@(Edge id np))
    | id `member` em = Just $ np
    | otherwise = Nothing
    
