{-# LANGUAGE TypeFamilies #-}
module Graph.Graph 
    ( Graph ) where

import Graph.GraphInterface
import qualified Data.List as L
import qualified Data.List.Utils as L
import Data.Maybe

data Node a = Node { nodePayload :: Maybe a
                   , nodeType    :: Maybe Int
              } deriving (Eq, Show, Read)

data Edge a = Edge { sourceOf    :: Int
                   , targetOf    :: Int
                   , edgePayload :: Maybe a
                   , edgeType    :: Maybe Int
              } deriving (Eq, Show, Read)

data Graph a b = Graph [(Int, Node a)] [(Int, Edge b)]
    deriving (Show, Read, Eq)

instance GraphClass (Graph a b) where
    type Nd (Graph a b) = Int
    type Ed (Graph a b) = Int

  -- Build and modify graphs
    empty = Graph [] []

    insertNode n g@(Graph ns es)
        | n `elem` (L.keysAL ns) = g
        | otherwise   = Graph (L.addToAL ns n (Node Nothing Nothing)) es

    insertEdge e src tgt g@(Graph ns es)
        | e `elem` (L.keysAL es) =  g
        | src `elem` (L.keysAL ns) && tgt `elem` (L.keysAL ns) =
            Graph ns (L.addToAL es e (Edge src tgt Nothing Nothing))
        | otherwise = g

    removeNode n g@(Graph ns es)
        | null $ incidentEdges n g = Graph (L.delFromAL ns n) es
        | otherwise = g

    removeEdge e (Graph ns es) = Graph ns (L.delFromAL es e)
        
    nodes (Graph ns _) = L.keysAL ns        
    edges (Graph _ es) = L.keysAL es        
    nodesConnectedTo e g@(Graph _ es) =
        let ed = L.lookup e es
        in case ed of
            Just (Edge src tgt _ _) -> Just (src, tgt)
            otherwise -> Nothing

instance TypedGraphClass (Graph a b) where
    getTypeOfNode n (Graph ns _) =
        let found = L.lookup n ns
        in case found of
            Just nd   -> nodeType nd
            otherwise -> Nothing
    setTypeOfNode tn n (Graph ns es) =
        let found = L.lookup n ns
        in case found of
            Just (Node p _) -> Graph (L.addToAL ns n (Node p (Just tn))) es
            otherwise -> Graph ns es

    getTypeOfEdge e (Graph _ es) =
        let found = L.lookup e es
        in case found of
            Just ed   -> edgeType ed
            otherwise -> Nothing
    setTypeOfEdge te e (Graph ns es) =
        let found = L.lookup e es
        in case found of
            Just (Edge s t p _) -> Graph ns (L.addToAL es e (Edge s t p (Just te)))
            otherwise -> Graph ns es
