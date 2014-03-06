module Graph.Graph
	( Edge (..)
	, Node (..)
	, Graph (..)
	, TypedGraph (..)
	, empty
	, node
	, edge
	, addNode
	, addEdge
	, removeNode
	, removeEdge
	, keepNode
	, keepEdge
	, nodes
	, edges
	, findNode
	, source
	, target
	, nodeID
	, edgeID
	, sourceID
	, targetID
	, Graph (..)
	, TGraph
	, nodeType
	, edgeType
	, srcType
	, tarType
	) where

import Control.Monad

import Data.IntMap (IntMap)
import qualified Data.IntMap	as IM
import qualified Data.List	as L
--import Assorted.PrettyPrint

-- Edge idE (idSrc, idTgt) t payload
data Edge a = Edge Int (Int, Int) Int a deriving (Show, Read)
instance Eq (Edge a) where
	Edge lid _ _ _ == Edge gid _ _ _ = lid == gid

-- Node idN t payload
data Node a = Node Int Int a deriving (Show, Read)
instance Eq (Node a) where
	Node lid _ _ == Node gid _ _ = lid == gid
	
data Graph a b = Graph (IntMap (Node a)) (IntMap (Edge b)) deriving (Show, Read, Eq)

{-
instance PrettyPrint a => PrettyPrint (Node a) where
    prettyPrint (Node i t p) = concat [show i, "(", show t, ")"]

instance PrettyPrint a => PrettyPrint (Edge a) where
    prettyPrint (Edge i (s, t) tp p) = concat [show s, "->", show t, " (", show tp, ")"]


instance PrettyPrint a => PrettyPrint (IntMap a) where
    prettyPrint m = concat $ L.intersperse ", " $ IM.foldl (\s e -> prettyPrint e:s) [] m

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Graph a b) where
    prettyPrint (Graph n e) = unlines [prettyPrint n, prettyPrint e]

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (TypedGraph a b) where
    prettyPrint (TypedGraph g t) = unlines ["Type", prettyPrint t, "Graph", prettyPrint g]
-}

data TypedGraph a b = TypedGraph (Graph a b) (TGraph a b)
	deriving (Show, Read, Eq)

type TGraph a b = Graph a b

empty :: Graph a b
empty = Graph (IM.empty) (IM.empty)

node :: Int -> Graph a b -> Maybe (Node a)
node i (Graph n _) = IM.lookup i n

edge :: Int -> Graph a b -> Maybe (Edge b)
edge i (Graph _ e) = IM.lookup i e

addNode :: Node a -> Graph a b -> Graph a b
addNode n@(Node id _ _) g@(Graph nm em) =
	if id `IM.member` nm 
		then g
		else Graph (IM.insert id n nm) em

keepNode :: Node a -> Graph a b -> Graph a b
keepNode _ g =
	g

keepEdge :: Edge b -> Graph a b -> Graph a b
keepEdge _ g =
	g
	
addEdge :: Edge b -> Graph a b -> Graph a b
addEdge e@(Edge id (s, t) _ _) g@(Graph nm em)
	| id `IM.member` em = 
		g
	| s `IM.member` nm && t `IM.member` nm =
		Graph nm (IM.insert id e em)
	| otherwise =
		g

removeNode :: Node a -> Graph a b -> Graph a b
removeNode n@(Node id _ _) g@(Graph nm em)
	| id `IM.notMember` nm =
		g
	| IM.fold 
		(\(Edge eid (s, t) _ _) acc -> acc || s == id || t == id) 
		False em =
		g
	| otherwise =
		Graph (IM.delete id nm) em

removeEdge :: Edge b -> Graph a b -> Graph a b
removeEdge e@(Edge id _ _ _) g@(Graph nm em) =
	if id `IM.member` em 
		then Graph nm (IM.delete id em)
		else g

findNode :: Int -> Graph a b -> Maybe (Node a)
findNode id (Graph nm _) =
	IM.lookup id nm

nodes :: Graph a b -> [(Node a)]
nodes (Graph nm _) = map snd $ IM.toList nm

edges :: Graph a b -> [(Edge b)]
edges (Graph _ em) = map snd $ IM.toList em

{- if 'e' is an Edge, it's valid so findNode will never fail -}
source :: Edge b -> Graph a b -> Node a
source e d =
	let (Just n) = findNode (sourceID e) d
	in n
	

target :: Edge b -> Graph a b -> Node a
target e d =
	let (Just n ) = findNode (targetID e) d
	in n

nodeID :: Node a -> Int
nodeID (Node id _ _) = id

edgeID :: Edge a -> Int
edgeID (Edge id _ _ _) = id

sourceID :: Edge b -> Int
sourceID (Edge _ (src, _) _ _) = src

targetID :: Edge b -> Int
targetID (Edge _ (_, tar) _ _) = tar


nodeType :: Node a -> Int
nodeType (Node _ t _) = t

edgeType :: Edge a -> Int
edgeType (Edge _ _ t _) = t

findNodeType :: Int -> TypedGraph a b -> Maybe Int
findNodeType id td@(TypedGraph (Graph nm em) _) =
	let n = IM.lookup id nm
	in case n of
		Nothing -> Nothing
		Just (Node _ tid _) -> Just tid

srcType :: Edge b -> TypedGraph a b -> Maybe Int
srcType (Edge _ (s, _) _ _) l =
	findNodeType s l

tarType :: Edge b -> TypedGraph a b -> Maybe Int
tarType (Edge _ (_, t) _ _) l =
	findNodeType t l
