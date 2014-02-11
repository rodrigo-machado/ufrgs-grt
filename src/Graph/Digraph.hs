module Graph.Digraph 
	( Edge (..)
	, Node (..)
	, Digraph (..)
	, Morphism (..)
	, EdgeAction (..)
	, empty
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
	, addNodeAction 
	, addEdgeAction 
	, applyActions
	, applyMorphism
	, applyTypedMorphism
	, TypedDigraph (..)
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


-- Edge idE (idSrc, idTgt) t payload
data Edge a = Edge Int (Int, Int) Int a deriving (Show, Read)
instance Eq (Edge a) where
	Edge lid _ _ _ == Edge gid _ _ _ = lid == gid

-- Node idN t payload
data Node a = Node Int Int a deriving (Show, Read)
instance Eq (Node a) where
	Node lid _ _ == Node gid _ _ = lid == gid
	

data Digraph a b = Digraph (IntMap (Node a)) (IntMap (Edge b)) deriving (Show, Read)

data TypedDigraph a b = TypedDigraph (Digraph a b) (TGraph a b)
	deriving (Show, Read)

type TGraph a b = Digraph a b

empty :: Digraph a b
empty = Digraph (IM.empty) (IM.empty)

addNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
addNode n@(Node id _ _) g@(Digraph nm em) =
	if id `IM.member` nm 
		then fail $ "addNode: node " ++ show id ++ " already in digraph"
		else return $ Digraph (IM.insert id n nm) em

addEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
addEdge e@(Edge id (s, t) _ _) g@(Digraph nm em)
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
		(\(Edge eid (s, t) _ _) acc -> acc || s == id || t == id) 
		False em =
		fail $ "removeNode: node " ++ show id ++ " has some edge pointing to it"
	| otherwise =
		return $ Digraph (IM.delete id nm) em

removeEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
removeEdge e@(Edge id _ _ _) g@(Digraph nm em) =
	if id `IM.member` em 
		then return $ Digraph nm (IM.delete id em)
		else fail $ "removeEdge: edge " ++ show id ++ " not in digraph"

keepNode :: (Monad m) => Node a -> Digraph a b -> m (Digraph a b)
keepNode (Node nid _ _) g@(Digraph ns es) = 
	if nid `IM.member` ns
		then return g
		else fail $ "keepNode: node " ++ show nid ++ " doesn't exist"

keepEdge :: (Monad m) => Edge b -> Digraph a b -> m (Digraph a b)
keepEdge (Edge eid _ _ _) g@(Digraph ns es) = 
	if eid `IM.member` es
		then return g
		else fail $ "keepEdge: edge " ++ show eid ++ " doesn't exist"

findNode :: Int -> Digraph a b -> Maybe (Node a)
findNode id (Digraph nm _) =
	IM.lookup id nm

nodes :: Digraph a b -> [(Node a)]
nodes (Digraph nm _) = map snd $ IM.toList nm

edges :: Digraph a b -> [(Edge b)]
edges (Digraph _ em) = map snd $ IM.toList em

{- if 'e' is an Edge, it's valid so findNode will never fail -}
source :: Edge b -> Digraph a b -> Node a
source e d =
	let (Just n) = findNode (sourceID e) d
	in n
	

target :: Edge b -> Digraph a b -> Node a
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

----------------------------------------------------------------------------
-- | Morphism related functions

type NodeAction a = (Maybe (Node a), Maybe (Node a))
type EdgeAction a = (Maybe (Edge a), Maybe (Edge a))

data Morphism a b = Morphism [NodeAction a] [EdgeAction b]
instance (Show a, Show b) => Show (Morphism a b) where
	show (Morphism nal eal) = 
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
	(Just (Node lid _ _), Just (Node gid _ _))
		-> show lid ++ " ==> " ++ show gid ++ "\n"

showEdgeAction :: (Show a) => EdgeAction a -> String
showEdgeAction na = case na of
	(Nothing, Nothing) -> "__ ==> __"
	(Just (Edge _ _ t p), Nothing)-> show (t, p) ++ " ==> __"
	(Nothing, Just (Edge _ _ t p)) -> show (t, p) ++ " __ ==> p"
	(Just (Edge _ _ lt lp), Just (Edge _ _ gt gp))
		-> show (lt, lp) ++ " ==> " ++ show (gt, gp) ++ "\n"


-- | Checks if NodeAction already exists. If so, adds it. Otherwise, returns
-- the original Morphism
addNodeAction :: Node a -> Node a -> Morphism a b -> Morphism a b
addNodeAction ln rn m@(Morphism nal eal) =
	if (Just ln, Just rn) `L.notElem` nal
		then Morphism ((Just ln, Just rn) : nal) eal
		else m

		
addEdgeAction :: Edge b -> Edge b -> Morphism a b -> Morphism a b
addEdgeAction le re m@(Morphism nal eal) =
	Morphism nal ((Just le, Just re) : eal)


nodeAction :: (Monad m, Eq a) => NodeAction a -> (Digraph a b -> m (Digraph a b))
nodeAction (Nothing, Just n) = addNode n
nodeAction (Just n, Nothing) = removeNode n 
nodeAction (Just n, Just n') = if n /= n' 
							then const $ fail "Node transformation is unhandled"
							else keepNode n
nodeAction (Nothing, Nothing) = return

edgeAction :: (Monad m, Eq b) => EdgeAction b -> (Digraph a b -> m (Digraph a b))
edgeAction (Nothing, Just e) = addEdge e
edgeAction (Just e, Nothing) = removeEdge e
edgeAction (Just e, Just e') = if e /= e'
							then const $ fail "Edge transformation is unhandled"
							else keepEdge e
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

nodeType :: Node a -> Int
nodeType (Node _ t _) = t

edgeType :: Edge a -> Int
edgeType (Edge _ _ t _) = t

findNodeType :: Int -> TypedDigraph a b -> Maybe Int
findNodeType id td@(TypedDigraph (Digraph nm em) _) =
	let n = IM.lookup id nm
	in case n of
		Nothing -> Nothing
		Just (Node _ tid _) -> Just tid

srcType :: Edge b -> TypedDigraph a b -> Maybe Int
srcType (Edge _ (s, _) _ _) l =
	findNodeType s l

tarType :: Edge b -> TypedDigraph a b -> Maybe Int
tarType (Edge _ (_, t) _ _) l =
	findNodeType t l


