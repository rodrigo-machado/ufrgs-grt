module Graph.TestRewriting where

import Data.Maybe

import Graph.Match
import Graph.Rewriting
import Graph.Digraph

-- Prelude's Either monad is broken, since fail only accepts Strings as arguments
data Error r = Err String | OK r deriving (Eq)

instance Monad Error where
	return x = OK x
	(Err y) >>= _ = Err y
	(OK x) >>= f = (f x)
	fail = Err

instance (Show r) => Show (Error r) where
	show (OK x) = show x
	show (Err msg) = unwords ["err:", msg]

rewriteOnError :: (Eq a, Eq b) => Rule a b -> TypedDigraph a b -> [Error (TypedDigraph a b)]
rewriteOnError = rewrite

typeInfo t = (t, ())

tGraph = fromJust $ applyActions empty	[ addNode $ Node 1 $ typeInfo 0
											, addNode $ Node 2 $ typeInfo 0
											, addEdge $ Edge 1 (1, 2) $ typeInfo 0
											, addEdge $ Edge 2 (2, 1) $ typeInfo 0
											, addEdge $ Edge 3 (1, 1) $ typeInfo 0
											, addEdge $ Edge 4 (2, 2) $ typeInfo 0
											]

nb0 = Node 0 $ typeInfo 1
nb1 = Node 1 $ typeInfo 1
nb2 = Node 2 $ typeInfo 2
nb3 = Node 3 $ typeInfo 1
eb1 = Edge 1 (1, 2) $ typeInfo 1
eb2 = Edge 2 (2, 3) $ typeInfo 2

beta = fromJust $ applyActions empty	[ addNode $ nb0
										, addNode $ nb1
										, addNode $ nb2
										, addNode $ nb3
										, addEdge $ eb1
										, addEdge $ eb2
										]

betat = TypedDigraph beta tGraph

na1 = Node 1 $ typeInfo 1
na2 = Node 2 $ typeInfo 2
na3 = Node 3 $ typeInfo 1
na4 = Node 4 $ typeInfo 1
ea1 = Edge 1 (3, 2) $ typeInfo 1
ea2 = Edge 2 (2, 1) $ typeInfo 2
ea3 = Edge 3 (4, 1) $ typeInfo 3

alpha = fromJust $ applyActions empty	[ addNode $ na1
										, addNode $ na2
										, addNode $ na3
										, addEdge $ ea1
										, addEdge $ ea2
										]

alphat = TypedDigraph alpha tGraph

rule1 = Morphism	[(Just na1, Just na1), (Just na2, Just na2), (Just na3, Nothing)] 
					[(Just ea1, Nothing), (Just ea2, Just ea2)]

rule2 = Morphism	[(Just na1, Just na1), (Just na2, Just na2), (Just na3, Nothing), (Nothing, Just na4)] 
					[(Just ea1, Nothing), (Just ea2, Just ea2), (Nothing, Just ea3)]

match1 = Morphism	[(Just na3, Just nb1), (Just na2, Just nb2), (Just na1, Just nb3)]
					[(Just ea1, Just eb1), (Just ea2, Just eb2)]

