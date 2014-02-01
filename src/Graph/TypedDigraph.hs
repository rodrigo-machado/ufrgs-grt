module Graph.TypedDigraph
	( TypedDigraph (..)
	, TypeInfo
	, TGraph
	, nodeType
	, srcType
	, tarType
	)
	where

import Control.Monad
import qualified Data.IntMap as IM
import Graph.Digraph

type TGraph a b = Digraph a b

type TypeInfo a = (Int, a)

data TypedDigraph a b = TypedDigraph (Digraph (TypeInfo a) b) (TGraph a b)
	deriving (Show)

nodeType :: Node (TypeInfo a) -> Int
nodeType (Node _ (t, _)) = t

findNodeType :: Int -> TypedDigraph a b -> Maybe Int
findNodeType id td@(TypedDigraph (Digraph nm em) _) =
	let n = IM.lookup id nm
	in case n of
		Nothing -> Nothing
		Just (Node _ (tid, _)) -> Just tid

srcType :: Edge b -> TypedDigraph a b -> Maybe Int
srcType (Edge _ (s, _) _) l =
	findNodeType s l

tarType :: Edge b -> TypedDigraph a b -> Maybe Int
tarType (Edge _ (_, t) _) l =
	findNodeType t l
