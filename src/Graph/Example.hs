import Control.Monad
import qualified Graph.Digraph as D
import Graph.Match

l = D.empty
g = D.empty
tg = D.empty

ln = 	[ D.Node 1 (1, "1")
	, D.Node 2 (1, "2")
	, D.Node 3 (2, "3")
	, D.Node 4 (1, "4")
	, D.Node 5 (2, "5")
	]
le = 	[ D.Edge 4 (2, 1) (1, "2 -> 1")
	, D.Edge 5 (1, 3) (1, "1 -> 3")
	, D.Edge 6 (4, 4) (2, "4 -> 4")
	]

l1 = foldM (\d n -> D.addNode n d) l ln :: Maybe (D.Digraph String String)
l2 = l1 >>=
	(\l -> foldM (\d e -> D.addEdge e d) l le) :: Maybe (D.Digraph String String)

	
gn = 	[ D.Node 10 (1, "a")
	, D.Node 11 (1, "b")
	, D.Node 12 (1, "c")
	, D.Node 13 (2, "d")
	, D.Node 14 (1, "e")
	, D.Node 15 (2, "f")
	, D.Node 16 (2, "g")
	]
ge = 	[ D.Edge 15 (10, 11) (1, "a -> b")
	, D.Edge 16 (10, 12) (1, "a -> c")
	, D.Edge 17 (12, 13) (1, "c -> d")
	, D.Edge 18 (14, 14) (2, "e -> e")
	]
g1 = foldM (\d n -> D.addNode n d) g gn :: Maybe (D.Digraph String String)
g2 = g1 >>=
	(\g -> foldM (\d e -> D.addEdge e d) g ge) :: Maybe (D.Digraph String String)

tdl = case l2 of
	Just t -> D.TypedDigraph t tg
	otherwise -> D.TypedDigraph l tg
tdg = case g2 of
	Just t -> D.TypedDigraph t tg
	otherwise -> D.TypedDigraph g tg


morphisms = matchEdges tdl tdg
