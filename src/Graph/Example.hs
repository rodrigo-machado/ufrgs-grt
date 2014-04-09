import Control.Monad
import qualified Graph.Digraph as D
import Graph.Match

l = D.empty
g = D.empty
tg = D.empty

ln = [ D.Node 1 1 "1"
	, D.Node 2 1 "2"
	]
le = []

l1 = foldM (\d n -> D.addNode n d) l ln :: Maybe (D.Digraph String String)
l2 = l1 >>=
	(\l -> foldM (\d e -> D.addEdge e d) l le) :: Maybe (D.Digraph String String)

	
gn = [ D.Node 10 1 "a"
	, D.Node 11 1 "b"
	, D.Node 12 1 "c"
	, D.Node 13 1 "d"
	]
ge = [ D.Edge 15 (12, 13) 1 "c -> d"
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

r = D.Morphism [(Just (ln!!0), Nothing)] []

mappings = findMatchesR r tdl tdg
mappings2 = findMatches tdl tdg
