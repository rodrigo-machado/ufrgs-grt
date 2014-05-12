import Control.Monad
import qualified Graph.Digraph as D
import Assorted.Render
import Graph.Match

l = D.empty
g = D.empty
tg = D.empty

ln = [ D.Node 1 1 "1"
	, D.Node 2 1 "2"
	, D.Node 3 2 "3"
	, D.Node 4 3 "4"
	]
le = [
	D.Edge 1 (1, 2) 1 "1 -> 2"
	, D.Edge 2 (3, 3) 1 "3 -> 3"
	]

l1 = foldM (\d n -> D.addNode n d) l ln :: Maybe (D.Digraph String String)
l2 = l1 >>=
	(\l -> foldM (\d e -> D.addEdge e d) l le) :: Maybe (D.Digraph String String)

	
gn = [ D.Node 10 1 "a"
	, D.Node 11 1 "b"
	, D.Node 12 1 "c"
	, D.Node 13 1 "d"
	, D.Node 14 2 "e"
	, D.Node 15 1 "f"
	, D.Node 16 3 "f"
	]
ge = [ D.Edge 15 (12, 13) 1 "c -> d"
	, D.Edge 16 (14, 14) 1 "f -> f"
	, D.Edge 17 (13, 12) 1 "d -> c"
	, D.Edge 18 (10, 15) 1 "a -> f"
	, D.Edge 19 (10, 15) 1 "a -> f"
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
emptyRule = D.Morphism [] []

maps = findMatches Mono tdl tdg

main = do
	putStrLn $ "found " ++ (show (length maps)) ++ " mappings\n"
	forM_ [0 .. ((length maps) - 1)] $ \m ->
		let contents = "digraph G {\n" ++
				mappingToDot (maps!!m) tdl tdg ++ "}"
		in writeFile ("graph" ++ (show m) ++ ".gv") contents
	--putStr $ graphToDot tdl
