import Control.Monad
import qualified Graph.Digraph as D
import Assorted.Render
import Graph.Match

l = D.empty
g = D.empty
k = D.empty
tg = D.empty

ln = [ D.Node 1 1 "1"
	, D.Node 2 1 "2"
	, D.Node 3 2 "3"
	]
le = [
	D.Edge 1 (1, 2) 1 "1 -> 2"
	]

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

kn = [ D.Node 20 1 "a"
	, D.Node 21 1 "b"
	, D.Node 22 1 "c"
	]
ke = [
	D.Edge 23 (20, 20) 1 "b"
	, D.Edge 24 (21, 22) 1 "b"
	]
k1 = foldM (\d n -> D.addNode n d) k kn :: Maybe (D.Digraph String String)
k2 = k1 >>=
	(\k -> foldM (\d e -> D.addEdge e d) k ke) :: Maybe (D.Digraph String String)

tdl = case l2 of
	Just t -> D.TypedDigraph t tg
	otherwise -> D.TypedDigraph l tg
tdg = case g2 of
	Just t -> D.TypedDigraph t tg
	otherwise -> D.TypedDigraph g tg
tdk = case k2 of
	Just t -> D.TypedDigraph t tg
	otherwise -> D.TypedDigraph k tg


mainGraph :: D.Digraph (D.TypedDigraph String String) String
mainGraph = D.insEdge (D.Edge 1 (1, 2) 1 "") $
	    D.insEdge (D.Edge 2 (1, 3) 1 "") $
	    D.insNode (D.Node 3 1 tdk) $ 
	    D.insNode (D.Node 2 1 tdg) $ 
	    D.insNode (D.Node 1 1 tdl) $
	    D.empty

r = D.Morphism [(Just (ln!!0), Nothing)] []
emptyRule = D.Morphism [] []



maps = findMatches Mono tdl tdg

main = do
{-	putStrLn $ "found " ++ (show (length maps)) ++ " mappings\n"
	forM_ [0 .. ((length maps) - 1)] $ \m ->
		let contents = "digraph G {\n" ++
				mappingToDot (maps!!m) tdl tdg ++ "}"
		in writeFile ("graph" ++ (show m) ++ ".gv") contents 
-}
	putStr $ "digraph G {" ++ (ggToDot mainGraph) ++ "}\n"
