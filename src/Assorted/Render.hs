module Assorted.Render
	(graphToDot
	 ,mappingToDot)
where

import qualified Graph.Digraph as D
import Graph.Match (Mapping)
import qualified Data.Set as S
import qualified Data.List as L

type Edge :: Int -> Int -

printEdge :: Int -> 

graphToDot :: D.TypedDigraph a b -> String
graphToDot (D.TypedDigraph dg _) =
	let el = D.edges dg
	    nl = D.nodes dg
	    str = foldr (\e str ->
		let sid = D.sourceID e
		    tid = D.targetID e
		in "\t" ++ (show sid) ++ " -> " ++ (show tid) ++ ";\n" ++ str)
		" " el
	    str_types = foldr (\n str ->
		let ntype = D.nodeType n
		in
			"\t" ++ (show $ D.nodeID n) ++
			" [shape=polygon, sides=" ++
			(show (ntype + 2)) ++ "];\n" ++ str)
		" " nl
	in str ++ str_types ++ "\n"

		
mappingToDot :: Mapping -> D.TypedDigraph a b -> D.TypedDigraph a b -> String
mappingToDot m@(nmaps, emaps) l g =
	let morphismEdges = 
		foldr (\(s, t) acc -> "\t" ++ (show s) ++ " -> " ++ (show t) ++ ";\n" ++ acc)
			" " nmaps
	    
	in 
	"\n\tsubgraph cluster0 {\n" ++
	"\tnode [style=filled,color=white];\n" ++
	"\tstyle=filled;\n" ++
	"\tcolor=lightgrey;\n" ++
	"\tlabel = \"L\"" ++
	graphToDot l ++
	"}\n" ++
	"\tsubgraph cluster1 {\n" ++
	"\tnode [style=filled]\n" ++
	"\tlabel = \"G\"\n" ++
	graphToDot g ++
	"}\n" ++
	"\tedge [color=red, style=dotted]\n\t" ++
	morphismEdges
