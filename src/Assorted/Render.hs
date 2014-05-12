module Assorted.Render
	(graphToDot
	,mappingToDot
	,ggToDot
	,finishDot
)
where

import qualified Graph.Digraph as D
import qualified Data.IntMap as IM
import Graph.Match (Mapping)
import qualified Data.Set as S
import qualified Data.List as L


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
			" [style=\"filled\", colorscheme=paired12, color=" ++
			(show ntype) ++ "];\n" ++ str
		)
		" " nl
	in str ++ str_types

		
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

ggToDot :: D.Digraph (D.TypedDigraph String String) a -> String
ggToDot g@(D.Digraph nm em) =
	let nodeStr = IM.fold (\n acc ->
		"\n\tsubgraph cluster" ++ show (D.nodeID n) ++ " {\n" ++
		graphToDot (D.nodePayload n) ++ "\t}\n" ++ acc)
		"" nm
	    edgeStr = IM.fold (\e acc ->
		let (D.TypedDigraph srcD _) = D.nodePayload $ D.source e g
		    (D.TypedDigraph tarD _) = D.nodePayload $ D.target e g
		    srcNodes = D.nodes srcD
		    tarNodes = D.nodes tarD
		in
		acc ++ "\n\t" ++ show (D.nodeID $ head srcNodes) ++
		" -> " ++ show (D.nodeID $ head tarNodes) ++ 
		" [ltail=cluster" ++ show (D.sourceID e) ++
		", lhead=cluster" ++ show (D.targetID e) ++ "];\n")
		nodeStr em
	    graphStyle = "\n\tgraph [style=\"rounded, filled\", color=\"lightyellow\"];\n"
	in graphStyle ++ edgeStr

finishDot :: String -> String -> String
finishDot name dot =
	"digraph " ++ show name ++ " {" ++ dot ++ "}\n"
