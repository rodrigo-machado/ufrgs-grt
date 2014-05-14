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


graphToDot :: String -> D.TypedDigraph a b -> String
graphToDot prefix (D.TypedDigraph dg _) =
	let el = D.edges dg
	    nl = D.nodes dg
	    str = foldr (\e str ->
		let sid = D.sourceID e
		    tid = D.targetID e
		in "\t" ++ prefix ++ (show sid) ++ " -> " ++ prefix ++ (show tid) ++ ";\n" ++ str)
		" " el
	    str_types = foldr (\n str ->
		let ntype = D.nodeType n
		in
			"\t" ++ prefix ++ (show $ D.nodeID n) ++
			" [shape=\"circle\", style=\"filled\", " ++
			"label=" ++ (show $ D.nodeID n) ++ ", colorscheme=paired12, color=" ++
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
	"\tnode [shape=circle,style=filled,color=white];\n" ++
	"\tstyle=filled;\n" ++
	"\tcolor=lightgrey;\n" ++
	"\tlabel = \"L\"" ++
	graphToDot "" l ++
	"}\n" ++
	"\tsubgraph cluster1 {\n" ++
	"\tnode [style=filled]\n" ++
	"\tlabel = \"G\"\n" ++
	graphToDot "" g ++
	"}\n" ++
	"\tedge [color=red, style=dotted]\n\t" ++
	morphismEdges

ggToDot :: D.Digraph (D.TypedDigraph String String) a -> String
ggToDot g@(D.Digraph nm em) =
	let nodeStr = IM.fold (\n acc ->
		"\n\tsubgraph cluster" ++ show (D.nodeID n) ++ " {\n" ++
--		"\tlayout=circo;\n" ++
		graphToDot (showIfNonZero (D.nodeID n)) (D.nodePayload n) ++ "\t}\n" ++ acc)
		"" nm
	    edgeStr = IM.fold (\e acc ->
		let srcId = D.sourceID e
		    tgtId = D.targetID e
		    (D.TypedDigraph srcD _) = D.nodePayload $ D.source e g
		    (D.TypedDigraph tarD _) = D.nodePayload $ D.target e g
		    srcNodes = D.nodes srcD
		    tarNodes = D.nodes tarD
		in
		acc ++ "\n\t" ++ showIfNonZero srcId ++ show (D.nodeID $ head srcNodes) ++
		" -> " ++ showIfNonZero tgtId ++ show (D.nodeID $ head tarNodes) ++ 
		" [ltail=cluster" ++ show (D.sourceID e) ++
		", lhead=cluster" ++ show (D.targetID e) ++ "];\n")
			nodeStr em
	    graphStyle = "\n\tgraph [style=\"rounded, dashed\", ranksep=1.3];\n" ++
			 "\tcompound=true;\n" ++
			 "\tedge [len=3];\n"
--			 "\tlayout=circo;\n"
	in graphStyle ++ edgeStr
	where showIfNonZero nid =
		if nid == 0 then "" else (show nid)


finishDot :: String -> String -> String
finishDot name dot =
	"digraph " ++ show name ++ " {" ++ dot ++ "}\n"
