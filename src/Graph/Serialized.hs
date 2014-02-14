{-# LANGUAGE DatatypeContexts #-}
module Graph.Serialized where

import Graph.Digraph
import Graph.Rewriting

data (Show a, Show b, Read a, Read b) => Serialized a b = Serialized [TypedDigraph a b] [Rule a b] deriving (Show, Read)

serialize :: (Show a, Show b, Read a, Read b) => [TypedDigraph a b] -> [Rule a b] -> String
serialize gs = show . Serialized gs

unserialize :: (Show a, Show b, Read a, Read b) => String -> Serialized a b
unserialize = read
