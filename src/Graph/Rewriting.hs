module Graph.Rewriting where

import Graph.Digraph

type Rule a b = Morphism a b a b

findMatches :: TypedDigraph a b -> TypedDigraph a b -> [Morphism a b]
findMatches = undefined

-- rewrite rule graph match
rewrite :: (Monad m) => Rule a b -> TypedDigraph a b -> Morphism a b -> m TypedDigraph a b
rewrite = undefined
-- fmap (rewrite rule graph) matches
