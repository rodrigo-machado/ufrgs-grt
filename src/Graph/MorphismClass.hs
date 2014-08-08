module Graph.MorphismClass where

import Data.List

class GraphClass g => MorphismClass (m g) where

    domain             :: m g -> g
    codomain           :: m g -> g
    mapping            :: m g -> ([(Nd g, Nd g)], [(Ed g, Ed g)])

    empty              :: g -> g -> m g
    updateNodeMapping  :: (Nd g) -> (Nd g) -> m g -> m g
    updateEdgeMapping  :: (Ed g) -> (Ed g) -> m g -> m g

    hasNodeMapping     :: Nd g -> Nd g -> m g -> Bool
    hasEdgeMapping     :: Ed g -> Ed g -> m g -> Bool

    hasNodeMapping lNode rNode m =
        (lNode, rNode) `elem` nodeMappings
      where
        (nodeMappings, _) = mapping m

    hasEdgeMapping lEdge rEdge m =
        (lNode, rNode) `elem` edgeMappings
      where
        (edgeMappings _) = mapping m
