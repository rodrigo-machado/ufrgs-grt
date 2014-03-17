{-# LANGUAGE NoMonomorphismRestriction #-}
module Graph.Draw where

import qualified Data.IntMap as M

import Diagrams.Prelude
--import Diagrams.TwoD.Polygons
import Diagrams.TwoD.Arrow
--import Diagrams.Backend.TikZ

import Graph.Digraph hiding (node)

--node :: Int -> Diagram B R2
node n = text (show n) # scale 0.2 
                       # fc white
      <> circle 0.2    # fc green 
                       # named n

arrowOpts = with & headGap  .~ 0.07
                 & tailGap  .~ 0.07
                 & headSize .~ 0.2

--formatGraph :: Digraph a b -> Diagram B R2
formatGraph g = let ns = nodes g
                    es = edges g
                in  decorateTrail (regPoly (length ns) 1) (map (node . nodeID) ns)
                  # applyAll (map (\(Edge _ (s, t) _ _) -> connectOutside' arrowOpts s t) es)
