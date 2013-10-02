module Ullmann	( module Data.Graph
				, module Matrix
				, buildMatrix
				, selectColumn
				, isomorphisms
				, homomorphisms
				) where

import Control.Monad
import Data.Maybe
import Data.Graph
import Data.List
import Matrix

zeros n = take n $ repeat 0

bruteForceIsomorphism :: Graph g => g -> g -> [Matrix Int]
bruteForceIsomorphism = enumHomomorphisms injectiveCondition checkMapping
-- The ullmann algorithm requires an additional prunning condition that is not yet implemented
isomorphisms :: Graph g => g -> g -> [Matrix Int]
isomorphisms = bruteForceIsomorphism

bruteForceHomomorphisms :: Graph g => g -> g -> [Matrix Int]
bruteForceHomomorphisms = enumHomomorphisms (const True) checkMapping
-- Reserved to add further prunning conditions.
homomorphisms :: Graph g => g -> g -> [Matrix Int]
homomorphisms = bruteForceHomomorphisms


-- This can be prettier, maybe...
buildMatrix :: Graph g => g -> g -> Matrix Int
buildMatrix match target = foldNodes (\mat m -> foldNodes (compareAndSet m) mat target) baseMatrix match
	where
		baseMatrix = mkMatrix (take lenMatch $ repeat $ zeros lenTarget) (nodeKeys target) (nodeKeys match)
		lenMatch = length $ nodeKeys match
		lenTarget = length $ nodeKeys target
		compareAndSet m matrix t = if degree m <= degree t then mSetCell 1 (nodeId t) (nodeId m) matrix else matrix

selectColumn :: Int -> Int -> Matrix Int -> Matrix Int
selectColumn col depth m = mSetLine depth m $ zeros col ++ [mGetCell depth col m] ++ zeros ((mColumns m) - col - 1)

nextDepth :: Int -> Matrix Int -> [Matrix Int]
nextDepth d m = do
	c <- [0..(mColumns m) - 1]
	return $ selectColumn c d m

nextDepthFilter :: (Matrix Int -> Bool) -> Int -> Matrix Int -> [Matrix Int]
nextDepthFilter f d = filter f . nextDepth d

nextDepthL d = concat . map (nextDepth d)

nextDepthLFilter f d = concat . map (nextDepthFilter f d)

iter pruner m = foldM (flip (nextDepthFilter pruner)) m [0..(mLines m) - 1]

bfEnum pruninigCondition finalCondition = filter finalCondition . iter pruninigCondition

enumCandidates :: Graph g => (Matrix Int -> Bool) -> g -> g -> [Matrix Int]
enumCandidates finalCondition alpha = 
	bfEnum surjectiveCondition finalCondition . buildMatrix alpha

enumHomomorphisms :: Graph g => 
								(Matrix Int -> Bool) -> 
								(Matrix Int -> Matrix Int -> Matrix Int -> Bool) ->
								g ->
								g ->
								[Matrix Int]
enumHomomorphisms finalCondition validityCondition alpha beta = let
	ma = adjacencyMatrix alpha
	mb = adjacencyMatrix beta
	in filter (validityCondition ma mb) $ enumCandidates finalCondition alpha beta

checkMapping :: Matrix Int -> Matrix Int -> Matrix Int-> Bool
checkMapping ma mb mk = let 
	mc = mMult mk . mTranspose . mMult mk $ mb
	in not . any (\(a, c) -> c > 0 && not (a == 1)) . toList $ zipMatrix ma mc

surjectiveCondition :: Matrix Int -> Bool
surjectiveCondition = not . any (\x -> not . any (==1) $ x) . matrix

injectiveCondition :: Matrix Int -> Bool
injectiveCondition = not . any (\c -> sum c > 1) . matrix . mTranspose
