module Main where

import System.Environment
import System.Console.GetOpt

import Data.Maybe

import Diagrams.Prelude hiding (Option,transform)
import Diagrams.Backend.SVG hiding (SVG)

import Control.Monad
import Control.Monad.Maybe
import Control.Monad.List
import Control.Monad.IO.Class

import Assorted.PrettyPrint

import Graph.Digraph
import Graph.Match
import Graph.Rewriting
import Graph.Serialized
import Graph.Draw

main = do
    (options, systems) <- parseOptions
    system <- readFile $ head systems
    let (Serialized graphs rules) = unserializeUnit system
    let ts = transform (stepsToStop options) (head graphs) rules
    output (outputFormat options) $ nubIsomorphic $ catMaybes ts

nubIsomorphic :: [TypedDigraph a b] -> [TypedDigraph a b]
nubIsomorphic gs = foldr (\g' gs' -> if any (isIsomorphic g') gs' then gs' else (g':gs')) [head gs] (tail gs)

transform :: (Eq a, Eq b) => Int -> TypedDigraph a b -> [Rule a b] -> [Maybe (TypedDigraph a b)]
transform n g rs = trans n [[return g]] rs
    where trans :: (Eq a, Eq b) => Int -> [[Maybe (TypedDigraph a b)]] -> [Rule a b] -> [Maybe (TypedDigraph a b)]
          trans 0 gs _ = concat gs
          trans n gs rs = trans (n - 1) ((map return $ nubIsomorphic $ catMaybes (transformAllGraphs (catMaybes $ head gs) rs)):gs) rs

transformAllGraphs :: (Eq a, Eq b) => [TypedDigraph a b] -> [Rule a b] -> [Maybe (TypedDigraph a b)]
transformAllGraphs gs rs = do
    g <- gs
    transformBigStep g rs

transformBigStep :: (Eq a, Eq b) => TypedDigraph a b -> [Rule a b] -> [Maybe (TypedDigraph a b)]
transformBigStep g rs = do
    r <- rs
    transformSmallStep g r

transformSmallStep :: (Eq a, Eq b) => TypedDigraph a b -> Rule a b -> [Maybe (TypedDigraph a b)]
transformSmallStep g r = do
    let (TypedDigraph _ t) = g
        l = left r t
        ms = findMatches l g
    map (rewrite r g) ms

unserializeUnit :: String -> Serialized () ()
unserializeUnit = unserialize

data SystemOptions = SystemOptions { stepsToStop :: Int
                                   , outputFormat :: OutputFormat
                                   } deriving (Show, Eq)

options = [ Option ['s'] ["stop-after"]    (ReqArg (\d o -> o { stepsToStop = read d }) "n") "stop after n steps"
          , Option ['f'] ["output-format"] (ReqArg (\d o -> o { outputFormat = toOutputFormat d}) "format") "defines the output format of the data (raw, pretty, tikz, svg)"
          ]

data OutputFormat = Pretty
                  | Raw
                  | TikZ
                  | SVG deriving (Show, Eq)

toOutputFormat s = case s of
                       "pretty"  -> Pretty
                       "tikz"    -> TikZ
                       "svg"     -> SVG
                       otherwise -> Raw

graph (TypedDigraph g t) = g

output :: (Show a, Show b, PrettyPrint a, PrettyPrint b) => OutputFormat -> [TypedDigraph a b] -> IO ()
output Pretty gs = printPretty gs
output Raw    gs = print gs
output TikZ   gs = undefined
output SVG    gs = forM_ [0..length gs - 1] $ \i -> renderSVG (concat ["output", show i, ".svg"]) (Dims 400 600) . formatGraph $ graph (gs !! i)


defaultOptions = SystemOptions { stepsToStop = 1
                               , outputFormat = Pretty
                               }

parseOptions :: MonadIO m => m (SystemOptions, [String])
parseOptions = do args <- liftIO $ getArgs
                  case getOpt Permute options args of
                      (o,(h:t),[]) -> return (foldl (flip id) defaultOptions o, (h:t))
                      (_,_,es) -> fail $ concat es ++ usageInfo header options
    where
        header = "Usage: main [args] file"
