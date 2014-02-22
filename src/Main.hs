module Main where

import System.Environment
import System.Console.GetOpt

import Data.Maybe

import Diagrams.Prelude hiding (Option)
import Diagrams.Backend.SVG hiding (SVG)

import Control.Monad.IO.Class

import Assorted.PrettyPrint

import Graph.Digraph
import Graph.Rewriting
import Graph.Serialized
import Graph.Draw

main = do
    (options, systems) <- parseOptions
    system <- readFile $ head systems
    let (Serialized graphs rules) = unserializeUnit system
    let result = iterate (catMaybes . rewriteAll rules) graphs !! stepsToStop options
    output (outputFormat options) result


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
output Pretty = printPretty
output Raw    = print
output TikZ   = undefined
output SVG    = renderSVG "output.svg" (Dims 400 600) . formatGraph . graph . (!!0)


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
