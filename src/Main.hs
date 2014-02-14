module Main where

import System.Environment
import System.Console.GetOpt

import Data.Maybe

import Control.Monad.IO.Class

import Assorted.PrettyPrint

import Graph.Rewriting
import Graph.Serialized

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
          , Option ['f'] ["output-format"] (ReqArg (\d o -> o { outputFormat = toOutputFormat d}) "format") "defines the output format of the data (raw, pretty)"
          ]

data OutputFormat = Pretty | Raw deriving (Show, Eq)

toOutputFormat s = case s of
                       "pretty" -> Pretty
                       otherwise -> Raw

output :: (Show a, PrettyPrint a) => OutputFormat -> a -> IO ()
output Pretty = printPretty
output Raw    = print


defaultOptions = SystemOptions { stepsToStop = 1
                               , outputFormat = Pretty
                               }

parseOptions :: MonadIO m => m (SystemOptions, [String])
parseOptions = do args <- liftIO $ getArgs
                  case getOpt Permute options args of
                      (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
                      (_,_,es) -> fail $ concat es ++ usageInfo header options
    where
        header = "Usage: main [args] file"
