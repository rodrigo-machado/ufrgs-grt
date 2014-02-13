module Assorted.PrettyPrint where

class Show a => PrettyPrint a where
    prettyPrint :: a -> String
    prettyPrint = show

printPretty :: PrettyPrint a => a -> IO ()
printPretty = putStr . prettyPrint

instance PrettyPrint Char
instance PrettyPrint Int
instance PrettyPrint a => PrettyPrint [a] where
    prettyPrint = unlines . map prettyPrint 
instance PrettyPrint a => PrettyPrint (Maybe a)
instance PrettyPrint () where
    prettyPrint = const ""
