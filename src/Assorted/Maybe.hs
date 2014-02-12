module Assorted.Maybe where

liftMaybeMsg :: Monad m => String -> Maybe a -> m a
liftMaybeMsg msg Nothing  = fail msg
liftMaybeMsg _   (Just a) = return a

liftMaybe :: Monad m => Maybe a -> m a
liftMaybe m = liftMaybeMsg "Nothing" m