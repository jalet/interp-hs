{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Utils where

import           Protolude

isLetter :: Char -> Bool
isLetter = flip elem $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

isDigit :: Char -> Bool
isDigit = flip elem ['0' .. '9']

(<<) :: (Monad m) => m a -> m b -> m a
ma << mb = ma >>= \a -> mb $> a

-- returnOrThrow :: (MonadError e m) => e -> Maybe -> m
returnOrThrow _ (Just a) = return a
returnOrThrow e Nothing  = throwError e
