module Brainfuck.IO where

import Prelude
import Brainfuck.Types
import Data.Char
import Control.Monad.State

class (Monad m) => MonadBFIO m where
    output :: Cell -> m ()
    input :: m Cell

instance MonadBFIO IO where
    output = putChar . chr . fromIntegral
    input = fromIntegral . ord <$> getChar

instance (MonadBFIO m) => MonadBFIO (StateT s m) where
    output = lift . output
    input = lift input
