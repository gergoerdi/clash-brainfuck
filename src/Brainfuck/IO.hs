module Brainfuck.IO where

import Prelude
import Brainfuck.Types
import Data.Char
import Control.Monad.State
import Data.Maybe
import System.IO
import System.Environment

class (Monad m) => MonadBFIO m where
    doOutput :: Cell -> m ()
    doInput :: m Cell

instance MonadBFIO IO where
    doOutput = putChar . chr . fromIntegral
    doInput = fromIntegral . ord <$> getChar

instance (MonadBFIO m) => MonadBFIO (StateT s m) where
    doOutput = lift . doOutput
    doInput = lift doInput

hello :: String
hello = "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."

prepareIO :: IO String
prepareIO = do
    hSetBuffering stdout NoBuffering

    fileName <- listToMaybe <$> getArgs
    maybe (return hello) readFile fileName
