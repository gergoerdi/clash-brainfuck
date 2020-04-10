{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Brainfuck.Memory where

import Clash.Prelude hiding (lift)

import Data.Word
import Data.Char (ord)
import Brainfuck.Types
import Brainfuck.IO

import Control.Monad.Reader
import Control.Monad.State

class (Monad m) => MonadBFMemory m where
    readProgROM :: PC -> m Word8
    readRAM :: Ptr -> m Cell
    writeRAM :: Ptr -> Cell -> m ()

type ProgSize = 2 ^ BitSize PC

type WithROM = ReaderT (Vec ProgSize Word8)

runWithROM :: (Monad m) => Vec ProgSize Word8 -> WithROM m a -> m a
runWithROM rom act = runReaderT act rom

type WithRAM = StateT (Vec MemSize Cell)

runWithRAM :: (Monad m) => WithRAM m a -> m a
runWithRAM act = evalStateT act (repeat 0)

newtype BFVec m a = BFVec{ unBFVec :: WithROM (WithRAM m) a }
    deriving newtype (Functor, Applicative, Monad)

runBFVec :: (Monad m) => Vec ProgSize Word8 -> BFVec m a -> m a
runBFVec prog = runWithRAM . runWithROM prog . unBFVec

instance (Monad m) => MonadBFMemory (BFVec m) where
    readProgROM pc = BFVec $ asks (!!pc)
    readRAM addr = BFVec $ gets (!!addr)
    writeRAM addr x = BFVec $ modify $ replace addr x

instance (MonadBFIO m) => MonadBFIO (BFVec m) where
    doInput = BFVec . lift $ doInput
    doOutput = BFVec . lift . doOutput

loadVec :: (KnownNat n) => [a] -> a -> Vec n a
loadVec xs x0 = unfoldrI uncons xs
  where
    uncons (x:xs) = (x, xs)
    uncons [] = (x0, [])

stringToROM :: (Functor f) => f Char -> f Word8
stringToROM = fmap (fromIntegral . ord)
