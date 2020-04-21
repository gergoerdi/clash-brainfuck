{-# LANGUAGE RecordWildCards #-}
module Main where

import Clash.Prelude hiding (lift)
import Brainfuck.CPU
import Brainfuck.Types
import Brainfuck.IO
import Brainfuck.Memory

import Data.Char (ord)
import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Lens
import Control.Monad

world :: (MonadBFMemory m, MonadBFIO m) => Pure CPUOut -> m (Pure CPUIn)
world CPUOut{..} = do
    romRead <- readProgROM _romAddr
    ramRead <- readRAM _ramAddr
    input <- if _inputNeeded then Just <$> doInput else return Nothing

    traverse_ (writeRAM _ramAddr) _ramWrite
    traverse_ doOutput _output
    outputAck <- return True

    return CPUIn{..}

simulateCPU :: (MonadBFMemory m, MonadBFIO m) => StateT (Pure CPUIn, CPUState) m ()
simulateCPU = do
    (inp, s) <- get
    let (out, s') = runState (cpuMachine inp) s
    inp' <- lift $ world out
    put (inp', s')

main :: IO ()
main = do
    prog <- prepareIO

    runBFVec (stringToROM $ loadVec prog '\0') $ do
        let initInput = CPUIn
                { romRead = 0
                , ramRead = 0
                , outputAck = False
                , input = Nothing
                }
        flip evalStateT (initInput, initCPUState) $ forever simulateCPU
