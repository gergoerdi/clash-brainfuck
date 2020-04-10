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

cpuOI :: (MonadBFMemory m, MonadBFIO m) => Raw CPUOut -> m CPUIn
cpuOI CPUOut{..} = do
    instr <- readProgROM _progAddr
    memRead <- readRAM _memAddr
    input <- if _inputNeeded then Just <$> doInput else return Nothing

    traverse_ (writeRAM _memAddr) _memWrite
    traverse_ doOutput _output
    outputAck <- return True

    return CPUIn{..}

simulateCPU :: (MonadBFMemory m, MonadBFIO m) => StateT (CPUIn, CPUState) m ()
simulateCPU = do
    (inp, s) <- get
    let (out, s') = runState (cpuIO inp) s
    inp' <- lift $ cpuOI out
    put (inp', s')

main :: IO ()
main = do
    prog <- prepareIO

    runBFVec (stringToROM $ loadVec prog '\0') $ do
        let initInput = CPUIn
                { instr = 0
                , memRead = 0
                , outputAck = False
                , input = Nothing
                }
        flip evalStateT (initInput, initCPUState) $ forever simulateCPU
