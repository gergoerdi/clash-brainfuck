{-# LANGUAGE RecordWildCards #-}
module Main where

import Clash.Prelude hiding (lift)
import Brainfuck.CPU
import Brainfuck.Types
import Brainfuck.IO
import Brainfuck.Memory

import qualified Data.ByteString as BS
import System.IO
import Data.Foldable (traverse_)
import Control.Monad.State.Strict
import Control.Lens


simulateCPU :: (MonadBFMemory m, MonadBFIO m) => StateT (CPUState, Raw CPUOut) m ()
simulateCPU = do
    (s, CPUOut{..}) <- get

    instr <- lift $ readProgROM _progAddr
    memRead <- lift $ readRAM _memAddr

    lift $ traverse_ (writeRAM _memAddr) _memWrite

    let cpuIn = CPUIn
            { outputAck = True
            , input = Nothing
            , ..
            }
    cpuOut'@CPUOut{..} <- zoom _1 $ mapStateT (return . runIdentity) $ cpuIO cpuIn
    zoom _2 $ put cpuOut'

    lift $ traverse_ doOutput _output

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    prog <- BS.readFile "hello.bf"

    runBFVec (loadVec (BS.unpack prog) 0) $
      flip evalStateT (initBFState, defaultOutput initBFState) $
        forever simulateCPU
