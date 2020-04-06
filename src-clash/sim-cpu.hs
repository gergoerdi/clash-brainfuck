{-# LANGUAGE RecordWildCards #-}
module Main where

import Clash.Prelude hiding (lift)
import Brainfuck.CPU hiding (output)
import Brainfuck.Types
import Brainfuck.IO
import Brainfuck.Memory

import qualified Data.ByteString as BS
import System.IO
import Data.Foldable (for_)
import Control.Monad.State.Strict
import Control.Lens


simulateCPU :: (MonadBFMemory m, MonadBFIO m) => StateT (CPUState, Raw CPUOut) m ()
simulateCPU = do
    (s, out@CPUOut{..}) <- get

    instr <- lift $ readProgROM _progAddr
    memRead <- lift $ readRAM _memAddr

    for_ _memWrite $ lift . writeRAM _memAddr

    let input = CPUIn
            { outputAck = True
            , input = Nothing
            , ..
            }
    out'@CPUOut{..} <- zoom _1 $ mapStateT (return . runIdentity) $ step' input
    zoom _2 $ put out'

    for_ _output $ lift . output

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    prog <- BS.readFile "hello.bf"

    runBFVec (loadVec (BS.unpack prog) 0) $
      flip evalStateT (initBFState, defaultOutput initBFState) $
        forever simulateCPU
