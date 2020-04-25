{-# LANGUAGE NumericUnderscores, ApplicativeDo, RecordWildCards #-}
module Brainfuck.LogicBoard (logicBoard) where

import Clash.Prelude
import RetroClash.Utils

import Brainfuck.Types
import Brainfuck.CPU

import Data.Functor.Barbie
import Barbies.Bare
import Control.Lens hiding (Index, (:>))

logicBoard
    :: forall dom. (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom (Maybe Cell)
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom (Maybe Cell))
logicBoard programFile inputValue ack = (_inputNeeded <$> cpuOut, _output <$> cpuOut)
  where
    cpuOut = cpu cpuIn

    ramRead = blockRam1 NoClearOnReset (SNat @30_000) 0 (_ramAddr <$> cpuOut) write
    romRead = unpack <$> romFilePow2 programFile (_romAddr <$> cpuOut)

    cpuIn = do
        romRead <- romRead
        ramRead <- ramRead
        outputAck <- ack
        input <- inputValue
        pure $ CPUIn{..}

    write = packWrite <$> (_ramAddr <$> cpuOut) <*> (_ramWrite <$> cpuOut)
