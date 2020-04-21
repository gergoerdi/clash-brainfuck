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
logicBoard programFile inputValue ack = (_inputNeeded, _output)
  where
    CPUOut{..} = bdistribute' . fmap bcover $ cpu cpuIn

    ramRead = blockRam1 ClearOnReset (SNat @30_000) 0 _ramAddr write
    romRead = unpack <$> romFilePow2 programFile _romAddr

    cpuIn = do
        romRead <- romRead
        ramRead <- ramRead
        outputAck <- ack
        input <- inputValue
        pure $ CPUIn{..}

    write = packWrite <$> _ramAddr <*> _ramWrite
