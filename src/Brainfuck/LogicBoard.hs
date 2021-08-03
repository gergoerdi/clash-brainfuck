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
logicBoard programFile input outputAck = (_inputNeeded, _output)
  where
    CPUOut{..} = cpu CPUIn{..}

    ramRead = blockRam1 NoClearOnReset (SNat @30_000) 0 _ramAddr write
    romRead = unpack <$> romFilePow2 programFile _romAddr
    write = packWrite <$> _ramAddr <*> _ramWrite
