{-# LANGUAGE PartialTypeSignatures, NumericUnderscores, ApplicativeDo, RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Brainfuck where

import Clash.Prelude
import RetroClash.Utils
import Control.Monad
import Data.Maybe
import Data.Char
import Control.Monad.State

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
logicBoard programFile inputValue ack = (view inputNeeded <$> cpuOut, view output <$> cpuOut)
  where
    cpuOut = cpu cpuIn

    ramRead = withStart 0 $ blockRam1 NoClearOnReset (SNat @30_000) 0 ramAddr ramWrite
    romRead = withStart 0 $ unpack <$> romFilePow2 programFile romAddr

    cpuIn = do
        instr <- romRead
        memRead <- ramRead
        outputAck <- ack
        input <- inputValue
        pure $ CPUIn{..}

    romAddr = view progAddr <$> cpuOut

    ramAddr = view memAddr <$> cpuOut
    ramWrite = packWrite <$> ramAddr <*> (view memWrite <$> cpuOut)
