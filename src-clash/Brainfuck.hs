{-# LANGUAGE PartialTypeSignatures, NumericUnderscores, ApplicativeDo, RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Brainfuck where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SevenSegment
import RetroClash.Keypad
import RetroClash.Clock
import RetroClash.SerialRx
import RetroClash.SerialTx
import Control.Monad
import Data.Maybe
import Data.Char
import Control.Monad.State

import Brainfuck.Types
import Brainfuck.CPU

import Data.Functor.Barbie
import Barbies.Bare
import Control.Lens hiding (Index, (:>))

topEntity
    :: "CLK"     ::: Clock System
    -> "BTN"     ::: Signal System (Active High)
    -> "ROWS"    ::: Signal System (Vec 4 (Active Low))
    -> ( "SS"    ::: SevenSegment System 4 Low Low Low
       , "COLS"  ::: Signal System (Vec 4 (Active Low))
       )
topEntity = withResetEnableGen board
  where
    board btn rows =
        ( driveSS display (reverse <$> digits)
        , reverse <$> cols
        )
      where
        (inputNeeded, output) = logicBoard (pure Nothing) ack
        ack = isRising False $ debounce (SNat @(Milliseconds 5)) False $
            fromActive <$> btn

        ui :: _ (Maybe Bool)
        ui = do
            inputNeeded <- inputNeeded
            output <- output
            pure $ case (inputNeeded, output) of
                (True, _) -> Just True
                (_, Just{}) -> Just False
                _ -> Nothing

        io :: _ (Vec 2 (Maybe (Unsigned 4)))
        io = do
            output <- output
            pure $ case output of
                Nothing -> Nothing :> Nothing :> Nil
                Just x -> let (hi, lo) = bitCoerce x
                          in Just hi :> Just lo :> Nil

        digits :: _ (Vec 4 (Maybe (Either Bool (Unsigned 4))))
        digits = bundle $
            (pure Nothing) :>
            (fmap Left <$> ui) :>
            unbundle (map (fmap Right) <$> io)

        input = inputKeypad keymap
        (cols, key) = input rows
        -- cmdKey = (keyToCmd =<<) <$> key

        display :: Either Bool (Unsigned 4) -> _
        display (Left isInput) = (if isInput then i else o, False)
          where
            i = False :> False :> True :> False :> False :> False :> False :> Nil
            o = False :> False :> True :> True :> True :> False :> True :> Nil
        display (Right digit) = (encodeHexSS digit, False)

keymap :: Matrix 4 4 (Unsigned 4)
keymap =
    (0x1 :> 0x2 :> 0x3 :> 0xa :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xb :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xc :> Nil) :>
    (0x0 :> 0xf :> 0xe :> 0xd :> Nil) :>
    Nil

logicBoard
    :: forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe Cell)
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom (Maybe Cell))
logicBoard inputValue ack = (view inputNeeded <$> cpuOut, view output <$> cpuOut)
  where
    cpuOut = cpu cpuIn

    ramRead = withStart 0 $ blockRam1 NoClearOnReset (SNat @30_000) 0 ramAddr ramWrite
    romRead = withStart 0 $ unpack <$> romFilePow2 "hello.rom" romAddr

    cpuIn = do
        instr <- romRead
        memRead <- ramRead
        outputAck <- ack
        input <- inputValue
        pure $ CPUIn{..}

    romAddr = view progAddr <$> cpuOut

    ramAddr = view memAddr <$> cpuOut
    ramWrite = packWrite <$> ramAddr <*> (view memWrite <$> cpuOut)

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

withStart :: (HiddenClockResetEnable dom) => Signal dom a -> Signal dom a -> Signal dom a
withStart = mux (register True $ pure False)

makeTopEntity 'topEntity
