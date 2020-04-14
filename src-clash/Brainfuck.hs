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
        ( display outbuf (enable inputNeededBuf inbuf)
        , reverse <$> cols
        )
      where
        (inputNeeded, output) = logicBoard "hello.rom" (enable ack inbuf) ack
        (cols, ack, inbuf) = inputs btn rows

        outbuf = regMaybe Nothing $ do
            clear <- ack
            output <- output
            pure $ case output of
                Just x -> Just (Just x)
                _ -> if clear then Just Nothing else Nothing

        inputNeededBuf = regMaybe False $ do
            clear <- ack
            inputNeeded <- inputNeeded
            pure $ case inputNeeded of
                True -> Just True
                _ -> if clear then Just False else Nothing

data SSChar
    = SSHex (Unsigned 4)
    | SSOutput
    | SSInput

displaySS :: SSChar -> Vec 7 Bool
displaySS (SSHex digit) = encodeHexSS digit
displaySS SSOutput = False :> False :> True :> True :> True :> False :> True :> Nil
displaySS SSInput = False :> False :> True :> False :> False :> False :> False :> Nil

displayChars :: Maybe Cell -> Maybe Cell -> Vec 4 (Maybe SSChar)
displayChars output input = case (output, input) of
    (Just o, _) -> Nothing :> Just SSOutput  :> (Just <$> digits o)
    (_, Just i) -> Nothing :> Just SSInput :> (Just <$> digits i)
    _           -> repeat Nothing
  where
    digits :: Cell -> Vec 2 SSChar
    digits x = SSHex hi :> SSHex lo :> Nil
      where
        (hi, lo) = bitCoerce x

display
    :: (HiddenClockResetEnable dom, _)
    => Signal dom (Maybe Cell)
    -> Signal dom (Maybe Cell)
    -> SevenSegment dom 4 anodes segments dp
display outbuf inbuf = driveSS (\c -> (displaySS c, False)) (reverse <$> chars)
  where
    chars = displayChars <$> outbuf <*> inbuf

inputs
    :: (HiddenClockResetEnable dom, _)
    => Signal dom (Active btn)
    -> Signal dom (Vec 4 (Active row))
    -> ( Signal dom (Vec 4 (Active col))
       , Signal dom Bool
       , Signal dom Cell
       )
inputs btn rows = (cols, ack, buffer)
  where
    ack = isRising False $ debounce (SNat @(Milliseconds 5)) False $
          fromActive <$> btn

    (cols, key) = inputKeypad keymap rows

    buffer = register 0x00 $ do
        ack <- ack
        digit <- key
        current <- buffer
        pure $ if ack then 0x00 else maybe current (shiftIn current) digit
      where
        shiftIn :: Cell -> Unsigned 4 -> Cell
        shiftIn x d = bitCoerce (lo, d)
          where
            (hi, lo) = bitCoerce x :: (Unsigned 4, Unsigned 4)


keymap :: Matrix 4 4 (Unsigned 4)
keymap =
    (0x1 :> 0x2 :> 0x3 :> 0xa :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xb :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xc :> Nil) :>
    (0x0 :> 0xf :> 0xe :> 0xd :> Nil) :>
    Nil

logicBoard
    :: forall dom. (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom (Maybe Cell)
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom (Maybe Cell))
logicBoard programFile inputValue ack = (view inputNeeded <$> cpuOut, view output <$> cpuOut)
  where
    cpuOut = cpu cpuIn

    ramRead = blockRam1 NoClearOnReset (SNat @30_000) 0 ramAddr ramWrite
    romRead = unpack <$> romFilePow2 programFile romAddr

    cpuIn = do
        instr <- romRead
        memRead <- ramRead
        outputAck <- ack
        input <- inputValue
        pure $ CPUIn{..}

    romAddr = view progAddr <$> cpuOut

    ramAddr = view memAddr <$> cpuOut
    ramWrite = packWrite <$> ramAddr <*> (view memWrite <$> cpuOut)

makeTopEntity 'topEntity
