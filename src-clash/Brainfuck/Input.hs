{-# LANGUAGE PartialTypeSignatures, ApplicativeDo #-}
module Brainfuck.Input (inputs) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Keypad
import RetroClash.Clock
import Brainfuck.Types

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
