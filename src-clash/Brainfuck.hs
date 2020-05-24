module Brainfuck where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SevenSegment
import RetroClash.Barbies
import Data.Monoid (First(..), Any(..))

import Brainfuck.Types
import Brainfuck.LogicBoard
import Brainfuck.Display
import Brainfuck.Input

topEntity
    :: "CLK"     ::: Clock System
    -> "BTN"     ::: Signal System (Active High)
    -> "ROWS"    ::: Signal System (Vec 4 (Active Low))
    -> ( "COLS"  ::: Signal System (Vec 4 (Active Low))
       , "SS"    ::: Signal System (SevenSegment 4 Low Low Low)
       )
topEntity = withResetEnableGen board
  where
    board btn rows = (cols, ss)
      where
        (cols, ack, inBuf) = inputs btn rows

        (inputNeeded, output) = logicBoard "hello.rom" (enable ack inBuf) ack
        outBuf = fmap getFirst . integrate ack . fmap First $ output
        inputNeededBuf = fmap getAny . integrate ack . fmap Any $ inputNeeded

        ss = display outBuf (enable inputNeededBuf inBuf)

makeTopEntity 'topEntity
