module Brainfuck where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SevenSegment
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
       , "SS"    ::: SevenSegment System 4 Low Low Low
       )
topEntity = withResetEnableGen board
  where
    board btn rows = (cols, display outbuf (enable inputNeededBuf inbuf))
      where
        (inputNeeded, output) = logicBoard "hello.rom" (enable ack inbuf) ack
        (cols, ack, inbuf) = inputs btn rows

        outbuf = fmap getFirst . integrate ack . fmap First $ output
        inputNeededBuf = fmap getAny . integrate ack . fmap Any $ inputNeeded

makeTopEntity 'topEntity
