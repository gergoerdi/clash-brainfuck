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

instance NFDataX a => NFDataX (First a)

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
        , cols
        )
      where
        (inputNeeded, output) = logicBoard "hello.rom" (enable ack inbuf) ack
        (cols, ack, inbuf) = inputs btn rows

        outbuf = getFirst <$> integrate ack (First <$> output)
        inputNeededBuf = getAny <$> integrate ack (Any <$> inputNeeded)

makeTopEntity 'topEntity
