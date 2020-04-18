{-# LANGUAGE PartialTypeSignatures #-}
module Brainfuck.Display (display) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.SevenSegment
import Brainfuck.Types

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
display output inbuf = driveSS (\c -> (displaySS c, False)) chars
  where
    chars = displayChars <$> output <*> inbuf
