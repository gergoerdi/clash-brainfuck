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

iSS :: Vec 7 Bool
iSS = False :> False :> True :> False :> False :> False :> False :> Nil

oSS :: Vec 7 Bool
oSS = False :> False :> True :> True :> True :> False :> True :> Nil

displaySS :: SSChar -> (Vec 7 Bool, Bool)
displaySS (SSHex digit) = (encodeHexSS digit, False)
displaySS SSOutput = (oSS, True)
displaySS SSInput = (iSS, True)

displayChars :: Maybe Cell -> Maybe Cell -> Vec 3 (Maybe SSChar)
displayChars output input = case (output, input) of
    (Just o, _) -> Just SSOutput  :> (Just <$> digits o)
    (_, Just i) -> Just SSInput :> (Just <$> digits i)
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
    -> Signal dom (SevenSegment (k + 3) anodes segments dp)
display output inbuf = driveSS displaySS (pad <$> chars)
  where
    chars = displayChars <$> output <*> inbuf
    pad xs = repeat Nothing ++ xs
