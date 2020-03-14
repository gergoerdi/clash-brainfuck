{-# LANGUAGE NumericUnderscores #-}
module Brainfuck.Types where

import Data.Word
import Clash.Prelude

type PC = Unsigned 12
type Ptr = Index 30_0000
type Cell = Word8
type StackSize = 32
