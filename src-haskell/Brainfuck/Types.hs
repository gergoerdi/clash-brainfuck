{-# LANGUAGE DataKinds, NumericUnderscores #-}
module Brainfuck.Types where

import Data.Word
import Clash.Sized.Index
import Clash.Sized.Unsigned

type Cell = Word8

type MemSize = 30_000
type PC = Unsigned 12
type Ptr = Index MemSize
type StackSize = 32
