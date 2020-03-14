{-# LANGUAGE DataKinds #-}
module Brainfuck.Stack where

import Clash.Prelude

data Stack n a = Stack (Vec n a) (Index n)
    deriving (Generic, NFDataX, Show)

push :: (KnownNat n) => Stack n a -> a -> Stack n a
push (Stack xs i) x = Stack (replace i x xs) (i + 1)

pop :: (KnownNat n) => Stack n a -> (a, Stack n a)
pop (Stack xs i) = (xs !! (i - 1), Stack xs (i - 1))
