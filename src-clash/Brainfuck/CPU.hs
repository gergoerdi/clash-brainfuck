{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Brainfuck.CPU where

import Brainfuck.Types
import Brainfuck.Stack

import Clash.Prelude
import RetroClash.Utils
-- import RetroClash.CPU
import Control.Monad
import Data.Maybe
import Data.Char
import Data.Word
import Control.Monad.Writer
import Control.Monad.State

data CPUIn = CPUIn
    { instr :: Word8
    , memRead :: Cell
    , outputAck :: Bool
    , input :: Maybe Cell
    }
    deriving (Generic, Show)

data CPUOut = CPUOut
    { progAddr :: PC
    , memAddr :: Ptr
    , memWrite :: Maybe Cell
    , output :: Maybe Cell
    , inputNeeded :: Bool
    }
    deriving (Generic, Show)

data CPUOut' = CPUOut'
    { memWrite' :: Last Cell
    , output' :: Last Cell
    , inputNeeded' :: Last Bool
    }

instance Semigroup CPUOut' where
    (CPUOut' wr out inp) <> (CPUOut' wr' out' inp') = CPUOut' (wr <> wr') (out <> out') (inp <> inp')

instance Monoid CPUOut' where
    mempty = CPUOut' mempty mempty mempty

data Phase
    = Init
    | Exec
    | Skip (Index StackSize)
    | WaitWrite
    | WaitOutput
    | WaitInput
    | Halt
    deriving (Show, Generic, NFDataX)

data CPUState = CPUState
    { phase :: Phase
    , pc :: PC
    , stack :: Stack StackSize PC
    , ptr :: Ptr
    }
    deriving (Generic, NFDataX)

initBFState = CPUState
    { phase = Init
    , pc = 0
    , stack = Stack (pure 0) 0
    , ptr = 0
    }

cpu :: (HiddenClockResetEnable dom) => Signal dom CPUIn -> Signal dom CPUOut
cpu = mealyState step' initBFState
  where
    step' inp = do
        -- trace (showX ("step", memRead inp)) $ return ()
        CPUOut'{..} <- execWriterT (step inp)
        CPUState{..} <- get
        return $ CPUOut
            { progAddr = pc
            , memAddr = ptr
            , memWrite = getLast memWrite'
            , output = getLast output'
            , inputNeeded = fromMaybe False $ getLast inputNeeded'
            }

type M = WriterT CPUOut' (State CPUState)

pushPC :: M ()
pushPC = do
    pc <- gets pc
    modify $ \st -> st{ stack = push (stack st) (pc - 1) }

popPC :: M ()
popPC = modify $ \st ->
    let (pc', stack') = pop (stack st)
    in st{ pc = pc', stack = stack' }

step :: CPUIn -> M ()
step CPUIn{..} = gets phase >>= \case
    Halt -> return ()
    Init -> goto Exec
    Skip depth -> fetch >>= \case
        '[' -> goto $ Skip $ depth + 1
        ']' -> goto $ maybe Exec Skip $ predIdx depth
        _ -> return ()
    Exec -> fetch >>= \case
        '>' -> modifyPtr (+ 1)
        '<' -> modifyPtr (subtract 1)
        '+' -> modifyCell (+ 1)
        '-' -> modifyCell (subtract 1)
        '.' -> do
            tell mempty{ output' = pure memRead }
            goto WaitOutput
        ',' -> do
            tell mempty{ inputNeeded' = pure True }
            goto WaitInput
        '[' -> if memRead /= 0 then pushPC else goto (Skip 0)
        ']' -> popPC
        '\0' -> goto Halt
        _ -> return ()
    WaitWrite -> goto Exec
    WaitOutput -> do
        tell mempty{ output' = pure memRead }
        when outputAck $ goto Exec
    WaitInput -> do
        tell mempty{ inputNeeded' = pure True }
        forM_ input $ \x -> writeCell x
  where
    fetch = do
        modify $ \st -> st{ pc = pc st + 1 }
        return $ ascii instr

    goto ph = modify $ \st -> st{ phase = ph }

    modifyCell f = writeCell $ f memRead
    modifyPtr f = modify $ \st -> st{ ptr = f (ptr st) }

    writeCell x = do
        tell mempty{ memWrite' = pure x }
        goto WaitWrite

ascii :: Word8 -> Char
ascii = chr . fromIntegral
