{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Clash.Prelude hiding (lift)

import Brainfuck.Types
import Brainfuck.IO
import Brainfuck.Memory
import Brainfuck.Stack

import RetroClash.Utils (predIdx)
import Data.Word
import Data.Char (ord)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Loops (whileJust_)
import Data.Array.IO

data Phase
    = Exec
    | Skip (Index StackSize)

data BFState = MkBFState
    { pc :: PC
    , stack :: Stack StackSize PC
    , phase :: Phase
    , ptr :: Ptr
    }

initBFState = MkBFState
    { pc = 0
    , stack = Stack (repeat 0) 0
    , phase = Exec
    , ptr = 0
    }

interp :: (MonadBFMemory m, MonadBFIO m) => StateT BFState m ()
interp = whileJust_ fetchNext interp1
  where
    nextPC = do
        pc <- gets pc
        modify $ \st -> st{ pc = pc + 1}
        return pc

    fetchNext = do
        pc <- nextPC
        instr <- lift $ ascii <$> readProgROM pc
        return $ do
            guard $ instr /= '\0'
            return instr

interp1 :: (MonadBFMemory m, MonadBFIO m) => Char -> StateT BFState m ()
interp1 instr = gets phase >>= \case
    Skip depth -> case instr of
        '[' -> goto $ Skip (depth + 1)
        ']' -> goto $ maybe Exec Skip $ predIdx depth
        _ -> return ()
    Exec -> case instr of
        '>' -> modifyPtr (+ 1)
        '<' -> modifyPtr (subtract 1)
        '+' -> modifyCell (+ 1)
        '-' -> modifyCell (subtract 1)
        '.' -> do
            x <- getCell
            lift $ doOutput x
        ',' -> do
            x <- lift doInput
            setCell x
        '[' -> do
            x <- getCell
            if x /= 0 then pushPC else goto $ Skip 0
        ']' -> popPC
        _ -> return ()
  where
    goto phase = modify $ \st -> st{ phase = phase }

    modifyPtr f = modify $ \st -> st{ ptr = f (ptr st) }

    modifyCell f = do
        x <- getCell
        setCell (f x)
    getCell = do
        ptr <- gets ptr
        lift $ readRAM ptr
    setCell x = do
        ptr <- gets ptr
        lift $ writeRAM ptr x

    pushPC = do
        pc <- gets pc
        modify $ \st -> st{ stack = push pc (stack st) }
    popPC = modify $ \st ->
        let (top, stack') = pop (stack st)
        in st{ pc = top - 1, stack = stack' }

main :: IO ()
main = do
    prog <- prepareIO
    runBFVec (stringToROM $ loadVec prog '\0') $ evalStateT interp initBFState
    putStrLn ""
