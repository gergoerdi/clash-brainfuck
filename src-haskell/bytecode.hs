{-# LANGUAGE LambdaCase #-}
module Main where

import Clash.Prelude hiding (lift, (!))

import Brainfuck.Types
import Brainfuck.IO
import RetroClash.Utils (predIdx)

import Data.Word
import Control.Monad.State
import Control.Monad.Loops (whileJust_)
import Data.Array
import Data.Maybe (isJust)
import Data.List as L

data Phase
    = Exec
    | Skip Word8

data BFMachine = MkBFMachine
    { pc :: Int
    , stack :: [Int]
    , phase :: Phase
    , cells :: ([Cell], [Cell])
    }

initBFMachine = MkBFMachine
    { pc = 0
    , stack = []
    , phase = Exec
    , cells = ([], L.replicate 30000 0)
    }

interp :: (MonadBFIO m) => (Int -> m (Maybe Char)) -> StateT BFMachine m ()
interp fetch = whileJust_ fetchNext interp1
  where
    fetchNext = do
        pc <- gets pc
        modify $ \st -> st{ pc = pc + 1}
        lift $ fetch pc

interp1 :: (MonadBFIO m) => Char -> StateT BFMachine m ()
interp1 instr = gets phase >>= \case
    Skip depth -> case instr of
        '[' -> goto $ Skip (depth + 1)
        ']' -> goto $ maybe Exec Skip $ predIdx depth
        _ -> return ()
    Exec -> case instr of
        '>' -> modifyCells $ \(ls, x:rs) -> (x:ls, rs)
        '<' -> modifyCells $ \(x:ls, rs) -> (ls, x:rs)
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

    modifyCells f = modify $ \st@MkBFMachine{ cells = cells } ->
        st{ cells = f cells }

    modifyCell f = modifyCells $ \(ls, x:rs) -> (ls, f x:rs)
    setCell = modifyCell . const
    getCell = gets $ \MkBFMachine{ cells = (_, x:_) } -> x

    pushPC = modify $ \st@MkBFMachine{ pc = pc, stack = stack } ->
        st{ stack = pc:stack }
    popPC = modify $ \st@MkBFMachine{ stack = top:stack} ->
        st{ pc = top - 1, stack = stack }

hello = "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."

fetchFrom :: (Monad m) => String -> (Int -> m (Maybe Char))
fetchFrom s = \i -> return $ do
    guard $ inRange (bounds arr) i
    return $ arr!i
  where
    arr = listArray (0, L.length s - 1) s

main :: IO ()
main = do
    evalStateT (interp (fetchFrom hello)) initBFMachine
    putStrLn ""
