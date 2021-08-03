module Main where

import Clash.Prelude hiding (lift)
import Brainfuck.IO
import Brainfuck.Types

import Data.Word
import Control.Monad.State
import Control.Monad.Loops (whileM_)
import Data.List as L

data BF
    = IncrPtr
    | DecrPtr
    | Incr
    | Decr
    | Output
    | Input
    | While [BF]
    deriving Show

interp :: (MonadBFIO m) => [BF] -> StateT ([Cell], [Cell]) m ()
interp = mapM_ $ \instr -> case instr of
    IncrPtr -> modify $ \(ls, x:rs) -> (x:ls, rs)
    DecrPtr -> modify $ \(x:ls, rs) -> (ls, x:rs)
    Incr -> modifyCell (+ 1)
    Decr -> modifyCell (subtract 1)
    Output -> do
        x <- getCell
        lift $ doOutput x
    Input -> do
        x <- lift doInput
        setCell x
    While prog -> whileM_ ((/= 0) <$> getCell) $ interp prog
  where
    getCell = gets $ \(_, x:_) -> x
    setCell x = modify $ \(ls, _:rs) -> (ls, x:rs)
    modifyCell f = modify $ \(ls, x:rs) -> (ls, f x:rs)

parse :: String -> (String, [BF])
parse [] = ([], [])
parse (c:cs) = case c of
    '>' -> one IncrPtr
    '<' -> one DecrPtr
    '+' -> one Incr
    '-' -> one Decr
    '.' -> one Output
    ',' -> one Input
    '[' -> case parse cs of
        (cs', instrs) -> fmap (While instrs:) $ parse cs'
    ']' -> (cs, [])
    _ -> parse cs
  where
    one instr = fmap (instr:) $ parse cs

main :: IO ()
main = do
    prog <- snd . parse <$> prepareIO
    evalStateT (interp prog) ([], L.replicate 30000 0)
    putStrLn ""
