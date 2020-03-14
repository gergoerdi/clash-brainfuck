{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Clash.Prelude hiding (lift)

import Brainfuck.Types
import Brainfuck.IO
import Brainfuck.Stack

import RetroClash.Utils (predIdx)
import Data.Word
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Loops (whileJust_)
import Data.Array.IO

class (Monad m) => MonadBFMemory m where
    readProgROM :: PC -> m Char
    readRAM :: Ptr -> m Cell
    writeRAM :: Ptr -> Cell -> m ()

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
        instr <- lift $ readProgROM pc
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
            lift $ output x
        ',' -> do
            x <- lift input
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
        modify $ \st -> st{ stack = push (stack st) pc }
    popPC = modify $ \st ->
        let (top, stack') = pop (stack st)
        in st{ pc = top - 1, stack = stack' }

type ProgSize = 2 ^ BitSize PC

type WithROM = ReaderT (Vec ProgSize Char)

runWithROM :: (Monad m) => Vec ProgSize Char -> WithROM m a -> m a
runWithROM rom act = runReaderT act rom

type WithRAM = StateT (Vec MemSize Cell)

runWithRAM :: (Monad m) => WithRAM m a -> m a
runWithRAM act = evalStateT act (repeat 0)

newtype BFVec m a = BFVec{ unBFVec :: WithROM (WithRAM m) a }
    deriving (Functor, Applicative, Monad)

runBFVec :: (Monad m) => Vec ProgSize Char -> BFVec m a -> m a
runBFVec prog = runWithRAM . runWithROM prog . unBFVec

instance (Monad m) => MonadBFMemory (BFVec m) where
    readProgROM pc = BFVec $ asks (!!pc)
    readRAM addr = BFVec $ gets (!!addr)
    writeRAM addr x = BFVec $ modify $ replace addr x

instance (MonadBFIO m) => MonadBFIO (BFVec m) where
    input = BFVec . lift $ input
    output = BFVec . lift . output

unfoldr :: SNat n -> (s -> (a, s)) -> s -> Vec n a
unfoldr n f s0 = map unpack $ generate n f' (Nothing, s0)
  where
    f' (_, s) = let (x, s') = f s in (Just x, s')
    unpack (Just x, _) = x

unfoldrI :: (KnownNat n) => (s -> (a, s)) -> s -> Vec n a
unfoldrI = unfoldr SNat

loadVec :: (KnownNat n) => [a] -> a -> Vec n a
loadVec xs x0 = unfoldrI uncons xs
  where
    uncons (x:xs) = (x, xs)
    uncons [] = (x0, [])

hello = "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."

main :: IO ()
main = do
    runBFVec (loadVec hello '\0') $ evalStateT interp initBFState
    putStrLn ""
