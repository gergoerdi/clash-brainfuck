{-# LANGUAGE RecordWildCards #-}
module Main where

import Clash.Prelude
import Brainfuck.CPU
import Brainfuck.Types

import Data.Array.IO
import qualified Data.ByteString as BS
import Data.List as L
import Data.Char
import System.IO
import Control.Monad
import Data.Foldable (for_)
import Control.Monad.State.Strict
import Control.Lens
import Data.Functor.Identity
import Data.Word

newROM :: forall addr val. (Integral addr, Bounded addr) => [val] -> IO (addr -> IO val)
newROM contents = do
    arr <- newListArray @IOArray ((0 :: Int), fromIntegral (maxBound :: addr)) contents
    return $ readArray arr . fromIntegral

newRAM :: forall addr val. (Integral addr, Bounded addr, Num val) => IO (addr -> IO val, addr -> val -> IO ())
newRAM = do
    arr <- newArray @IOArray ((0 :: Int), fromIntegral (maxBound :: addr)) 0
    return (readArray arr . fromIntegral, writeArray arr . fromIntegral)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    prog <- BS.readFile "hello.bf"

    readROM <- newROM (BS.unpack prog <> L.repeat 0)
    (readRAM, writeRAM) <- newRAM

    -- let dummyOut = CPUOut
    --         { _progAddr = 0
    --         , _memAddr = 0
    --         , _memWrite = Nothing
    --         , _output = Nothing
    --         , _inputNeeded = False
    --         }

    flip evalStateT (initBFState, defaultOutput initBFState) $ forever $ do
        (s, out@CPUOut{..}) <- get

        instr <- liftIO $ readROM _progAddr
        memRead <- liftIO $ readRAM _memAddr
        let input = CPUIn
                { outputAck = True
                , input = Nothing
                , ..
                }
        out'@CPUOut{..} <- zoom _1 $ mapStateT (return . runIdentity) $ step' input
        zoom _2 $ put out'

        for_ _output $ liftIO . putChar . chr . fromIntegral
        for_ _memWrite $ liftIO . writeRAM _memAddr
        return ()
