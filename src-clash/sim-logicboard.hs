module Main where

import Clash.Prelude
import Brainfuck
import Brainfuck.Types (Cell)
import Brainfuck.IO
import Brainfuck.Memory (ProgSize, stringToROM)

import Control.Concurrent
import Data.Word
import System.IO
import Control.Monad
import Data.Foldable (traverse_)
import System.IO.Temp
import Data.List as L

driveIO :: ([i] -> [o]) -> i -> IO ((o -> IO i) -> IO ())
driveIO f input0 = do
    inChan <- newChan
    writeChan inChan input0

    ins <- getChanContents inChan
    outs <- newMVar $ f ins

    return $ \world -> do
        modifyMVar_ outs $ \(out:outs) -> do
            world out >>= writeChan inChan
            return outs

world :: (MonadBFIO m) => (Bool, Maybe Cell) -> m (Maybe Cell, Bool)
world (inputNeeded, output) = do
    traverse_ doOutput output
    input <- if inputNeeded then Just <$> doInput else return Nothing
    return (input, True)

main :: IO ()
main = withSystemTempFile "brainfuck-.rom" $ \romFile romHandle -> do
    prog <- stringToROM <$> prepareIO
    hPutStr romHandle $ unlines $ binLines (Just (snatToNum (SNat @ProgSize))) prog
    hClose romHandle

    sim <- driveIO (simulateB @System (uncurry $ logicBoard romFile)) undefined
    forever $ sim world

binLines :: Maybe Int -> [Word8] -> [String]
binLines size bs = L.map (L.filter (/= '_') . show . pack) bytes
  where
    bytes = maybe id ensureSize size bs
    ensureSize size bs = L.take size $ bs <> L.repeat 0x00
