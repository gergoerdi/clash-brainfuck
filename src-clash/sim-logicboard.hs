module Main where

import Clash.Prelude
import Brainfuck
import Brainfuck.IO
import Brainfuck.Memory (ProgSize)

import Control.Concurrent
import Data.Char
import System.IO
import Control.Monad
import Data.Foldable (traverse_)
import System.IO.Temp
import qualified Data.ByteString as BS
import Data.List as L

main :: IO ()
main = withSystemTempFile "brainfuck-.rom" $ \romFile romHandle -> do
    prog <- BS.pack . L.map (fromIntegral . ord) <$> prepareIO
    hPutStr romHandle $ unlines $ binLines (Just (snatToNum (SNat @ProgSize))) prog
    hClose romHandle

    inChan <- newChan
    writeChan inChan (Nothing, False)
    is <- getChanContents inChan

    forM_ (simulateB @System (uncurry $ logicBoard romFile) is) $ \(inputNeeded, output) -> do
        traverse_ doOutput output
        input <- if inputNeeded then Just <$> doInput else return Nothing

        writeChan inChan (input, True)

binLines :: Maybe Int -> BS.ByteString -> [String]
binLines size bs = L.map (L.filter (/= '_') . show . pack) bytes
  where
    bytes = maybe id ensureSize size $ BS.unpack bs
    ensureSize size bs = L.take size $ bs <> L.repeat 0x00
