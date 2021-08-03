{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Brainfuck.CPU where

import Brainfuck.Types
import Brainfuck.Stack

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Barbies
import RetroClash.CPU
import Control.Monad
import Data.Word
import Data.Foldable (traverse_)

import Data.Monoid (Last(..))

import Control.Monad.Writer
import Control.Monad.State
import Control.Lens hiding (Index, assign)

import Barbies
import Barbies.Bare
import Data.Barbie.TH

declareBareB [d|
  data CPUIn = CPUIn
      { romRead :: Word8
      , ramRead :: Cell
      , outputAck :: Bool
      , input :: Maybe Cell
      } |]

declareBareB [d|
  data CPUOut = CPUOut
      { _romAddr :: PC
      , _ramAddr :: Ptr
      , _ramWrite :: Maybe Cell
      , _output :: Maybe Cell
      , _inputNeeded :: Bool
      } |]
makeLenses ''CPUOut

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
    { _phase :: Phase
    , _pc :: PC
    , _stack :: (Stack StackSize PC)
    , _ptr :: Ptr
    }
    deriving (Generic, NFDataX)
makeLenses ''CPUState

initCPUState :: CPUState
initCPUState = CPUState
    { _phase = Init
    , _pc = 0
    , _stack = Stack (pure 0) 0
    , _ptr = 0
    }

defaultOutput :: CPUState -> Pure CPUOut
defaultOutput CPUState{..} = CPUOut
    { _romAddr = _pc
    , _ramAddr = _ptr
    , _ramWrite = Nothing
    , _output = Nothing
    , _inputNeeded = False
    }

cpu :: (HiddenClockResetEnable dom) => Signals dom CPUIn -> Signals dom CPUOut
cpu = mealyCPU initCPUState defaultOutput step

cpuMachine :: Pure CPUIn -> State CPUState (Pure CPUOut)
cpuMachine = runCPU defaultOutput . step

type CPU = CPUM CPUState CPUOut

pushPC :: CPU ()
pushPC = do
    pc <- use pc
    stack %= push (pc - 1)

popPC :: CPU ()
popPC = do
    (pc', stack') <- uses stack pop
    pc .= pc'
    stack .= stack'

step :: Pure CPUIn -> CPU ()
step CPUIn{..} = use phase >>= \case
    Halt -> return ()
    Init -> phase .= Exec
    Skip depth -> fetch >>= \case
        '[' -> phase .= Skip (depth + 1)
        ']' -> phase .= maybe Exec Skip (predIdx depth)
        _ -> return ()
    Exec -> fetch >>= \case
        '>' -> ptr %= nextIdx
        '<' -> ptr %= prevIdx
        '+' -> writeCell $ nextIdx ramRead
        '-' -> writeCell $ prevIdx ramRead
        '.' -> outputCell ramRead
        ',' -> startInput
        '[' -> if ramRead /= 0 then pushPC else phase .= Skip 0
        ']' -> popPC
        '\0' -> phase .= Halt
        _ -> return ()
    WaitWrite -> phase .= Exec
    WaitOutput -> when outputAck $ phase .= Exec
    WaitInput -> traverse_ writeCell input
  where
    fetch = do
        pc += 1
        return $ ascii romRead

outputCell :: Cell -> CPU ()
outputCell x = do
    output .:= Just x
    phase .= WaitOutput

writeCell :: Cell -> CPU ()
writeCell x = do
    ramWrite .:= Just x
    phase .= WaitWrite

startInput :: CPU ()
startInput = do
    inputNeeded .:= True
    phase .= WaitInput
