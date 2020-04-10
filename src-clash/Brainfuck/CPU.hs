{-# LANGUAGE RecordWildCards, LambdaCase, NumericUnderscores #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
module Brainfuck.CPU where

import Brainfuck.Types
import Brainfuck.Stack

import Clash.Prelude
import RetroClash.Utils
import Control.Monad
import Data.Word
import Data.Foldable (traverse_)

import Data.Monoid (Last(..))

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Lens hiding (Index, assign)

import Barbies
import Barbies.Bare
import Data.Barbie.TH

(.:=) :: (Applicative f, MonadWriter (Barbie b f) m) => Setter' (b f) (f a) -> a -> m ()
fd .:= x = scribe (iso getBarbie Barbie . fd) (pure x)

type Raw b = b Bare Identity
type Partial b = Barbie (b Covered) Last

update :: (BareB b, ApplicativeB (b Covered)) => Raw b -> Partial b -> Raw b
update initials edits = bstrip $ bzipWith update1 (bcover initials) (getBarbie edits)
  where
    update1 :: Identity a -> Last a -> Identity a
    update1 initial edit = maybe initial Identity (getLast edit)

data CPUIn = CPUIn
    { instr :: !Word8
    , memRead :: !Cell
    , outputAck :: !Bool
    , input :: !(Maybe Cell)
    }

declareBareB [d|
  data CPUOut = CPUOut
      { _progAddr :: PC
      , _memAddr :: Ptr
      , _memWrite :: Maybe Cell
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
    { _phase :: !Phase
    , _pc :: !PC
    , _stack :: !(Stack StackSize PC)
    , _ptr :: !Ptr
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

defaultOutput :: CPUState -> Raw CPUOut
defaultOutput CPUState{..} = CPUOut
    { _progAddr = _pc
    , _memAddr = _ptr
    , _memWrite = Nothing
    , _output = Nothing
    , _inputNeeded = False
    }

cpu :: (HiddenClockResetEnable dom) => Signal dom CPUIn -> Signal dom (Raw CPUOut)
cpu = mealyState cpuIO initCPUState

cpuIO :: CPUIn -> State CPUState (Raw CPUOut)
cpuIO inp = do
    edits <- execWriterT (step inp)
    out0 <- gets defaultOutput
    return $ update out0 edits

type CPU = WriterT (Barbie (CPUOut Covered) Last) (State CPUState)

pushPC :: CPU ()
pushPC = do
    pc <- use pc
    stack %= push (pc - 1)

popPC :: CPU ()
popPC = do
    (pc', stack') <- uses stack pop
    pc .= pc'
    stack .= stack'

step :: CPUIn -> CPU ()
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
        '+' -> writeCell $ nextIdx memRead
        '-' -> writeCell $ prevIdx memRead
        '.' -> outputCell memRead
        ',' -> startInput
        '[' -> if memRead /= 0 then pushPC else phase .= Skip 0
        ']' -> popPC
        '\0' -> phase .= Halt
        _ -> return ()
    WaitWrite -> phase .= Exec
    WaitOutput -> when outputAck $ phase .= Exec
    WaitInput -> traverse_ writeCell input
  where
    fetch = do
        pc += 1
        return $ ascii instr

outputCell :: Cell -> CPU ()
outputCell x = do
    output .:= Just x
    phase .= WaitOutput

writeCell :: Cell -> CPU ()
writeCell x = do
    memWrite .:= Just x
    phase .= WaitWrite

startInput :: CPU ()
startInput = do
    inputNeeded .:= True
    phase .= WaitInput
