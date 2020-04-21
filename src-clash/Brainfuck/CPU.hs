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

import Control.Monad.Writer
import Control.Monad.State
import Control.Lens hiding (Index, assign)

import Barbies
import Barbies.Bare
import Data.Barbie.TH

(.:=) :: (Applicative f, MonadWriter (Barbie b f) m) => Setter' (b f) (f a) -> a -> m ()
fd .:= x = scribe (iso getBarbie Barbie . fd) (pure x)

type Pure b = b Bare Identity
type Partial b = Barbie (b Covered) Last

update :: (BareB b, ApplicativeB (b Covered)) => Pure b -> Partial b -> Pure b
update initials edits = bstrip $ bzipWith update1 (bcover initials) (getBarbie edits)
  where
    update1 :: Identity a -> Last a -> Identity a
    update1 initial edit = maybe initial Identity (getLast edit)

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

cpu :: (HiddenClockResetEnable dom) => CPUIn Covered (Signal dom) -> CPUOut Covered (Signal dom)
cpu = bdistribute' . fmap bcover . mealyState cpuMachine initCPUState . fmap bstrip . bsequence'

cpuMachine :: Pure CPUIn -> State CPUState (Pure CPUOut)
cpuMachine inp = do
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
