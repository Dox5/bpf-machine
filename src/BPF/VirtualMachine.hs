module BPF.VirtualMachine where

import BPF.Packet
import BPF.Instruction

import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Word

type Program = [Instruction]

data Machine = Machine {
   accum      :: Word32,
   instPtr    :: Int, -- Instruction pointer, mostly for debug right now
   program    :: Program,
   xRegister  :: Word32

}

data ExecutionError =
  AccessOutOfBounds -- Tried to load data outside of provided range
    Int -- Instruction pointer
    Int -- Address read 
  deriving(Show) -- TODO: Format this sensibly

type VMExecutor = State (Machine, PacketData)

type Result = Either Word32 ExecutionError

data StepResult = Continue | Completed Word32 | Error ExecutionError

makeMachine :: Program -> Machine
makeMachine prog = Machine {
  accum      = 0,
  instPtr    = 0,
  program    = prog,
  xRegister  = 0
}

incrementIP :: Int -> VMExecutor ()
incrementIP amount = do
  (vm, pkt) <- State.get 
  let nextProgram = drop amount (program vm)
  let nextIp = (instPtr vm) + amount
  State.put (vm{program = nextProgram,
                instPtr = nextIp},
             pkt)

class Context a where
  evaluateOperand :: Operand -> VMExecutor (Either a ExecutionError)

instance Context Word32 where

  evaluateOperand Accumulator = do
    (vm, _) <- State.get
    return . Left . accum $ vm

  evaluateOperand (ByteOffset i) = do
    (vm, pkt) <- State.get
    case sliceAsWord pkt 4 i of
      (Nothing) -> return . Right $ AccessOutOfBounds (instPtr vm) i
      (Just v) -> return . Left $ v

  evaluateOperand (Immediate k) = return . Left $ k

  evaluateOperand XRegister = do
    (vm, _) <- State.get
    return . Left . xRegister $ vm

executeInstruction :: VMExecutor StepResult
executeInstruction = do
  (vm, pkt) <- State.get
  let inst = head . program $ vm

  case inst of
    (Ret op) -> do
      valOrErr <- evaluateOperand op
      return $ either (\v -> Completed v) (\err -> Error err) valOrErr

    (Ld op) -> do
      valOrErr <- evaluateOperand op
      either
        (\v -> do State.put (vm{accum = v}, pkt); return Continue)
        (\err -> return $ Error err) 
        valOrErr

    (Ldx op) -> do
      valOrErr <- evaluateOperand op
      either
        (\v -> do
          State.put (vm{xRegister = v}, pkt)
          return Continue)
        (\err -> return $ Error err)
        valOrErr

    (Add op) -> do
      valOrErr <- evaluateOperand op
      either
        (\v -> do
          State.put (vm{accum = (accum vm) + v}, pkt)
          return Continue)
        (\err -> return $ Error err)
        valOrErr
  

singleStep :: State (Machine, PacketData) StepResult
singleStep = do
  res <- executeInstruction
  incrementIP 1

  return res
  

run :: PacketData -> State Machine Result
run pkt = do
  vm <- State.get
  let (res, (machine, _)) = State.runState loop (vm, pkt)
  State.put machine
  return res
  where
    loop :: State (Machine, PacketData) Result
    loop = do
      result <- singleStep
      case result of
        (Completed v) -> do
          return $ Left v
        (Error err) -> do
          return $ Right err
        (Continue) ->  loop
