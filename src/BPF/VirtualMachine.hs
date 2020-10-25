module BPF.VirtualMachine where

import BPF.Instruction

import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Word

type Program = [Instruction]

data Machine = Machine {
   accum      :: Word32,
   program    :: Program,
   xRegister :: Word32
}

makeMachine :: Program -> Machine
makeMachine prog = Machine {
  accum      = 0,
  program    = prog,
  xRegister  = 0
}

incrementIP :: Int -> State Machine ()
incrementIP amount = do
  vm <- State.get 
  let nextProgram = drop amount (program vm)
  State.put vm{program = nextProgram}

evaluateOperand :: Operand -> State Machine (Word32)

evaluateOperand (Immediate k) = return k

evaluateOperand Accumulator = do
  vm <- State.get
  return . accum $ vm

evaluateOperand XRegister = do
  vm <- State.get
  return . xRegister $ vm


executeInstruction :: State Machine (Maybe Word32)
executeInstruction = do
  vm <- State.get
  let inst = head . program $ vm

  case inst of
    (Ret op) -> do
      val <- evaluateOperand op
      return $ Just val

    (Ld op) -> do
      val <- evaluateOperand op
      State.put vm{accum = val}
      return Nothing

    (Ldx op) -> do
      val <- evaluateOperand op
      State.put vm{xRegister = val}
      return Nothing

    (Add op) -> do
      val <- evaluateOperand op
      State.put vm{accum = (accum vm) + val}
      return Nothing
  

singleStep :: State Machine (Maybe Word32)
singleStep = do
  maybeRes <- executeInstruction
  incrementIP 1

  return maybeRes
  


run :: State Machine (Word32)
run = do
  result <- singleStep
  case result of
    (Just v) -> do
      return v
    (Nothing) ->  run
