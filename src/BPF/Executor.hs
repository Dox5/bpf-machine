module BPF.Executor where

import BPF.Packet (PacketData)
import BPF.Instruction (
    Instruction (Ret, Ld, Ldx, Add)
  )

import BPF.OperandEval (
    doEvaluate32
  )

import BPF.VirtualMachine (
    ExecutionState,
    ExecutionError,
    Machine (accum, instPtr, program, xRegister),
  )

import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Word (Word32)

type VMExecutor = State ExecutionState

type Result = Either Word32 ExecutionError

data StepResult = Continue | Completed Word32 | Error ExecutionError

incrementIP :: Int -> VMExecutor ()
incrementIP amount = do
  (vm, pkt) <- State.get 
  let nextProgram = drop amount (program vm)
  let nextIp = (instPtr vm) + amount
  State.put (vm{program = nextProgram,
                instPtr = nextIp},
             pkt)

executeInstruction :: VMExecutor StepResult
executeInstruction = do
  (vm, pkt) <- State.get
  let inst = head . program $ vm

  case inst of
    (Ret op) -> do
      let valOrErr = doEvaluate32 (vm, pkt) op
      return $ either (\v -> Completed v) (\err -> Error err) valOrErr

    (Ld op) -> do
      let valOrErr = doEvaluate32 (vm, pkt) op
      either
        (\v -> do State.put (vm{accum = v}, pkt); return Continue)
        (\err -> return $ Error err) 
        valOrErr

    (Ldx op) -> do
      let valOrErr = doEvaluate32 (vm, pkt) op
      either
        (\v -> do
          State.put (vm{xRegister = v}, pkt)
          return Continue)
        (\err -> return $ Error err)
        valOrErr

    (Add op) -> do
      let valOrErr = doEvaluate32 (vm, pkt) op
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
