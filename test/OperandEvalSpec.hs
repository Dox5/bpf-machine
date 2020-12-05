module OperandEvalSpec where

import BPF.OperandEval (evaluateOperand)

import BPF.VirtualMachine (
    ExecutionError (AccessOutOfBounds),
    ExecutionState,
    Machine(accum, instPtr, xRegister),
    Program,

    makeMachine,
  )

import BPF.Instruction (
    AccumulatorOperand(Accumulator),
    ByteOffsetOperand(ByteOffset),
    ImmediateOperand(Immediate),
    XRegisterOperand(XRegister),
  )

import BPF.Packet (PacketData)

import qualified Data.Vector as Vector

import Data.Word ()

import Test.Hspec

iotaBytes :: Int -> PacketData
iotaBytes n = Vector.generate n (\v -> fromIntegral $ v + 1)

evaluationResult :: Eq a => a -> Either a ExecutionError -> Bool
evaluationResult expected = either (\ actual -> expected == actual) (\ _ -> False)

evaluationError :: Either a ExecutionError -> Bool
evaluationError = either (\ _ -> False) (\ _ -> True)

noProgram :: Program
noProgram =  []

basicMachine :: Machine
basicMachine = makeMachine noProgram

basicExecutionState :: ExecutionState
basicExecutionState = (basicMachine, Vector.empty)

executionState :: (ExecutionState -> ExecutionState) -> ExecutionState
executionState f = f basicExecutionState

errorIs :: ExecutionError -> Either a ExecutionError -> Bool
errorIs expected = either (\_ -> False) (\e -> e == expected)

spec :: Spec
spec = do
  describe "AccumulatorOperand" $ do
    context "32bit word" $
      it "should return the value of the accumulator [181]" $
        evaluateOperand (executionState (\(m, d) -> (m{accum = 181}, d))) (Accumulator)
          `shouldSatisfy` evaluationResult (fromInteger 181)

  describe "ByteOffsetOperand" $ do
    context "32bit word" $ do
      context "valid bytes" $ do
          it "should return word when all bytes valid at offset 0 [0x01020304]" $
            evaluateOperand (executionState (\(m, _) -> (m, iotaBytes 4))) (ByteOffset 0)
              `shouldSatisfy` evaluationResult (fromInteger 0x01020304)

          it "should return word when all bytes valid at offset 1 [0x02030405]" $
            evaluateOperand (executionState (\(m, _) -> (m, iotaBytes 5))) (ByteOffset 1)
              `shouldSatisfy` evaluationResult (fromInteger 0x02030405)

      context "invalid bytes" $ do
        it "should return ExecutionError when no bytes valid at offset 0" $
          evaluateOperand basicExecutionState (ByteOffset 0)
            `shouldSatisfy` evaluationError

        it "should return ExecutionError when no bytes valid at offset 1" $
          evaluateOperand basicExecutionState (ByteOffset 1)
            `shouldSatisfy` evaluationError

        it "should return ExecutionError when only one byte valid at offset" $
          evaluateOperand (executionState (\(m, _) -> (m, iotaBytes 3))) (ByteOffset 1)
            `shouldSatisfy` evaluationError

        it "should return AccessOutOfBounds with IP [32] and offset [4] set" $
          evaluateOperand (executionState
              (\(m, d) -> (m{instPtr = 32}, d)))
              (ByteOffset(4))
            `shouldSatisfy`
              errorIs (AccessOutOfBounds 32 4)
  
  describe "ImmediateOperand" $ do
    context "32bit word" $
      it "should return the immediate value [1001]" $
        evaluateOperand basicExecutionState (Immediate 1001)
          `shouldSatisfy` evaluationResult (fromInteger 1001)

  describe "XRegisterOperand" $ do
    context "32bit word" $
      it "should return the value of the accumulator [999]" $
        evaluateOperand (executionState (\(m, p) -> (m{xRegister = 999}, p))) (XRegister)
          `shouldSatisfy` evaluationResult (fromInteger 999)
