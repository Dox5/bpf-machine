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
    AccumulatorOperand (Accumulator),
    BankSelector (Bank2),
    ByteOffsetOperand (ByteOffset),
    ByteXOffsetOperand (ByteXOffset),
    ImmediateOperand (Immediate),
    MemoryBankOperand (MemoryBank),
    XRegisterOperand (XRegister),

--    bankIndex,
  )

import BPF.Packet (PacketData)

import qualified Data.Vector as Vector

import Data.Word (Word32)

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

executionState :: ExecutionState
executionState = (basicMachine, Vector.empty)

errorIs :: ExecutionError -> Either a ExecutionError -> Bool
errorIs expected = either (\_ -> False) (\e -> e == expected)

setX :: Word32 -> ExecutionState -> ExecutionState
setX x (m, d) = (m{xRegister = x}, d)

setAccum :: Word32 -> ExecutionState -> ExecutionState
setAccum a (m, d) = (m{accum = a}, d)

setIotaData :: Int -> ExecutionState -> ExecutionState
setIotaData c (m, _) = (m, iotaBytes c)

setIP :: Int -> ExecutionState -> ExecutionState
setIP ip (m, d) = (m{instPtr = ip}, d)

setMemory :: BankSelector -> Word32 -> ExecutionState -> ExecutionState
setMemory _ _ (m, d) = 
  let
    m' = m
  in  (m', d)

spec :: Spec
spec = do
  describe "XRegisterOperand" $ do
    context "32bit word" $
      it "should return the value of the accumulator [999]" $
        evaluateOperand (setX 999 $ executionState) (XRegister)
          `shouldSatisfy` evaluationResult (fromInteger 999)

  describe "ByteOffsetOperand" $ do
    context "32bit word" $ do
      context "valid bytes" $ do
          it "should return word when all bytes valid at offset 0 [0x01020304]" $
            evaluateOperand (setIotaData 4 $ executionState) (ByteOffset 0)
              `shouldSatisfy` evaluationResult (fromInteger 0x01020304)

          it "should return word when all bytes valid at offset 1 [0x02030405]" $
            evaluateOperand (setIotaData 5 $ executionState) (ByteOffset 1)
              `shouldSatisfy` evaluationResult (fromInteger 0x02030405)

      context "invalid bytes" $ do
        it "should return ExecutionError when no bytes valid at offset 0" $
          evaluateOperand executionState (ByteOffset 0)
            `shouldSatisfy` evaluationError

        it "should return ExecutionError when no bytes valid at offset 1" $
          evaluateOperand executionState (ByteOffset 1)
            `shouldSatisfy` evaluationError

        it "should return ExecutionError when only one byte valid at offset" $
          evaluateOperand (setIotaData 3 $ executionState) (ByteOffset 2)
            `shouldSatisfy` evaluationError

        it "should return AccessOutOfBounds with IP [32] and offset [4] set" $
          evaluateOperand (setIP 32 $ executionState) (ByteOffset(4))
            `shouldSatisfy`
              errorIs (AccessOutOfBounds 32 4)

  describe "ByteXOffsetOperand" $ do
    context "32bit word" $ do
      context "valid bytes" $ do
          it "should return word when all bytes valid at offset 0 (x = 0, k = 0) [0x01020304]" $
            evaluateOperand (setIotaData 4 $ executionState) (ByteXOffset 0)
              `shouldSatisfy` evaluationResult (fromInteger 0x01020304)

          it "should return word when all bytes valid at offset 3 (x = 2, k = 1) [0x02030405]" $
            evaluateOperand (setX 2 . setIotaData 8 $ executionState) (ByteXOffset 1)
              `shouldSatisfy` evaluationResult (fromInteger 0x04050607)

      context "invalid bytes" $ do
        it "should return ExecutionError when no bytes valid at offset 0 (x = 0, k = 0)" $
          evaluateOperand executionState (ByteXOffset 0)
            `shouldSatisfy` evaluationError

        it "should return ExecutionError when no bytes valid at offset 8 (x = 5, k = 3)" $
          evaluateOperand (setX 5 $ executionState) (ByteXOffset 3)
            `shouldSatisfy` evaluationError

        it "should return ExecutionError when only one byte valid at offset" $
          evaluateOperand (setX 2 . setIotaData 4 $ executionState) (ByteXOffset 1)
            `shouldSatisfy` evaluationError

        it "should return AccessOutOfBounds with IP [32] and offset [5] set" $
          evaluateOperand (setX 1 . setIP 32 $ executionState) (ByteXOffset(4))
            `shouldSatisfy`
              errorIs (AccessOutOfBounds 32 5)

  describe "MemoryBankOperand" $ do
    context "fetch stored word" $ do
      it "should return expected word [1221] from memory" $
        evaluateOperand
            (setMemory Bank2 1221 $ executionState)
            (MemoryBank Bank2)
        `shouldSatisfy`
          evaluationResult (fromInteger 1221)

  
  describe "ImmediateOperand" $ do
    context "32bit word" $
      it "should return the immediate value [1001]" $
        evaluateOperand executionState (Immediate 1001)
          `shouldSatisfy` evaluationResult (fromInteger 1001)

  describe "AccumulatorOperand" $ do
    context "32bit word" $
      it "should return the value of the accumulator [181]" $
        evaluateOperand (setAccum 181 $ executionState) (Accumulator)
          `shouldSatisfy` evaluationResult (fromInteger 181)


