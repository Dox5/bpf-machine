module VirtualMachineSpec where

import BPF

import Test.Hspec

import qualified Control.Monad.State as State
import qualified Data.Vector as Vector

import Data.Word

execute :: Program -> Result
execute = executeWithData Vector.empty

executeWithData :: Vector.Vector Word8 -> Program -> Result
executeWithData pkt prog = State.evalState (run pkt) (makeMachine  prog) 

asWord32 :: Int -> Word32
asWord32 = fromIntegral

executedWithResult :: Int -> Result -> Bool
executedWithResult expected = matcher
  where
    matcher :: Result -> Bool
    matcher (Left v) = (asWord32 expected) == v
    matcher _ = False

outOfBoundsError :: Result -> Bool
outOfBoundsError (Right (AccessOutOfBounds _ _)) = True
outOfBoundsError _ = False

spec :: Spec
spec = do
  describe "return instruction" $ do
    context "return immediate value" $
      it "should return given value" $
        execute [Ret $ Immediate $ asWord32 181] `shouldSatisfy`  executedWithResult 181

    context "return accumulator value" $
      it "should return value loaded into accumulator" $
        execute [Ld $ Immediate $ asWord32 997,
                 Ret $ Accumulator] `shouldSatisfy`  executedWithResult 997

  describe "add instruction" $ do
    context "add immediate value" $
      it "should return sum of values" $
        execute [Ld $ Immediate $ asWord32 100,
                 Add $ Immediate $ asWord32 25,
                 Ret $ Accumulator] `shouldSatisfy` executedWithResult 125

    context "add x register" $
      it "should return A + X" $
        execute [Ld $ Immediate $ asWord32 10,
                 Ldx $ Immediate $ asWord32 12,
                 Add $ XRegister,
                 Ret $ Accumulator] `shouldSatisfy` executedWithResult 22

  describe "ld" $ do
    context "load from data" $
      it "should load word from data" $
        executeWithData
          (Vector.fromList [0, 0, 0, 100])
          [ Ld $ ByteOffset 0,
           Ret $ Accumulator] `shouldSatisfy` executedWithResult 100

    context "load from out side of data range" $
      it "should return AccessOutOfBounds" $
        executeWithData
          (Vector.fromList [0, 0, 0, 0])
          [Ld $ ByteOffset 5,
           Ret $ Accumulator] `shouldSatisfy` outOfBoundsError

    context "load without full word available" $
      it "should return AccessOutOfBounds" $
        executeWithData
          (Vector.fromList [0, 0, 0, 0])
          [Ld $ ByteOffset 1,
           Ret $ Accumulator] `shouldSatisfy` outOfBoundsError

    context "load with negative index" $
      it "should return AccessOutOfBounds" $
        executeWithData
          (Vector.fromList [0, 0, 0, 0])
          [Ld $ ByteOffset (-1),
           Ret $ Accumulator] `shouldSatisfy` outOfBoundsError

        



