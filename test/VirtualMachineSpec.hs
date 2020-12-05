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
  describe "instructions" $ do
    context "return" $
      it "should end execution with value" $
        execute [Ret . mkOperand $ Immediate $ asWord32 181] `shouldSatisfy`  executedWithResult 181

    context "add values" $ do
      it "should return sum of values" $
        execute [Ld . mkOperand $ Immediate $ asWord32 100,
                 Add . mkOperand $ Immediate $ asWord32 25,
                 Ret . mkOperand $ Accumulator] `shouldSatisfy` executedWithResult 125

    context "load 32bit word from data" $
      it "should load word from data" $
        executeWithData
          (Vector.fromList [0, 0, 0, 100])
          [ Ld . mkOperand $ ByteOffset 0,
           Ret . mkOperand $ Accumulator] `shouldSatisfy` executedWithResult 100

    context "load from out side of data range" $
      it "should end execution with AccessOutOfBounds" $
        executeWithData
          (Vector.fromList [0, 0, 0, 0])
          [Ld . mkOperand $ ByteOffset 5,
           Ret . mkOperand $ Accumulator] `shouldSatisfy` outOfBoundsError
