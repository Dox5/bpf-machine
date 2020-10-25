module VirtualMachineSpec where

import BPF.VirtualMachine
import BPF.Instruction

import Test.Hspec

import qualified Control.Monad.State as State
import Data.Word

execute :: Program -> Word32
execute prog = State.evalState run  (makeMachine  prog) 

asWord32 :: Int -> Word32
asWord32 = fromIntegral

spec :: Spec
spec = do
  describe "return instruction" $ do
    context "return immediate value" $
      it "should return given value" $
        execute [Ret $ Immediate $ asWord32 181] `shouldSatisfy`  (== 181)

    context "return accumulator value" $
      it "should return value loaded into accumulator" $
        execute [Ld $ Immediate $ asWord32 997,
                 Ret $ Accumulator] `shouldSatisfy`  (== 997)

  describe "add instruction" $ do
    context "add immediate value" $
      it "should return sum of values" $
        execute [Ld $ Immediate $ asWord32 100,
                 Add $ Immediate $ asWord32 25,
                 Ret $ Accumulator] `shouldSatisfy` (== 125)

    context "add x register" $
      it "should return A + X" $
        execute [Ld $ Immediate $ asWord32 10,
                 Ldx $ Immediate $ asWord32 12,
                 Add $ XRegister,
                 Ret $ Accumulator] `shouldSatisfy` (== 22)
