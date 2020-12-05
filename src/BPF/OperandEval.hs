{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module BPF.OperandEval where

import BPF.Instruction (
    AccumulatorOperand,
    ByteOffsetOperand (ByteOffset),
    ImmediateOperand (Immediate),
    XRegisterOperand,

  )

import BPF.VirtualMachine (
    Machine (accum, instPtr, xRegister), 
    ExecutionError (
      AccessOutOfBounds
    ),
    ExecutionState,
  )

import BPF.Packet (
    sliceAsWord
  )

import Data.Extensible.Match (Match(..), match)
import Data.Extensible.Product ( Forall, htabulateFor )
import Data.Extensible.Sum ( (:/) )
import Data.Functor.Identity (Identity, runIdentity)
import Data.Word (Word32)

import Data.Proxy

-- Operand evauluation in 32bit word context
class Operand32 op where
  evaluateOperand :: ExecutionState -> op -> Either Word32 ExecutionError

instance Operand32 AccumulatorOperand where
  evaluateOperand (m, _) _ = Left $ accum m

instance Operand32 ByteOffsetOperand where
  evaluateOperand (m, d) (ByteOffset offset) = 
    case sliceAsWord d 4 offset of
      (Nothing) -> Right $ AccessOutOfBounds (instPtr m) offset
      (Just v)   -> Left v

instance Operand32 ImmediateOperand where
  evaluateOperand _ (Immediate v) = Left v

instance Operand32 XRegisterOperand where
  evaluateOperand (m, _) _ = Left $ xRegister m


doEvaluate32 :: Forall Operand32 xs => ExecutionState ->  xs :/ Identity -> Either Word32 ExecutionError
doEvaluate32 state =
  let
    matcher = htabulateFor (Proxy @Operand32) (\_ -> Match (evaluateOperand state . runIdentity))
  in match matcher
