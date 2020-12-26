{-# LANGUAGE  DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

module BPF.Instruction where

import Data.Word (Word32)

import Data.Extensible.Class (type (∈ ))
import Data.Extensible.Plain (OneOf, bury)
import Data.Extensible.Sum ((:/))
import Data.Functor.Identity

data AccumulatorOperand = Accumulator
  deriving (Show)

data ByteOffsetOperand  = ByteOffset Int
  deriving (Show)

data ByteXOffsetOperand = ByteXOffset Int
  deriving (Show)

data ImmediateOperand   = Immediate Word32
  deriving (Show)

data XRegisterOperand   = XRegister
  deriving (Show)


type ReturnOperand = OneOf '[AccumulatorOperand, ImmediateOperand]
type LoadOperand = OneOf '[ImmediateOperand, ByteOffsetOperand]
type ALUOperand = OneOf '[XRegisterOperand, ImmediateOperand]

data Instruction = 
     Ret ReturnOperand
   | Ld  LoadOperand
   | Ldx LoadOperand
   | Add ALUOperand

mkOperand :: (x ∈ xs ) => x -> xs :/ Identity
mkOperand = bury

