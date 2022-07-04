{-# LANGUAGE  DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

module BPF.Instruction where

import Data.Word (Word32)

import Data.Extensible.Class (type (∈ ))
import Data.Extensible.Plain (OneOf, bury)
import Data.Extensible.Sum ((:/))
import Data.Functor.Identity

-- 'types' from table in kernel docs for BPF
-- type 0
data XRegisterOperand   = XRegister
  deriving (Show)

-- type 1
data ByteOffsetOperand  = ByteOffset Int
  deriving (Show)

-- type 2
data ByteXOffsetOperand = ByteXOffset Int
  deriving (Show)

-- type 3
data BankSelector = Bank0  | Bank1  | Bank2  | Bank3  |
                    Bank4  | Bank5  | Bank6  | Bank7  |
                    Bank8  | Bank9  | Bank10 | Bank11 |
                    Bank12 | Bank13 | Bank14 | Bank15
  deriving (Show, Eq)

bankIndex :: BankSelector -> Int
bankIndex Bank0  = 0
bankIndex Bank1  = 1
bankIndex Bank2  = 2
bankIndex Bank3  = 3
bankIndex Bank4  = 4
bankIndex Bank5  = 5
bankIndex Bank6  = 6
bankIndex Bank7  = 7
bankIndex Bank8  = 8
bankIndex Bank9  = 9
bankIndex Bank10 = 10
bankIndex Bank11 = 11
bankIndex Bank12 = 12
bankIndex Bank13 = 13
bankIndex Bank14 = 14
bankIndex Bank15 = 15

data MemoryBankOperand = MemoryBank BankSelector
  deriving (Show)

-- type 4
data ImmediateOperand   = Immediate Word32
  deriving (Show)

-- type 11
data AccumulatorOperand = Accumulator
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

