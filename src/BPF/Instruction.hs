module BPF.Instruction where

import Data.Word

data Operand = Immediate Word32 | Accumulator | XRegister

data Instruction = 
  Ret Operand
  | Ld Operand
  | Ldx Operand
  | Add Operand
