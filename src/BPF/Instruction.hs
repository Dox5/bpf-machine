module BPF.Instruction where

import Data.Word

data Operand =
  Accumulator |
  ByteOffset Int |
  Immediate Word32 |
  XRegister

data Instruction = 
  Ret Operand
  | Ld Operand
  | Ldx Operand
  | Add Operand
