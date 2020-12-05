module BPF.VirtualMachine where

import BPF.Packet
import BPF.Instruction

import Data.Word

type Program = [Instruction]

data Machine = Machine {
   accum      :: Word32,
   instPtr    :: Int, -- Instruction pointer, mostly for debug right now
   program    :: Program,
   xRegister  :: Word32

}

data ExecutionError =
  AccessOutOfBounds -- Tried to load data outside of provided range
    Int -- Instruction pointer
    Int -- Address read 
  deriving(Show, Eq) -- TODO: Format this sensibly

type ExecutionState = (Machine, PacketData)

makeMachine :: Program -> Machine
makeMachine prog = Machine {
  accum      = 0,
  instPtr    = 0,
  program    = prog,
  xRegister  = 0
}

