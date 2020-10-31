module BPF.Packet where

import Data.Word
import Data.Bits
import qualified Data.Vector as Vector

type PacketData = Vector.Vector Word8

toWord :: PacketData -> Word32
toWord pkt = f 0 (fromInteger 0)
  where
    shiftAmount :: Int -> Int
    shiftAmount n = 8 * ((Vector.length pkt) - n - 1)

    f :: Int -> Word32 -> Word32
    f n total =
      if n == Vector.length pkt then
        total
      else
        let
          byte = fromIntegral (pkt Vector.! n) :: Word32
          val = byte `shiftL` (shiftAmount n)
        in f (n + 1) (total + val)


-- Take slice [start, start + n) from pkt and return as a Word32
-- Will be placed in the 'low' end of the word
sliceAsWord :: PacketData -> Int -> Int -> Maybe Word32
sliceAsWord pkt n start =
  let
    end = start + n
    notNegative = start >= 0 && end >= 0
    startBeforeEnd = start < end
    withinData = end <= Vector.length pkt
  in
    if notNegative && startBeforeEnd && withinData then
      let
        bytes = Vector.slice start n pkt 
      in Just $ toWord bytes
    else
      Nothing
