module CBInstructions where

import Data.Word
import Data.Bits
import Control.Monad.Reader

import CommonInstructions
import CPU

doCB :: CPU c =>Word8 -> Word8 -> Word8 -> ReaderT c IO String
--doCB cpu 0 0 z = return ("RLC r[" ++ show z ++ "]")
--doCB cpu 0 1 z = return ("RRC r[" ++ show z ++ "]")
doCB 0 2 z = getReg z >>= rotateLeft >>= writeReg z >> return ("RL  r[" ++ show z ++ "]")
--doCB cpu 0 3 z = return ("RR  r[" ++ show z ++ "]")
--doCB cpu 0 4 z = return ("SLA r[" ++ show z ++ "]")
--doCB cpu 0 5 z = return ("SRA r[" ++ show z ++ "]")
--doCB cpu 0 6 z = return ("SWAP r[" ++ show z ++ "]")
--doCB cpu 0 7 z = return ("SRL r[" ++ show z ++ "]")
doCB 1 y z = getReg z >>= \leBit ->
  (if testBit leBit (fromIntegral y) then unsetZ else setZ)
    >> unsetN
    >> setH
    >> return ("BIT " ++ show y ++ " r[" ++ show z ++ "]")
--doCB cpu 2 y z = return ("RES " ++ show y ++ " r[" ++ show z ++ "]")
--doCB cpu 3 y z = return ("SET " ++ show y ++ " r[" ++ show z ++ "]")
doCB _ _ _ = let loop = loop in loop --return "Unknown"