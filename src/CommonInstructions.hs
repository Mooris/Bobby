module CommonInstructions where

import Data.Word
import Data.Bits
import Control.Monad.Reader

import CPU
import Memory
import Utils

rotateLeft :: CPU c => Word8 -> ReaderT c IO Word8
rotateLeft word = do
  carry <- if' 1 0 <$> isSetC
  if (word .&. 0x80) /= 0 then setC else unsetC
  let result = (shiftL word 1) .|. carry
  if result == 0 then setZ else unsetZ
  unsetN
  unsetH
  return result

relativeJump :: CPU c => Word8 -> ReaderT c IO Word8
relativeJump inc = do
  lepc <- pc
  case testBit inc 7 of
    False -> let newPC = lepc + (fromIntegral inc) in writePC newPC
    True ->
      let
        newPC = lepc
          - ((fromIntegral :: Word8 -> Word16) . complement) (clearBit inc 8)
      in  writePC (newPC - 1)
  return inc

incDecFlags :: CPU c => Word8 -> ReaderT c IO Word8
incDecFlags flags =
  unsetN
    >> (if flags .&. 0x0F == 0x00 then setH else unsetH)
    >> (if flags == 0 then setZ else unsetZ)
    >> return flags

pop :: CPU c => ReaderT c IO Word16
pop = do
  idx <- sp
  mem <- memory
  low <- liftIO $ readM mem idx
  high <- liftIO $ readM mem (idx + 1)
  _ <- writeSP (idx + 2)
  let result = (shiftL (fromIntegral high) 8) .|. (fromIntegral low)
  return result

push :: CPU c => Word16 -> ReaderT c IO String
push wd = sp >>= \idx ->
  let high = fromIntegral ((wd .&. 0xFF00) `shiftR` 8)
      low  = fromIntegral (wd .&. 0x00FF)
  in  memory >>= \mem -> liftIO (writeM mem (idx - 1) high)
        >> liftIO (writeM mem (idx - 2) low)
        >> writeSP (idx - 2)
        >> return ("PUSH " ++ prettyHex wd)

call :: CPU c => ReaderT c IO ()
call = do
  low  <- (fromIntegral :: Integral a => a -> Word16) <$> fetchInstruction
  high <- (fromIntegral :: Integral a => a -> Word16) <$> fetchInstruction
  call' high low

call' :: CPU c => Word16 -> Word16 -> ReaderT c IO ()
call' high low = do
  _ <- pc >>= push
  let word = (shiftL high 8) .|. (fromIntegral low) :: Word16
  writePC word
  return ()

