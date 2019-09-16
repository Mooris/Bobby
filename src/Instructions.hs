module Instructions where

import CPU
import Memory
import Data.Bits
import Data.Word
import Utils
import Control.Applicative
import Control.Monad.Reader
import CBInstructions
import CommonInstructions

ld_nn_sp :: CPU c => ReaderT c IO String
ld_nn_sp = do                          -- LD (nn), SP
    firstHalf  <- fetchInstruction
    secondHalf <- fetchInstruction
    let result = (shiftL (fromIntegral secondHalf :: Word16) 8) .|. (fromIntegral firstHalf :: Word16)
    spreg <- sp
    mem <- memory
    _ <- liftIO $ writeM mem result (fromIntegral (shiftR (spreg .&. 0xFF00) 8))
    _ <- liftIO $ writeM mem (result + 1) (fromIntegral (spreg .&. 0x00FF))
    return $ "LD (" ++ prettyHex result ++ "), " ++ prettyHex spreg ++ " [SP]"

stop :: CPU c => ReaderT c IO String
stop = let loop = loop in loop -- STOP

rla :: CPU c => ReaderT c IO String
rla = regA >>= rotateLeft >>= writeA >> return "RLA"
      
jr :: CPU c => ReaderT c IO String
jr = fetchInstruction >>= \off -> (++) "JR " . prettyHex <$> relativeJump off

jr_nz :: CPU c => ReaderT c IO String
jr_nz = isSetZ >>= \case
              True  -> ((++) "JR NZ, ") . prettyHex <$> fetchInstruction
              False -> fetchInstruction >>= relativeJump >>= return . ((++) "JR NZ, ") . prettyHex

jr_z :: CPU c => ReaderT c IO String
jr_z = isSetZ >>= \case
              False -> ((++) "JR Z, ") . prettyHex <$> fetchInstruction
              True  -> fetchInstruction >>= relativeJump >>= return . ((++) "JR Z, ") . prettyHex

jr_nc :: CPU c => ReaderT c IO String
jr_nc = isSetC >>= \case
              True  -> ((++) "JR NC, ") . prettyHex <$> fetchInstruction
              False -> fetchInstruction >>= relativeJump >>= return . ((++) "JR NC, ") . prettyHex

jr_c :: CPU c => ReaderT c IO String
jr_c = isSetC >>= \case
              True  -> ((++) "JR C, ") . prettyHex <$> fetchInstruction
              False -> fetchInstruction >>= relativeJump >>= return . ((++) "JR C, ") . prettyHex

ld_rp_nn :: CPU c => Word8 -> ReaderT c IO String
ld_rp_nn p = do
  low  <- (fromIntegral :: Integral a => a -> Word16) <$> fetchInstruction
  high <- (fromIntegral :: Integral a => a -> Word16) <$> fetchInstruction
  let imm = (shiftL high 8) .|. low :: Word16
  writeRP p imm
  return $ "LD RP[" ++ show p ++ "], " ++ prettyHex imm

add_hl_rp :: CPU c => Word8 -> ReaderT c IO String
add_hl_rp p = do
  reg <- (fromIntegral :: Integral a => a -> Word32) <$> getLongReg p
  hl <- (fromIntegral :: Integral a => a -> Word32) <$> regHL
  let result = reg + hl :: Word32
  if (result .&. 0xFFFF0000) /= 0 then setC else unsetC
  let truncated = (fromIntegral result) :: Word16
  writeHL truncated
  if ((truncated .&. 0x0f) + ((fromIntegral reg) .&. 0x0f)) > 0x0f then setH else unsetH
  unsetN
  return "ADD_HL_RP"

  

ld_bc_a :: CPU c => ReaderT c IO String
ld_bc_a = memory >>= \mem ->
  regBC >>= \idx -> regA >>= liftIO . writeM mem idx  >> return "LD (DE), A"
  
ld_de_a :: CPU c => ReaderT c IO String
ld_de_a = memory >>= \mem ->
  regDE >>= \idx -> regA >>= liftIO . writeM mem idx  >> return "LD (DE), A"
      
      
ld_hlpp_a :: CPU c => ReaderT c IO String
ld_hlpp_a = do
  idx <- regHL
  a   <- regA
  mem <- memory
  _ <- liftIO $ writeM mem idx a
  _ <- writeHL (idx + 1)
  return "LD (HL+), A"

ld_hlmm_a :: CPU c => ReaderT c IO String
ld_hlmm_a = do
  idx <- regHL
  a   <- regA
  mem <- memory
  _ <- liftIO $ writeM mem idx a
  _ <- writeHL (idx - 1)
  return "LD (HL-), A"

ld_a_bc :: CPU c => ReaderT c IO String
ld_a_bc = memory >>= \mem ->
  regBC >>= liftIO . readM mem >>= writeA >> return "LD A, (BC)"

ld_a_de :: CPU c => ReaderT c IO String
ld_a_de = memory >>= \mem ->
  regDE >>= liftIO . readM mem >>= writeA >> return "LD A, (DE)"

ld_a_hlpp :: CPU c => ReaderT c IO String
ld_a_hlpp = do
  idx <- regHL
  mem <- memory
  val <- liftIO $ readM mem idx
  _ <- writeHL (idx + 1)
  _ <- writeA val
  return "LD A, (HL+)"

ld_a_hlmm :: CPU c => ReaderT c IO String
ld_a_hlmm = do
  idx <- regHL
  mem <- memory
  val <- liftIO $ readM mem idx
  _ <- writeHL (idx - 1)
  _ <- writeA val
  return "LD A, (HL-)"
  

inc_rp :: CPU c => Word8 -> ReaderT c IO String
inc_rp p = 
  (flip (+) 1) <$> getLongReg p >>= writeLongReg p >> return
    ("INC rp[" ++ show p ++ "]")

dec_rp :: CPU c => Word8 -> ReaderT c IO String
dec_rp p =
  (flip (-) 1) <$> getLongReg p >>= writeLongReg p >> return
  ("DEC rp[" ++ show p ++ "]")

inc_r :: CPU c => Word8 -> ReaderT c IO String
inc_r y = 
  ((flip (+) 1) <$> getReg y) >>= incDecFlags >>= writeReg y >> return
    ("INC r[" ++ show y ++ "]")

dec_r :: CPU c => Word8 -> ReaderT c IO String
dec_r y =
  ((flip (-) 1) <$> getReg y) >>= incDecFlags >>= writeReg y >> return
    ("DEC r[" ++ show y ++ "]")
    
ld_r_n :: CPU c => Word8 -> ReaderT c IO String
ld_r_n y = do
  imm <- fetchInstruction
  writeReg y imm
  return $ "LD r[" ++ show y ++ "], " ++ prettyHex imm

halt :: CPU c => ReaderT c IO String
halt = let loop = loop in loop -- return "HALT"

ld_r_r :: CPU c => Word8 -> Word8 -> ReaderT c IO String
ld_r_r y z = getReg z >>= writeReg y >> return ("LD r[" ++ show y ++ "], r[" ++ show z ++ "]")

add_a :: CPU c => Word8 -> ReaderT c IO String
add_a z = regA >>= \a -> getReg z >>= flgs a >>= writeA >> return "ADD A"
  where
   flgs a other = do
     let result = (fromIntegral a + fromIntegral other) :: Word16
     if result == 0 then setZ else unsetZ
     if (result .&. 0xFF00) /= 0 then setC else unsetC
     if (result .&. 0x0f) /= 0 then setH else unsetH
     unsetN
     return (fromIntegral result :: Word8)

adc_a :: CPU c => Word8 -> ReaderT c IO String
adc_a wd = do
  a <- regA
  carry <- if' 1 0 <$> isSetC
  let result = (fromIntegral a + fromIntegral wd + fromIntegral carry) :: Word16
  if result == 0 then setZ else unsetZ
  if result > 0xFF then setC else unsetC
  if (result .&. 0x0F) + ((fromIntegral wd) .&. 0x0F) + carry > 0x0f then setH else unsetH
  unsetN
  writeA (fromIntegral result :: Word8)
  return ("ADC A, " ++ prettyHex wd)

sub_a_r :: CPU c => Word8 -> ReaderT c IO String
sub_a_r reg = getReg reg >>= sub_a

sub_a :: CPU c => Word8 -> ReaderT c IO String
sub_a wd = do
  setN
  a <- regA
  if wd > a then setC else unsetC
  if (wd .&. 0x0F) > (a .&. 0x0F) then setH else unsetH
  let result = a - wd
  if result == 0 then setZ else unsetZ
  writeA result
  return ("SUB A, " ++ prettyHex wd)

cp :: CPU c => Word8 -> ReaderT c IO String
cp wd = do
  a <- regA
  setN
  if a < wd then setC else unsetC
  if a == wd then setZ else unsetZ
  if (a - wd) .&. 0xF > a .&. 0xF then setH else unsetH
  return ("CP a: " ++ prettyHex a ++ " op: " ++ prettyHex wd)

and_a :: CPU c => Word8 -> ReaderT c IO String
and_a z = liftA2 (.&.) (getReg z) regA >>= \res ->
  writeA res
    >> clearFlags
    >> (case res of
         0 -> setZ
         _ -> unsetZ
       )
    >> setH
    >> return ("AND reg[" ++ prettyHex z ++ "]")
  

xor_r :: CPU c => Word8 -> ReaderT c IO String
xor_r z = liftA2 xor (getReg z) regA >>= \res ->
  writeA res
    >> clearFlags
    >> (case res of
         0 -> setZ
         _ -> unsetZ
       )
    >> return ("XOR reg[" ++ prettyHex z ++ "]")

or_r :: CPU c => Word8 -> ReaderT c IO String
or_r z = liftA2 (.|.) (getReg z) regA >>= \res ->
  writeA res
    >> clearFlags
    >> (case res of
         0 -> setZ
         _ -> unsetZ
       )
    >> return ("OR reg[" ++ prettyHex z ++ "]")

pop_af :: CPU c => ReaderT c IO String
pop_af = pop >>= writeAF >> return "POP reg[AF]"

pop_r :: CPU c => Word8 -> ReaderT c IO String
pop_r p = pop >>= writeLongReg p >> return ("POP reg[" ++ prettyHex p ++ "]")

ret_nz :: CPU c => ReaderT c IO String
ret_nz = isSetZ >>= \case
      True -> return "RET NZ"
      False -> flip (++) " NZ" <$> ret

ret_z :: CPU c => ReaderT c IO String
ret_z = isSetZ >>= \case
      True -> flip (++) " NZ" <$> ret
      False -> return "RET NZ"

ret_nc :: CPU c => ReaderT c IO String
ret_nc = isSetC >>= \case
      True -> return "RET NZ"
      False -> flip (++) " NZ" <$> ret

ret_c :: CPU c => ReaderT c IO String
ret_c = isSetC >>= \case
      True -> flip (++) " NZ" <$> ret
      False -> return "RET NZ"

ret :: CPU c => ReaderT c IO String
ret = pop >>= writePC >> return "RET"

reti :: CPU c => ReaderT c IO String
reti = ret >> setIme >> return "RETI"

cb :: CPU c => ReaderT c IO String
cb = fetchInstruction >>= \inst ->
  (++) ("CB:  " ++ prettyHex inst ++ ":  ") <$> doCB
    (shiftR (inst .&. 0xC0) 6)
    (shiftR (inst .&. 0x38) 3)
    (inst .&. 0x07)

call_nz :: CPU c => ReaderT c IO String
call_nz = isSetZ >>= \case
            True -> fetchInstruction >> fetchInstruction >> return "CALL NZ, nn"
            False -> call >> return "CALL NZ, nn"

call_z :: CPU c => ReaderT c IO String
call_z = isSetZ >>= \case
            True -> call >> return "CALL Z, nn"
            False -> fetchInstruction >> fetchInstruction >> return "CALL Z, nn"

call_nc :: CPU c => ReaderT c IO String
call_nc = isSetC >>= \case
            True -> fetchInstruction >> fetchInstruction >> return "CALL NC, nn"
            False -> call >> return "CALL NC, nn"

call_c :: CPU c => ReaderT c IO String
call_c = isSetC >>= \case
            True -> call >> return "CALL C, nn"
            False -> fetchInstruction >> fetchInstruction >> return "CALL C, nn"

ld_a_nn :: CPU c => ReaderT c IO String
ld_a_nn = do
    low  <- fetchInstruction
    high <- fetchInstruction
    let idx = (shiftL (fromIntegral high :: Word16) 8) .|. (fromIntegral low :: Word16)
    mem <- memory
    val <- liftIO $ readM mem idx
    writeA val
    return $ "LD A, (" ++ prettyHex val ++ ")"
            
ld_ff_nn_a :: CPU c => ReaderT c IO String
ld_ff_nn_a = do
  mem <- memory
  a <- regA
  word <- fetchInstruction
  _ <- liftIO $ writeM mem (0xFF00 + (fromIntegral word)) a
  return ("LD nn, " ++ prettyHex a)

ld_ff_c_a :: CPU c => ReaderT c IO String
ld_ff_c_a = regC >>= \off -> memory >>= \mem ->
  regA >>= liftIO . (writeM mem (0xFF00 + (fromIntegral off))) >> return
    ("LD 0xFF + C, A")

ld_nn_a :: CPU c => ReaderT c IO String
ld_nn_a = do                          -- LD (nn), SP
    firstHalf  <- fetchInstruction
    secondHalf <- fetchInstruction
    let result = (shiftL (fromIntegral secondHalf :: Word16) 8) .|. (fromIntegral firstHalf :: Word16)
    a <- regA
    mem <- memory
    _ <- liftIO $ writeM mem result a
    return $ "LD (" ++ prettyHex result ++ "), " ++ prettyHex a ++ " [A]"

ld_a_ff :: CPU c => ReaderT c IO String
ld_a_ff = do
  imm <- fetchInstruction
  let res = 0xFF00 + (fromIntegral imm) :: Word16
  m <- memory
  val <- liftIO (readM m res)
  writeA val
  return "LD A, 0xFF00 + n"

push_af :: CPU c => ReaderT c IO String
push_af = regAF >>= push

push_reg :: CPU c => Word8 -> ReaderT c IO String
push_reg p = getLongReg p >>= push