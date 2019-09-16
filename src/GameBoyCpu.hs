module GameBoyCpu(
  module CPU
  , Bobby(..)
  , GameBoyCpu(..)
  , CPU(..)
  , BobbyCallback
  , Registers
) where

import           Data.Word
import qualified Data.ByteString               as BS
import           Memory
import           Data.Bits
import           Data.Array.IO
import           CPU
import           Utils
import           Instructions
import Control.Monad.Reader
import CommonInstructions
import GameBoyGpu
import Data.IORef

data Bobby = Bobby GameBoyCpu | Fuk String
type Registers = IOArray Int Word16

data GameBoyCpu = GameBoyCpu {
  bios      :: Maybe BS.ByteString,
  registers :: Registers,
  mem       :: GameBoyMemory,
  gp        :: GameBoyGpu,
  ime       :: IORef Bool
}

type BobbyCallback a = Bobby -> IO a

instance CPU GameBoyCpu where
    memory = asks mem
    gpu = asks gp
    pc = ask >>= liftIO . flip readArray 0 . registers
    sp = ask >>= liftIO . flip readArray 1 . registers
    regA = ask >>= \cpu -> fromIntegral . flip shiftR 8 . (.&.) 0xFF00 <$> liftIO (readArray
        (registers cpu)
        2)
    regF = ask >>= \cpu -> fromIntegral . (.&.) 0x00FF <$> liftIO (readArray (registers cpu) 2)
    regB = ask >>= \cpu -> fromIntegral . flip shiftR 8 . (.&.) 0xFF00 <$> liftIO (readArray
        (registers cpu)
        3)
    regC = ask >>= \cpu -> fromIntegral . (.&.) 0x00FF <$> liftIO (readArray (registers cpu) 3)
    regD = ask >>= \cpu -> fromIntegral . flip shiftR 8 . (.&.) 0xFF00 <$> liftIO (readArray
        (registers cpu)
        4)
    regE = ask >>= \cpu -> fromIntegral . (.&.) 0x00FF <$> liftIO (readArray (registers cpu) 4)
    regH = ask >>= \cpu -> fromIntegral . flip shiftR 8 . (.&.) 0xFF00 <$> liftIO (readArray
        (registers cpu)
        5)
    regL = ask >>= \cpu -> fromIntegral . (.&.) 0x00FF <$> liftIO (readArray (registers cpu) 5)
    regAF = ask >>= \cpu -> liftIO $ readArray (registers cpu) 2
    regBC = ask >>= \cpu -> liftIO $ readArray (registers cpu) 3
    regDE = ask >>= \cpu -> liftIO $ readArray (registers cpu) 4
    regHL = ask >>= \cpu -> liftIO $ readArray (registers cpu) 5
    writeAF wd = ask >>= \cpu -> liftIO $ writeArray (registers cpu) 2 wd
    writeBC wd = ask >>= \cpu -> liftIO $ writeArray (registers cpu) 3 wd
    writeDE wd = ask >>= \cpu -> liftIO $ writeArray (registers cpu) 4 wd
    writeHL wd = ask >>= \cpu -> liftIO $ writeArray (registers cpu) 5 wd
    writeSP wd = ask >>= \cpu -> liftIO $ writeArray (registers cpu) 1 wd
    getReg 0 = regB
    getReg 1 = regC
    getReg 2 = regD
    getReg 3 = regE
    getReg 4 = regH
    getReg 5 = regL
    getReg 6 = memory >>= \mlm -> regHL >>= liftIO . readM mlm
    getReg 7 = regA
    getReg _ = let loop = loop in loop
    getLongReg 0 = regBC
    getLongReg 1 = regDE
    getLongReg 2 = regHL
    getLongReg 3 = sp
    getLongReg _ = let loop = loop in loop
    writeReg 0 = writeB
    writeReg 1 = writeC
    writeReg 2 = writeD
    writeReg 3 = writeE
    writeReg 4 = writeH
    writeReg 5 = writeL
    writeReg 6 = \wd -> memory >>= \mlm ->
        regHL >>= \idx -> liftIO (writeM mlm idx wd) >> return ()
    writeReg 7 = writeA
    writeReg _ = let loop = loop in loop
    writeLongReg 0 = writeBC
    writeLongReg 1 = writeDE
    writeLongReg 2 = writeHL
    writeLongReg 3 = writeSP
    writeLongReg _ = let loop = loop in loop
    writeA wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 2)
        >>= liftIO . writeArray (registers cpu) 2 . (.|.) (( (flip shiftL 8) . (fromIntegral :: Integral i => i -> Word16)) wd) . (.&.) 0x00FF
    writeF wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 2)
            >>= liftIO . writeArray (registers cpu) 2
            .   (.|.) ((fromIntegral :: Integral i => i -> Word16) wd)
            .   (.&.) 0xFF00
    writeB wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 3)
            >>= liftIO . writeArray (registers cpu) 3
            .   (.|.)
                    (( (flip shiftL 8)
                     . (fromIntegral :: Integral i => i -> Word16)
                     )
                        wd
                    )
            .   (.&.) 0x00FF
    writeC wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 3)
            >>= liftIO . writeArray (registers cpu) 3
            .   (.|.) ((fromIntegral :: Integral i => i -> Word16) wd)
            .   (.&.) 0xFF00
    writeD wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 4)
            >>= liftIO . writeArray (registers cpu) 4
            .   (.|.)
                    (( (flip shiftL 8)
                     . (fromIntegral :: Integral i => i -> Word16)
                     )
                        wd
                    )
            .   (.&.) 0x00FF
    writeE wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 4)
            >>= liftIO . writeArray (registers cpu) 4
            .   (.|.) ((fromIntegral :: Integral i => i -> Word16) wd)
            .   (.&.) 0xFF00
    writeH wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 5)
            >>= liftIO . writeArray (registers cpu) 5
            .   (.|.)
                    (( (flip shiftL 8)
                     . (fromIntegral :: Integral i => i -> Word16)
                     )
                        wd
                    )
            .   (.&.) 0x00FF
    writeL wd = ask >>= \cpu ->
        liftIO (readArray (registers cpu) 5)
            >>= liftIO . writeArray (registers cpu) 5
            .   (.|.) ((fromIntegral :: Integral i => i -> Word16) wd)
            .   (.&.) 0xFF00
    writePC wd = ask >>= \cpu -> liftIO $ writeArray (registers cpu) 0 wd
    unsetZ =  regF >>= writeF . flip clearBit 7
    unsetN =  regF >>= writeF . flip clearBit 6
    unsetH =  regF >>= writeF . flip clearBit 5
    unsetC =  regF >>= writeF . flip clearBit 4
    setZ =    regF >>= writeF . flip setBit 7
    setN =    regF >>= writeF . flip setBit 6
    setH =    regF >>= writeF . flip setBit 5
    setC =    regF >>= writeF . flip setBit 4
    isSetZ =  flip testBit 7 <$> regF 
    isSetN =  flip testBit 6 <$> regF 
    isSetH =  flip testBit 5 <$> regF 
    isSetC =  flip testBit 4 <$> regF 
    toogleZ = flip testBit 7 <$> regF >>= \case
        True  -> regF >>= writeF . flip clearBit 7
        False -> regF >>= writeF . flip setBit 7
    toogleN = flip testBit 6 <$> regF >>= \case
        True  -> regF >>= writeF . flip clearBit 6
        False -> regF >>= writeF . flip setBit 6
    toogleH = flip testBit 5 <$> regF >>= \case
        True  -> regF >>= writeF . flip clearBit 5
        False -> regF >>= writeF . flip setBit 5
    toogleC = flip testBit 4 <$> regF >>= \case
        True  -> regF >>= writeF . flip clearBit 4
        False -> regF >>= writeF . flip setBit 4
    clearFlags = writeF 0
    writeRP 0 = writeBC
    writeRP 1 = writeDE
    writeRP 2 = writeHL
    writeRP 3 = writeSP
    writeRP _ = let loop = loop in loop
    setIme = asks ime >>= liftIO . flip writeIORef True
    clearIme = asks ime >>= liftIO . flip writeIORef False
    readIme = asks ime >>= liftIO . readIORef
    fetchInstruction = ask >>= fetchInstruction'
      where
        fetchInstruction' (GameBoyCpu (Just bis) _ _ _ _) =
            BS.index bis . fromIntegral <$> pc >>= \inst ->
                (+) 1 <$> pc >>= writePC >> return inst
        fetchInstruction' (GameBoyCpu Nothing _ mam _ _) =
            pc >>= liftIO . readM mam >>= \inst ->
                (+) 1 <$> pc >>= writePC >> return inst
    storeCurrentInst i = ask >>= (\cpu -> liftIO (writeArray (registers cpu) 6 (fromIntegral i))) >> return i
    execInstruction =
            fetchInstruction >>= storeCurrentInst
            >>= doInst >>= \case
                "Unknown" -> return 1
                _ -> gpu >>= liftIO . flip updateTicks 12 >> readIme >>= handleIRQ
      where
        handleIRQ True = do
            mlm <- memory
            flagIE <- liftIO $ readIE mlm
            flagIF <- liftIO $ readIF mlm
            let fired = flagIE .&. flagIF
            let f   | testBit fired 0 = liftIO (writeIF mlm (clearBit flagIF 0)) >> clearIme >> call' 0x00 0x40 >> return 0
                    | otherwise = return 0
            f
        handleIRQ False = return 0
    doInst inst = doInst'
                        (shiftR (inst .&. 0xC0) 6)
                        (shiftR (inst .&. 0x38) 3)
                        (inst .&. 0x07)
                        (shiftR (inst .&. 0x30) 4)
                        (shiftR (inst .&. 0x08) 3)
        where
            -- doInst' :: CPU c => Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> ReaderT c IO String
            doInst' 0 0 0 _ _ = liftIO $ return "NOPE"
            doInst' 0 1 0 _ _ = ld_nn_sp  
            doInst' 0 2 0 _ _ = stop      
            doInst' 0 2 7 _ _ = rla       
            doInst' 0 3 0 _ _ = jr        
            doInst' 0 4 0 _ _ = jr_nz     
            doInst' 0 5 0 _ _ = jr_z      
            doInst' 0 6 0 _ _ = jr_nc     
            doInst' 0 7 0 _ _ = jr_c      
            doInst' 0 _ 1 p 0 = ld_rp_nn    p
            doInst' 0 _ 1 p 1 = add_hl_rp   p
            --doInst' 0 0 1 p 1 = return $ "ADD HL, RP[" ++ show p ++ "]"
            doInst' 0 _ 2 0 0 = ld_bc_a   
            doInst' 0 _ 2 1 0 = ld_de_a   
            doInst' 0 _ 2 2 0 = ld_hlpp_a 
            doInst' 0 _ 2 3 0 = ld_hlmm_a 
            doInst' 0 _ 2 0 1 = ld_a_bc   
            doInst' 0 _ 2 1 1 = ld_a_de   
            doInst' 0 _ 2 2 1 = ld_a_hlpp
            doInst' 0 _ 2 3 1 = ld_a_hlpp
            doInst' 0 _ 3 p 0 = inc_rp      p
            doInst' 0 _ 3 p 1 = dec_rp      p
            doInst' 0 y 4 _ _ = inc_r       y
            doInst' 0 y 5 _ _ = dec_r       y
            doInst' 0 y 6 _ _ = ld_r_n      y
            doInst' 1 6 6 _ _ = halt    
            doInst' 1 y z _ _ = ld_r_r      y z
            doInst' 2 0 z _ _ = add_a       z
            doInst' 2 1 z _ _ = getReg z >>= adc_a
            doInst' 2 2 z _ _ = sub_a       z
            --doInst' 2 3 _ _ _ = return "SBC A"
            doInst' 2 4 z _ _ = and_a       z
            doInst' 2 5 z _ _ = xor_r       z
            doInst' 2 6 z _ _ = or_r        z
            doInst' 2 7 z _ _ = getReg z >>= cp
            doInst' 3 0 0 _ _ = ret_nz
            doInst' 3 1 0 _ _ = ret_z
            doInst' 3 2 0 _ _ = ret_nc
            doInst' 3 3 0 _ _ = ret_c
            doInst' 3 _ 1 p 0 = pop_r       p
            doInst' 3 _ 1 0 1 = ret
            doInst' 3 _ 1 1 1 = reti
            doInst' 3 _ 1 3 _ = pop_af
            --doInst' 3 0 2 1 _ = return "JP HL"
            doInst' 3 1 3 _ _ = cb
            doInst' 3 0 4 _ _ = call_nz
            doInst' 3 1 4 _ _ = call_z
            doInst' 3 2 4 _ _ = call_nc
            doInst' 3 3 4 _ _ = call_c
            doInst' 3 1 6 _ _ = fetchInstruction >>= adc_a -- Double check 
            doInst' 3 7 2 _ _ = ld_a_nn
            doInst' 3 7 6 _ _ = fetchInstruction >>= cp
            doInst' 3 4 0 _ _ = ld_ff_nn_a
            doInst' 3 4 2 _ _ = ld_ff_c_a
            doInst' 3 5 2 _ _ = ld_nn_a
            doInst' 3 6 0 _ _ = ld_a_ff
            doInst' 3 0 3 _ _ = do
              low  <- (fromIntegral :: Integral a => a -> Word16) <$> fetchInstruction
              high <- (fromIntegral :: Integral a => a -> Word16) <$> fetchInstruction
              let off = (shiftL high 8) .|. low :: Word16
              writePC off
              return $ "JP " ++ prettyHex off
            doInst' 3 6 3 _ _ = clearIme >> return "DI"
            doInst' 3 7 3 _ _ = setIme >> return "EI"
            doInst' 3 _ 5 3 0 = push_af
            doInst' 3 _ 5 p 0 = push_reg    p
            doInst' 3 _ 5 0 1 = call >> return ("CALL nn")
            doInst' x y z p q = pc >>= liftIO . putStrLn . ((++) ("\nx: " ++ show x ++ "\ty: " ++ show y ++ "\tz: " ++ show z ++ "\np: " ++ show p ++ "\tq: " ++ show q ++ "\npc: ")) . prettyHex >> return "Unknown"