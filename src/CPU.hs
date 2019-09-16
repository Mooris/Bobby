module CPU where

import Memory
import Data.Word
import Control.Monad.Reader
import GameBoyGpu

class CPU c where
    memory              :: ReaderT c IO GameBoyMemory
    gpu                 :: ReaderT c IO GameBoyGpu
    pc                  :: ReaderT c IO Word16
    sp                  :: ReaderT c IO Word16
    regA                :: ReaderT c IO Word8
    regF                :: ReaderT c IO Word8
    regB                :: ReaderT c IO Word8
    regC                :: ReaderT c IO Word8
    regD                :: ReaderT c IO Word8
    regE                :: ReaderT c IO Word8
    regH                :: ReaderT c IO Word8
    regL                :: ReaderT c IO Word8
    regAF               :: ReaderT c IO Word16
    regBC               :: ReaderT c IO Word16
    regDE               :: ReaderT c IO Word16
    regHL               :: ReaderT c IO Word16
    writeAF             :: Word16 -> ReaderT c IO ()
    writeBC             :: Word16 -> ReaderT c IO ()
    writeDE             :: Word16 -> ReaderT c IO ()
    writeHL             :: Word16 -> ReaderT c IO ()
    writeSP             :: Word16 -> ReaderT c IO ()
    getReg              :: Word8 -> ReaderT c IO Word8
    getLongReg          :: Word8 -> ReaderT c IO Word16
    writeReg            :: Word8 -> Word8 -> ReaderT c IO ()
    writeLongReg        :: Word8 -> Word16 -> ReaderT c IO ()
    writeA              :: Word8 -> ReaderT c IO ()
    writeF              :: Word8 -> ReaderT c IO ()
    writeB              :: Word8 -> ReaderT c IO ()
    writeC              :: Word8 -> ReaderT c IO ()
    writeD              :: Word8 -> ReaderT c IO ()
    writeE              :: Word8 -> ReaderT c IO ()
    writeH              :: Word8 -> ReaderT c IO ()
    writeL              :: Word8 -> ReaderT c IO ()
    writePC             :: Word16 -> ReaderT c IO ()
    unsetZ              :: ReaderT c IO ()
    unsetN              :: ReaderT c IO ()
    unsetH              :: ReaderT c IO ()
    unsetC              :: ReaderT c IO ()
    setZ                :: ReaderT c IO ()
    setN                :: ReaderT c IO ()
    setH                :: ReaderT c IO ()
    setC                :: ReaderT c IO ()
    isSetZ              :: ReaderT c IO Bool
    isSetN              :: ReaderT c IO Bool
    isSetH              :: ReaderT c IO Bool
    isSetC              :: ReaderT c IO Bool
    clearFlags          :: ReaderT c IO ()
    toogleZ             :: ReaderT c IO ()
    toogleN             :: ReaderT c IO ()
    toogleH             :: ReaderT c IO ()
    toogleC             :: ReaderT c IO ()
    writeRP             :: Word8 -> Word16 -> ReaderT c IO ()
    setIme              :: ReaderT c IO ()
    clearIme            :: ReaderT c IO ()
    readIme             :: ReaderT c IO Bool
    fetchInstruction    :: ReaderT c IO Word8
    storeCurrentInst    :: Word8 -> ReaderT c IO Word8
    execInstruction     :: ReaderT c IO Int
    doInst              :: Word8 -> ReaderT c IO String
      