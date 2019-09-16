module Memory where

import Data.Word
import qualified Data.ByteString               as BS
import           Data.Array.IO

data RomType = GB

data Rom = BasicRom RomType BS.ByteString

data MBC = RomOnly {
  active :: BS.ByteString
}

type MemoryZone = IOArray Word16 Word8

data GameBoyMemory = GameBoyMemory {
  mbc :: MBC,
  ram :: MemoryZone
}

class Memory a where
  writeM  :: a -> Word16 -> Word8 -> IO Bool
  readM   :: a -> Word16 -> IO Word8
  readIF  :: a -> IO Word8
  writeIF :: a -> Word8 -> IO Bool
  readIE  :: a -> IO Word8
  writeIE :: a -> Word8 -> IO Bool

instance Memory GameBoyMemory where
  writeM _ idx _ | idx >= 0 && idx < 0x8000 = return False
  writeM memory idx value | idx >= 0x8000 && idx < 0xA000 =
    writeArray (ram memory) idx value >> return True
  writeM memory idx value | idx >= 0xA000 && idx < 0xC000 =
    writeArray (ram memory) idx value >> return True
  writeM memory idx value | idx >= 0xC000 && idx < 0xDE00 =
    writeArray (ram memory) idx value
    >> writeArray (ram memory) (idx + 0x2000) value >> return True
  writeM memory idx value | idx >= 0xDE00 && idx < 0xE000 =
    writeArray (ram memory) idx value >> return True
  writeM memory idx value | idx >= 0xE000 && idx < 0xFE00 =
    writeArray (ram memory) idx value
    >> writeArray (ram memory) (idx - 0x2000) value >> return True    
  writeM memory idx value | idx >= 0xFE00 && idx < 0xFF80 =
    writeArray (ram memory) idx value >> return True
  writeM memory idx value | idx >= 0xFF80 && idx < 0xFFFF =
    writeArray (ram memory) idx value >> return True
  writeM memory idx value | idx == 0xFFFF =
    writeArray (ram memory) idx value >> return True
  writeM _ _ _                       = return False

  readM memory idx | idx >= 0 && idx < 0x8000 =
    return $ BS.index (active (mbc memory)) (fromIntegral idx)
  readM memory idx | idx >= 0x8000 && idx < 0xFF00 = readArray (ram memory) idx
  readM memory idx | idx >= 0xFF00 && idx < 0xFFFF = readArray (ram memory) idx
  readM memory idx | idx == 0xFFFF = readArray (ram memory) idx
  readM _ _                        = return 0
    
  readIF = flip readM 0xFF0F
  writeIF = flip writeM 0xFF0F
  readIE = flip readM 0xFFFF
  writeIE = flip writeM 0xFFFF