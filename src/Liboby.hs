module Liboby
  ( RomType(..)
  , Rom(..)
  , Bobby(..)
  , openRom
  , bobbyRun
  , dumpRegisters
  , getPC
  , loadRom
  , step
  )
where

import qualified Data.ByteString               as BS
import           Data.Bits
import           Data.Word
import           Data.Array.IO
import           Control.Monad.Reader
import Data.IORef
import GPU
import           Debug.Trace
import           GameBoyCpu
import           GameBoyGpu
import           Memory
import           Utils
import           Render

data BobbyError = BadMBC

instance Show BobbyError where
  show BadMBC = "Unsuported MBC type"

openRom :: String -> RomType -> IO Rom
openRom filename GB = BasicRom GB <$> BS.readFile filename

bobbyRun :: BobbyCallback Bool -> Rom -> IO (Maybe BobbyError)
bobbyRun callback (BasicRom GB rom) = either (return . Just)
                                       createAndRun
                                       (load rom)
 where
  createAndRun mb =
    loadBios >>= createGameBoyCpu mb >>= runBios callback >>= runRom callback

createGameBoyCpu :: MBC -> Rom -> IO GameBoyCpu
createGameBoyCpu mb (BasicRom GB _) = let createRegs = newArray (0, 7) 0 >>= initRegs in do
    regs <- createRegs
    m <- createMemory mb
    initGPU <- createGPU
    initIME <- newIORef True
    return $ GameBoyCpu Nothing regs m initGPU initIME

createGPU :: IO GameBoyGpu
createGPU = do
    initState <- newIORef HBlank
    initTicks <- newIORef 0
    initScanline <- newIORef 0
    ary <- newArray (0, (144 * 160)) 0
    return $ GameBoyGpu initState initTicks initScanline ary

loadBios :: IO Rom
loadBios = openRom "bios.gb" GB

runBios :: BobbyCallback Bool -> GameBoyCpu -> IO GameBoyCpu
runBios callback cpu@(GameBoyCpu _ regs mam gbGpu imeRef) = runReaderT pc cpu >>= \case
  256 -> (GameBoyCpu Nothing regs mam gbGpu imeRef) <$ runReaderT (writePC 0x100) cpu
  _   -> runReaderT execInstruction cpu >>= \case
    --0 -> runBios cpu
    0 -> callback (Bobby cpu) >> runBios callback cpu
    _ -> let loop = loop in loop

getPC :: Bobby -> IO Word16
getPC (Bobby cpu) = runReaderT pc cpu
getPC _ = return 0xFFFF

labite :: Registers -> Int -> IO Word16
labite = readArray

dumpRegisters :: Bobby -> IO [String]
dumpRegisters (Bobby cpu) = mapM prnt
  $ zip ["PC:\t", "SP:\t", "AF:\t", "BC:\t", "DE:\t", "HL:\t", "Current:\t"] [0 ..]
 where
  prnt (name, idx) =
    (\x -> name ++ prettyHex ((x .&. 0xFF00) `shiftR` 8) ++ " " ++ prettyHex (x .&. 0xFF))
      <$> labite (registers cpu) idx
dumpRegisters _ = return ["Shit happened\b"]

loadRom :: Rom -> IO Bobby
loadRom (BasicRom GB rom) = case load rom of
    Right bankController -> loadBios >>= createGameBoyCpu bankController >>= return . Bobby
    _ -> return (Fuk "Unknown")

step :: Bobby -> Int -> IO Bobby
step b@(Bobby cpu@(GameBoyCpu _ _ mlm gbGpu _)) counter = runReaderT execInstruction cpu >>= \case
      0 -> runReaderT (update mlm) gbGpu >> if counter > 0 then step b (counter - 1) else return b
      _ -> return (Fuk "Unknown")
step b _ = return b
     
-- Private Implementation

load :: BS.ByteString -> Either BobbyError MBC
load rom = case (BS.index rom 0x147) of
  0 -> Right $ RomOnly rom
  other -> trace (show other) $ Left BadMBC

createMemory :: MBC -> IO GameBoyMemory
createMemory gbMbc = GameBoyMemory gbMbc <$> newArray (0x8000, 0xFFFF) 0
                    >>= \mlm -> writeM mlm 0xFF05 0x00
                                >> writeM mlm 0xFF06 0x00
                                >> writeM mlm 0xFF07 0x00
                                >> writeM mlm 0xFF10 0x80
                                >> writeM mlm 0xFF11 0xBF
                                >> writeM mlm 0xFF12 0xF3
                                >> writeM mlm 0xFF14 0xBF
                                >> writeM mlm 0xFF16 0x3F
                                >> writeM mlm 0xFF17 0x00
                                >> writeM mlm 0xFF19 0xBF
                                >> writeM mlm 0xFF1A 0x7F
                                >> writeM mlm 0xFF1B 0xFF
                                >> writeM mlm 0xFF1C 0x9F
                                >> writeM mlm 0xFF1E 0xBF
                                >> writeM mlm 0xFF20 0xFF
                                >> writeM mlm 0xFF21 0x00
                                >> writeM mlm 0xFF22 0x00
                                >> writeM mlm 0xFF23 0xBF
                                >> writeM mlm 0xFF24 0x77
                                >> writeM mlm 0xFF25 0xF3
                                >> writeM mlm 0xFF26 0xF1
                                >> writeM mlm 0xFF40 0x91
                                >> writeM mlm 0xFF42 0x00
                                >> writeM mlm 0xFF43 0x00
                                >> writeM mlm 0xFF45 0x00
                                >> writeM mlm 0xFF47 0xFC
                                >> writeM mlm 0xFF48 0xFF
                                >> writeM mlm 0xFF49 0xFF
                                >> writeM mlm 0xFF4A 0x00
                                >> writeM mlm 0xFF4B 0x00
                    >> return mlm

initRegs :: Registers -> IO Registers
initRegs regs =
  writeArray regs 0 0x100
    >> writeArray regs 1 0xFFFE
    >> writeArray regs 2 0x01B0
    >> writeArray regs 3 0x0013
    >> writeArray regs 4 0x00D8
    >> writeArray regs 5 0x014D
    >> return regs

runRom :: BobbyCallback Bool -> GameBoyCpu -> IO (Maybe BobbyError)
runRom callback cpu = runReaderT execInstruction cpu >>= \case
  0 -> callback (Bobby cpu) >>= \case
    True  -> runRom callback cpu
    False -> return Nothing
  _ -> callback (Fuk "Unknown") >> return Nothing