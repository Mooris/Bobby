module GPU(
    module Control.Monad.Reader
    , GpuState(..)
    , GPU(..)
) where

import Control.Monad.Reader
import Memory

data GpuState = HBlank | VBlank | OAM | Vram deriving (Enum)

class GPU g where
    state           :: ReaderT g IO GpuState
    ticks           :: ReaderT g IO Int
    decTicks        :: Int -> ReaderT g IO ()
    scanLine        :: ReaderT g IO Int
    resetScanLine   :: ReaderT g IO ()
    incScanLine     :: ReaderT g IO Int
    triggerVBlank   :: ReaderT g IO ()
    triggerOAM      :: ReaderT g IO ()
    triggerVRAM     :: ReaderT g IO ()
    triggerHBlank   :: ReaderT g IO ()
    hblank          :: ReaderT g IO ()
    renderScanLine  :: Memory m => m -> ReaderT g IO ()
