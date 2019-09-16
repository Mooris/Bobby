module Render where

import Control.Monad.Reader
import Data.Word
import Data.Bits

import GPU
import Memory

data Pixel = Off Int | Low Int | High Int | On Int

type Framebuffer = [Word8]

update :: (GPU c, Memory m) => m -> ReaderT c IO ()
update mem = state >>= (\case
    HBlank  -> (>= 204) <$> ticks >>= \case
        True    -> decTicks 204 >> hblank >> scanLine >>= \case
                143     -> do
                    flagIF <- liftIO $ readIF mem
                    _ <- liftIO $ writeIF mem (setBit flagIF 0)
                    triggerVBlank
                _       -> triggerOAM
        False   -> return ()
    VBlank  -> (>= 456) <$> ticks >>= \case
        True    -> decTicks 456 >> (> 153) <$> incScanLine >>= \case
                True    -> resetScanLine >> triggerOAM
                False   -> return ()
        False   -> return ()
    OAM     -> (>= 80) <$> ticks >>= \case
        True    -> decTicks 80 >> triggerVRAM
        False   -> return ()
    Vram    -> (>= 172) <$> ticks >>= \case
        True    -> decTicks 172 >> triggerHBlank >> renderScanLine mem
        False   -> return ()) >> scanLine >>= liftIO . writeM mem 0xFF44 . fromIntegral >> return ()