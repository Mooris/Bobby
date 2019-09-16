module GameBoyGpu where

import Data.IORef
import Data.Array.IO
import Data.Word
import Data.Bits
import Memory
import GPU

data GameBoyGpu = GameBoyGpu {
    st  :: IORef GpuState,
    tks :: IORef Int,
    scl :: IORef Int,
    framebuffer :: IOArray Int Word8
}

instance GPU GameBoyGpu where
    state           = asks st >>= liftIO . readIORef
    ticks           = asks tks >>= liftIO . readIORef
    decTicks dc     = (flip (-) dc) <$> ticks >>= \tcks -> (asks tks >>= liftIO . flip writeIORef tcks)
    scanLine        = asks scl >>= liftIO . readIORef
    resetScanLine   = asks scl >>= liftIO . flip writeIORef 0
    incScanLine     = (+ 1) <$> scanLine >>= \arg -> arg <$ (asks scl >>= liftIO . flip writeIORef arg)
    triggerVBlank   = asks st >>= liftIO . (flip writeIORef VBlank)
    triggerOAM      = asks st >>= liftIO . (flip writeIORef OAM)
    triggerVRAM     = asks st >>= liftIO . (flip writeIORef Vram)
    triggerHBlank   = asks st >>= liftIO . (flip writeIORef HBlank)
    hblank          = do
        ln <- scanLine
        ref <- asks scl
        liftIO $ writeIORef ref (ln + 1)
    renderScanLine mem  = do
        ly  <- scanLine
        _   <- liftIO $ writeM mem 0xFF44 (fromIntegral ly)
        scx <- liftIO $ readM mem 0xFF43
        scy <- liftIO $ readM mem 0xFF42
        lcdc <- liftIO $ readM mem 0xFF40
        let lcdon = testBit lcdc 7
        let bgon = testBit lcdc 0
        let bgmap = testBit lcdc 3
        let bgmapStartAddr = if bgmap then 0x9C00 else 0x9800
        let bgtiles = testBit lcdc 4
        let bgtilesStartAddr = if bgtiles then 0x8000 else 0x9000
        --let spritesOn = testBit lcdc 1
        --let spritesBig = testBit lcdc 2
        let getBgPixel :: Int -> Int -> IO Word8
            getBgPixel x y =
                let x' = (x + fromIntegral scx) `mod` 256
                    y' = (y + fromIntegral scy) `mod` 256
                    yrow = y' `div` 8
                    xrow = x' `div` 8
                    tileNum = yrow * 32 + xrow
                    xoff = 7 - (x' `mod` 8)
                    yoff = y' `mod` 8
                in  readM mem ((fromIntegral tileNum) + bgmapStartAddr) >>= \tmp -> return (((fromIntegral tmp :: Int) * 16) + bgtilesStartAddr)
                    >>= \tileStartMem ->    let     hiByte = tileStartMem + (fromIntegral yoff * 2)
                                                    loByte = tileStartMem + (fromIntegral yoff * 2) + 1
                                            in  do
                                                    hiByteValue <- readM mem (fromIntegral hiByte)
                                                    loByteValue <- readM mem (fromIntegral loByte)
                                                    let color = (2 * (fromEnum (testBit loByteValue xoff))) + (fromEnum (testBit hiByteValue xoff))
                                                    if bgon && lcdon then return (fromIntegral color) else return 0
        fb <- asks framebuffer
        mapM_ (\x -> do
            col <- liftIO $ getBgPixel x ly
            liftIO $ writeArray fb (x + (160 * ly)) col) [0..159]
        --scanLine >>= liftIO . putStrLn . ((++) "scanline ") . show >> return ()


updateTicks     :: GameBoyGpu -> Int -> IO ()
updateTicks (GameBoyGpu _ tickCount _ _) inc = ((+ inc) <$> liftIO (readIORef tickCount)) >>= liftIO . writeIORef tickCount >> return ()