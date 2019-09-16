{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Liboby
import SFML.Graphics
import SFML.Window

import GameBoyGpu
import GameBoyCpu
import Data.Array.IO
import Data.Word
import Utils

import Memory
import qualified Data.ByteString               as BS
import GPU
import Data.IORef
import Render
data TestRenderMemory = TRM BS.ByteString

instance Memory TestRenderMemory where
    writeM _ _ _ = return False
    readM _ 0xFF43 = return 0
    readM _ 0xFF42 = return 0
    readM _ 0xFF40 = return 0x91
    readM (TRM gbMemory) idx | idx >= 0x8000 && idx <= 0xA000 = return $ BS.index gbMemory (fromIntegral (idx - 0x8000))
    readM _ idx = putStrLn (prettyHex idx) >> return 0
    --readM (TRM mem) idx = return 0
    readIE = const $ return 0
    writeIE = const (const (return False))
    readIF = const $ return 0
    writeIF = const (const (return False))

createGPU :: IO GameBoyGpu
createGPU = do
    status <- newIORef HBlank
    initTicks <- newIORef 0
    initScanLine <- newIORef 0
    ary <- newArray (0, (144 * 160)) 0
    return $ GameBoyGpu status initTicks initScanLine ary

doTest :: Bool -> RenderWindow -> VertexArray -> IO ()
doTest True window va = do
  dump <- TRM <$> BS.readFile "Tetris_video.dump"
  gbGpu <- createGPU
  testLoop window va dump gbGpu
doTest False window va = do
  args <- getArgs
  rom <- openRom (head args) GB
  bobby <- loadRom rom
  appLoop window va bobby

baseScale :: Int
baseScale = 12

main :: IO ()
main = do
  window <- createRenderWindow  (VideoMode (160 * baseScale) (143 * baseScale) 32)
                                "Bobby"
                                [SFTitlebar, SFClose]
                                Nothing
  vertexArray <- createVA
  setPrimitiveType vertexArray Quads
  doTest False window vertexArray
  destroy window

processEvt :: RenderWindow -> String -> IO (Maybe String)
processEvt wnd key = do
    evt <- pollEvent wnd
    case evt of
        Just SFEvtClosed -> return Nothing
        Just e@SFEvtKeyPressed{} -> processEvt wnd (show . code $ e)
        Nothing -> return . Just $ key
        _ -> processEvt wnd key

testLoop :: RenderWindow -> VertexArray -> TestRenderMemory -> GameBoyGpu -> IO ()
testLoop window vertexArray mlm gbGpu = do
  clearVA vertexArray
  key <- processEvt window ""
  case key of
    Nothing -> return ()
    Just "KeyQ" -> return ()
    Just _ -> do
        let loop ctr  | ctr > 0   = updateTicks gbGpu 12 >> runReaderT (update mlm) gbGpu >> loop (ctr - 1)
                      | otherwise = return ()
        loop (255 :: Int)
        mapM_ (\leY -> mapM_ (\leX -> do
            col <- readArray (framebuffer gbGpu) (leX + (160 * leY))
            let nx = leX * baseScale
            let ny = leY * baseScale
            appendVA vertexArray (putPixel col (fromIntegral (nx + 0)) (fromIntegral (ny + 0)))
            appendVA vertexArray (putPixel col (fromIntegral (nx + baseScale)) (fromIntegral (ny + 0)))
            appendVA vertexArray (putPixel col (fromIntegral (nx + baseScale)) (fromIntegral (ny + baseScale)))
            appendVA vertexArray (putPixel col (fromIntegral (nx + 0)) (fromIntegral (ny + baseScale)))) [0..160]) [0..143]
        drawVertexArray window vertexArray Nothing --(Just (renderStates { transform = scaling 8 8}))
        display window
        testLoop window vertexArray mlm gbGpu

appLoop :: RenderWindow -> VertexArray -> Bobby -> IO ()
appLoop window vertexArray bobby = do
  clearVA vertexArray
  key <- processEvt window ""
  -- pc <- getPC bobby
  -- putStrLn ("Lepc: " ++ prettyHex pc)
  case key of
    Nothing -> return ()
    Just "KeyQ" -> return ()
    Just _ -> do
        -- case pc >= 0x021E of
        --   True -> getChar >> return ()
        --   False -> return ()
        b2 <- step bobby 128
        -- regs <- dumpRegisters b2
        -- let str = foldl ((. ('\n' :)) . (++)) "" regs
        -- putStrLn str
        case b2 of
          Fuk _ -> do
            appLoop window vertexArray b2
          (Bobby (GameBoyCpu _ _ _ (GameBoyGpu _ _ _ fb) _)) -> do
              mapM_ (\leY -> mapM_ (\leX -> do
                  col <- readArray fb (leX + (160 * leY))
                  let nx = leX * baseScale
                  let ny = leY * baseScale
                  appendVA vertexArray (putPixel col (fromIntegral (nx + 0)) (fromIntegral (ny + 0)))
                  appendVA vertexArray (putPixel col (fromIntegral (nx + baseScale)) (fromIntegral (ny + 0)))
                  appendVA vertexArray (putPixel col (fromIntegral (nx + baseScale)) (fromIntegral (ny + baseScale)))
                  appendVA vertexArray (putPixel col (fromIntegral (nx + 0)) (fromIntegral (ny + baseScale)))) [0..160]) [0..143]
              drawVertexArray window vertexArray Nothing --(Just (renderStates { transform = scaling 8 8}))
              display window
              appLoop window vertexArray b2
                
putPixel :: Word8 -> Float -> Float -> Vertex
putPixel col leX leY = do
  case col of
    0 -> Vertex (Vec2f leX leY) (Color 220 255 220 255) (Vec2f 0 0)
    1 -> Vertex (Vec2f leX leY) (Color 160 192 160 255) (Vec2f 0 0)
    2 -> Vertex (Vec2f leX leY) (Color 60 96 60 255) (Vec2f 0 0)
    3 -> Vertex (Vec2f leX leY) (Color 0 20 0 255) (Vec2f 0 0)
    _ -> Vertex (Vec2f leX leY) (Color 0 20 0 255) (Vec2f 0 0)
