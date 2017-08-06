-- |
-- Module      : Pixels.Render
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : 
-- Portability : 
--

-- TODO | - High-level interaction and events
--          -- Composite events (eg. pressing while moving mouse)
--          -- Gestures (?)
--          -- Key combos
--          -- Mouse delta
--        - Key maps (dynamic)
--        - Introspection (for debuggibg)
--        - Visual feedback and bindings menu

-- SPEC | -
--        -



-- GHC pragmas ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeFamilies      #-}

-- API -----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Interaction where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import Prelude hiding (print, putStr, putStrLn)

import qualified Data.ByteString as B
import           Data.Word
import           Data.Function (on)
import           Data.Bits
import           Data.Time
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.IORef
import           Data.Foldable
import qualified Data.Set         as S
import qualified Data.Map         as M
import qualified Data.Traversable as T
-- import           Data.Aeson
import           Data.List  (transpose, isInfixOf, sortBy)
import           Data.Ord   (comparing)

import           Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa as R

import Text.Printf
import Text.Read (readMaybe, readEither)

import Linear (V2(..), V3(..), V4(..))

import System.FilePath  ((</>), (<.>), takeExtension, dropExtension, takeFileName, normalise, isValid)
import System.Directory (getDirectoryContents)
import System.FSNotify
import System.Console.ANSI as ANSI

import qualified Codec.Picture.Types as Juicy

import           Control.Monad.Trans.Class as St
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as St
import           Control.Monad.Trans.Either
import           Control.Lens hiding (argument, (<.>))
import           Control.Monad
import           Control.Monad.Loops (whileM, whileJust)
import           Control.Applicative ((<$>), (<*>), liftA2)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception (finally, catch, displayException)

import           Graphics.GPipe hiding (texture)
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import           Graphics.GPipe.Context.GLFW       (WindowConfig(..), setWindowShouldClose)
import           Graphics.GPipe.Context.GLFW.Input (Key(..), KeyState(..), MouseButton(..), MouseButtonState(..))

import Leibniz.Constants (π)
import Cartesian.Core    (x,y,z)

import           Pixels.Types
import           Pixels.Lenses
import           Pixels.Persistence
import           Pixels.Trinkets
import           Pixels.Render as Render
import qualified Pixels.Load   as Load

-- Definitions ---------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Move
--        - Don't hard-code
root :: FilePath
root = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"


-- |
initialInput :: InputChannel -> V2 Int -> Input
initialInput ch sz = Input {
                       fSize   = sz,
                       fScroll = V2 0 0,
                       fMouse  = Mouse (V2 0 0) S.empty,
                       fKeyboard = S.empty,
                       fInputChannel = ch }

-- App actions ---------------------------------------------------------------------------------------------------------------------------------------

-- These should eventually be moved to some other module

-- | 
clearCanvas :: App os -> AppT os (App os)
clearCanvas app = do
  -- TODO: Clear texture instead of recreating
  -- clearImageColor
  blank <- Render.new (app^.easel.canvas.size) (\_ -> app^.easel.canvas.colour)
  return $ app & easel.canvas.texture .~ blank


-- |
quitApplication :: App os -> AppT os (App os)
quitApplication app = do
  setWindowShouldClose (app^.window) True
  return app


-- |
-- TODO | - Choose path properly
--        -
saveCanvas :: App os -> AppT os (App os)
saveCanvas app = do
  now <- normalise . formatTime defaultTimeLocale "%s" <$> liftIO getCurrentTime
  if isValid now
    then Render.save (root </> "assets" </> now <.> "png") (app^.easel.canvas.texture) id
    else liftIO . putStrLn $ "Invalid file name: " ++ (root </> "assets" </> now <.> "png")
  return app


-- | Writes a pixel on the canvas if `p` is inside it
drawPixelOnCanvas :: App os -> V2 Float -> V3 Juicy.Pixel8 -> AppT os (Either String ())
drawPixelOnCanvas app p colour = Render.writePixel (floor <$> toCanvasCoords app p) colour (app^.easel.canvas.texture)


-- |
drawWithBrush :: App os -> V2 Float -> AppT os (Either String ()) 
drawWithBrush app pos = drawPixelOnCanvas app pos (app^.easel.brush.colour)


-- |
erasePixel :: App os -> V2 Float -> AppT os (Either String ())
erasePixel app pos = drawPixelOnCanvas app pos (app^.easel.canvas.colour)

-- Event plumbing ------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Add modifiers, scancode (?)
--        - KeyDown
--        - KeyUp
--        - FileChange
setupEvents :: Window os RGBFloat Depth -> AppT os (TChan AppEvent)
setupEvents win = do
  ch <- liftIO newTChanIO

  GLFW.setDropCallback        win (Just $ \paths            -> putEvent ch $ FileDrop paths)
  GLFW.setScrollCallback      win (Just $ \sx sy            -> putEvent ch $ MouseScroll (V2 sx sy))
  GLFW.setCursorPosCallback   win (Just $ \mx my            -> putEvent ch $ MouseMotion (V2 mx my))
  GLFW.setMouseButtonCallback win (Just $ \b bstate mods    -> putEvent ch $ makeMouseEvent b bstate mods)
  GLFW.setKeyCallback         win (Just $ \k sc kstate mods -> putEvent ch $ makeKeyEvent k sc kstate mods)
  GLFW.setWindowCloseCallback win (Just $ putEvent ch $ WindowClosing) -- TODO: Invoke directly (?)
  -- GLFW.setCursorEnterCallback win (Just $ \cstate -> _)
  -- (CursorState'InWindow, CursorState'NotInWindow)
  -- setClipboardString, getClipboardString
  return ch
  where
    makeMouseEvent b MouseButtonState'Pressed  _ = MouseDown b
    makeMouseEvent b MouseButtonState'Released _ = MouseUp b

    makeKeyEvent k _ (KeyState'Pressed)   mods = KeyDown k
    makeKeyEvent k _ (KeyState'Released)  mods = KeyUp k
    makeKeyEvent k _ (KeyState'Repeating) mods = KeyRepeat k
    
    putEvent :: TChan AppEvent -> AppEvent -> IO ()
    putEvent ch = liftIO . atomically . writeTChan ch


-- |
-- TODO | - How do we deal with repeat events (does it even matter)?
processEvents :: App os -> AppT os (App os)
processEvents app = do
  events <- liftIO $ whileJust (atomically . tryReadTChan $ app^.input.inputChannel) return
  St.execStateT (mapM set events) app
  where
    -- TODO: Clean up on aisle 7
    set :: AppEvent -> St.StateT (App os) (ContextT GLFW.Handle os IO) ()
    set e = do
      s  <- St.get
      s' <- lift $ onevent s e
      St.put s'

-- Events --------------------------------------------------------------------------------------------------------------------------------------------

-- |
onclosing :: App os -> AppT os (App os)
onclosing app = liftIO $ do
  putStrLn "\n\n\nI'm leaving"
  return app


-- |
onkeydown :: App os -> Key -> AppT os (App os)
onkeydown app k = case k of
  Key'Space  -> clearCanvas app
  Key'Escape -> quitApplication app
  Key'S      -> saveCanvas app
  _          -> return app


-- |
onmotion :: App os -> V2 Float -> AppT os (App os)
onmotion app pos = do
  when (app^.input.mouse.buttons.contains MouseButton'1) (void $ drawWithBrush app pos)
  when (app^.input.mouse.buttons.contains MouseButton'2) (void $ erasePixel app pos)
  return app


-- |
onmousedown :: App os -> MouseButton -> AppT os (App os)
onmousedown app b = case b of
  MouseButton'1 -> drawWithBrush app (app^.input.mouse.cursor) >> return app
  MouseButton'2 -> erasePixel    app (app^.input.mouse.cursor) >> return app
  _             -> return app


-- |
onscroll :: App os -> V2 Double -> AppT os (App os)
onscroll app (V2 dx dy) = flip St.execStateT app $ do
  -- TODO: Merge brush and palette
  easel.palette %= stepBy (floor dy)
  new <- St.get -- TODO: Refactor
  easel.brush.colour .= (current $ new^.easel.palette)


-- |
onevent :: App os -> AppEvent -> AppT os (App os)
onevent app e = case e of
  MouseMotion mpos -> do
    let npos = realToFrac <$> mpos
    newapp <- return $ flip St.execState app $ do
      input.mouse.cursor .= npos
    onmotion newapp npos
  KeyDown k   -> onkeydown (app & input.keyboard.contains k .~ True) k
  KeyUp k     -> return (app & input.keyboard.contains k .~ False)
  MouseDown b -> onmousedown (app & input.mouse.buttons.contains b .~ True) b
  MouseUp b   -> return (app & input.mouse.buttons.contains b .~ False)
  MouseScroll sc -> onscroll (app & input.scroll %~ (+ sc)) sc
  WindowClosing -> onclosing app
  _ -> return app

-- Linear algebra (coordinate systems) ---------------------------------------------------------------------------------------------------------------

-- TODO | - Factor out, refactor
--        - Type safety (use types to distinguish different coordinate systems)

-- |
transformInverse :: App os -> M44 Float
transformInverse = inv44 . modelProj


-- |
modelProj app = (app^.uniforms.singular (matrices.values.ix 0)) !*! (app^.uniforms.singular (matrices.values.ix 1))


-- | Convert screen coordinates to canvas coordinates
-- TODO | - This function has to be a lot cleverer
--        - Take viewport into account
--        - Factor out
--        - All 2D (?)
--        - Should W be 0 or 1
toCanvasCoords :: App os -> V2 Float -> V2 Float
toCanvasCoords app (V2 px py) = dropZW $ screenToNormalised app !*! fmap (fmap realToFrac) (transformInverse app) !* V4 px py 0 1
-- toCanvasCoords app p = dropZW $ (fmap realToFrac <$> transformInverse app) !* to4D 0 (to3D 0 p)
--fmap fromIntegral . (\(V2 mx my) -> V2 (mx) (app^.canvas.size.y-my) + origin app) . fmap floor


-- |
-- TODO | - Make sure this is correct
--        -
screenToNormalised :: App os -> M44 Float
screenToNormalised app = translated !*! scaled (V4 (sx) (-sy) 1 1)
  where
    (V2 sx sy) = (2*) . recip . fromIntegral <$> app^.input.size
    translated = identity & translation .~ V3 0 (fromIntegral $ app^.easel.canvas.size.y) 0


-- toTextureCoords :: App os -> V2 Double -> V2 Int


-- |
-- origin :: App os -> V2 Int
-- origin app = (`div` 2) <$> (app^.easel.canvas.size - app^.easel.canvas.size)

-- Debugging and logging ----------------------------------------------------------------------------------------------------------------------------

-- |
alignedCoords :: String -> V2 Float -> IO ()
alignedCoords label coord = let (V2 x y) = round <$> coord in printf "%-10s% 5d % 5d\n" (label ++ ":") (x :: Int) y


-- |
debugScreen :: App os -> AppT os ()
debugScreen app = liftIO $ do
  let sm = app^.input.mouse.cursor
      cm = toCanvasCoords app sm
  alignedCoords "Screen" sm
  alignedCoords "Canvas" cm
  printf "% 6s|%-6s\n" (show $ app^.input.mouse.buttons.contains MouseButton'1) (show $ app^.input.mouse.buttons.contains MouseButton'2)
  cursorUp 3

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Rename
--        -
main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do

  (Right config) <- liftIO $ loadConfig (root </> "assets/settings/config.json")

  win <- newWindow (WindowFormatColorDepth RGB8 Depth32) $ WindowConfig {
                                                             configWidth   = config^.windowSize.x
                                                           , configHeight  = config^.windowSize.y
                                                           , configTitle   = "Pixels"
                                                           , configMonitor = Nothing
                                                           , configHints   = []
                                                           , configSwapInterval = Nothing }

  cvs <- Render.newCanvas (config^.canvasSize) (config^.canvasColour)
  br  <- Render.newBrush  (V2 0 0)             (config^.brushColour)
  us  <- Render.newUniforms config

  -- TODO: How do you use the same shader for different topologies?
  shade <- compileShader $ do
    canvasFragments <- Render.texturedShader
    drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBFloat)) canvasFragments
    pointFragments  <- Render.colorShader
    drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBFloat)) pointFragments
    -- ContextColorOption NoBlending (pure True :: ColorMask RGBFloat)

  -- Events
  channel <- setupEvents win

  -- Off we go
  mainloop $ App {
    fRasterOptions = (FrontAndBack, ViewPort (V2 0 0) (config^.windowSize), DepthRange 0 1),
    fShader        = shade,
    fUniforms      = us,
    fEasel         = Easel {
                       fCanvas  = cvs,
                       fBrush   = br,
                       fPalette = let (Just xs) = newCircleList (config^.easelPalette) in xs }, -- TODO: Don't assume
    fWindow        = win,
    fInput         = initialInput channel (config^.windowSize) -- (InputChannel channel)
  }


-- |
tick :: App os -> AppT os (App os)
tick app' = do
  -- Read input
  app <- processEvents app'

  -- Write uniforms
  writeBuffer (app^.uniforms.matrices.buffer) 0 (app^.uniforms.matrices.values)
  writeBuffer (app^.uniforms.vectors.buffer)  0 (app^.uniforms.vectors.values)
  writeBuffer (app^.uniforms.scalars.buffer)  0 (app^.uniforms.scalars.values)

  -- Write attributes
  writeBuffer (app^.easel.brush.positionBuffer) 0 [let (V2 x y) = app^.input.mouse.cursor
                                                       pos      = screenToNormalised app !* (V4 x y 0 1)
                                                       pixel    = app^.easel.brush.colour --fromIntegral <$> head (app^.easel.palette)
                                                   in (pos, V3 0 0 0)]

  -- Paint the canvas
  -- TODO: Apply transform
  debugScreen app
  -- when (app^.input.mouse.buttons.contains MouseButton'1) $ do
  --   liftIO $ putStrLn "Draw"
  --   void $ drawPixelOnCanvas app (app^.input.mouse.cursor) (app^.easel.brush.colour)

  -- when (app^.input.mouse.buttons.contains MouseButton'2) $ do
  --   liftIO $ putStrLn "Erase"
  --   (void $ drawPixelOnCanvas app (app^.input.mouse.cursor) (app^.easel.canvas.colour))
  
  return app


-- |
mainloop :: App os -> AppT os ()
mainloop app' = do
  Render.render app'
  swapWindowBuffers $ app'^.window
  app <- tick app'
  closeRequested <- fromMaybe True <$> GLFW.windowShouldClose (app^.window)
  unless closeRequested $ mainloop app