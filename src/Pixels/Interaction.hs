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

import           Control.Monad.Trans.Class as St
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as St
import           Control.Monad.Trans.Either
import           Control.Lens hiding (argument, (<.>))
import           Control.Monad
import           Control.Monad.Loops (whileM)
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

-- User interaction ----------------------------------------------------------------------------------------------------------------------------------

-- | TODO - We need to rehaul the events interface

-- Gets the current cursor position, in pixels relative to the top-left corner of the window.
-- getCursorPos :: MonadIO m => ContextT GLFWWindow os f m (Double, Double)

-- Gets the state of the specified MouseButton.
-- getMouseButton :: MonadIO m => MouseButton -> ContextT GLFWWindow os f m MouseButtonState

-- Gets the state of the specified Key.
-- getKey :: MonadIO m => Key -> ContextT GLFWWindow os f m KeyState

-- Registers the specified ScrollCallback.
-- registerScrollCallback :: MonadIO m => Maybe ScrollCallback -> ContextT GLFWWindow os f m ()

-- windowShouldClose :: MonadIO m => ContextT GLFWWindow os f m Bool


-- |
-- pressed :: S


-- |
-- TODO | - Move
--        - Don't hard-code
root :: FilePath
root = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"


-- |
initialInput :: InputChannel -> Input
initialInput ch = Input {
                    fMouse = Mouse {
                      fCursor  = V2 0 0,
                      fButtons = S.empty },
                    fKeyboard  = S.empty,
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
  us  <- Render.newUniforms

  -- TODO: How do you use the same shader for different topologies?
  shade <- compileShader $ do
    canvasFragments <- Render.texturedShader
    pointFragments  <- Render.colorShader
    drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBFloat)) canvasFragments
    drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBFloat)) pointFragments
    -- ContextColorOption NoBlending (pure True :: ColorMask RGBFloat)

  -- Events
  channel <- liftIO newTChanIO

  -- TODO | - KeyDown
  --        - KeyUp
  --        - FileChange

  -- TODO | - Add modifiers, scancode (?)
  let makeMouseEvent b MouseButtonState'Pressed  _ = MouseDown b
      makeMouseEvent b MouseButtonState'Released _ = MouseUp b

      makeKeyEvent k _ (KeyState'Pressed)   mods = KeyDown k
      makeKeyEvent k _ (KeyState'Released)  mods = KeyUp k
      makeKeyEvent k _ (KeyState'Repeating) mods = KeyRepeat k
      
      putEvent :: AppEvent -> IO ()
      putEvent = liftIO . atomically . writeTChan channel

  GLFW.setDropCallback        win (Just $ \paths            -> putEvent $ FileDrop paths)
  GLFW.setScrollCallback      win (Just $ \sx sy            -> putEvent $ MouseScroll (V2 sx sy))
  GLFW.setCursorPosCallback   win (Just $ \mx my            -> putEvent $ MouseMotion (V2 mx my))
  GLFW.setMouseButtonCallback win (Just $ \b bstate mods    -> putEvent $ makeMouseEvent b bstate mods)
  GLFW.setKeyCallback         win (Just $ \k sc kstate mods -> putEvent $ makeKeyEvent k sc kstate mods)
  -- setCursorEnterCallback :: MonadIO m => Window os c ds -> Maybe (CursorState -> IO ()) -> ContextT Handle os m (Maybe ())

  -- setClipboardString 
  -- getClipboardString 

  -- setWindowShouldClose :: MonadIO m => Window os c ds -> Bool -> ContextT Handle os m (Maybe ())
  -- setWindowCloseCallback :: MonadIO m => Window os c ds -> Maybe (IO ()) -> ContextT Handle os m (Maybe ())

  -- Off we go
  mainloop $ App {
    fRasterOptions = (FrontAndBack, ViewPort (V2 0 0) (config^.windowSize), DepthRange 0 1),
    fShader        = shade,
    fUniforms      = us,
    fEasel         = Easel {
                       fCanvas  = cvs,
                       fBrush   = br,
                       fPalette = config^.easelPalette },
    fWindow        = win,
    fInput         = initialInput channel -- (InputChannel channel)
  }


-- |
onkeydown :: App os -> Key -> AppT os (App os)
onkeydown app k = case k of
  Key'Space  -> clearCanvas app
  Key'Escape -> quitApplication app
  Key'S      -> saveCanvas app
  _          -> return app


-- |
onevent :: App os -> AppT os (App os)
onevent app e = _


-- |
processEvents :: App os -> AppT os Input
processEvents app = do
  -- cursor   <- fmap realToFrac . uncurry V2 . fromMaybe (0,0) <$> GLFW.getCursorPos (app^.window)
  -- keys     <- S.fromList <$> filterM (keyDown    $ app^.window) [Key'Space .. Key'Menu] --[toEnum 0 ..]
  -- mbuttons <- S.fromList <$> filterM (buttonDown $ app^.window) [toEnum 0 ..]

  -- whileM (/= Nothing) (tryReadTChan $ app^.input.inputChannel)
  liftIO $ (atomically . tryReadTChan $ app^.input.inputChannel) >>= print
  return $ Input {
    fMouse    = Mouse { fCursor = cursor, fButtons = mbuttons },
    fKeyboard = keys,
    fInputChannel = app^.input.inputChannel
  }


-- |
tick :: App os -> AppT os (App os)
tick app' = do
  -- Read input
  input' <- processEvents app'

  -- New app state
  app <- flip St.execStateT app' $ do
    app <- St.get
    input .= input'
    uniforms.vectors.values.ix 0 .= (realToFrac <$> to3D 0 (app^.input.mouse.cursor))
    uniforms.projection          .= (let (V2 x' y') = (*0.5) . fromIntegral <$> (app^.easel.canvas.size) in ortho (-x') (x') (-y') (y') 0 1)
    uniforms.modelview           .= (identity & translation.z .~ 0)

  -- Write uniforms
  writeBuffer (app^.uniforms.matrices.buffer) 0 (app^.uniforms.matrices.values)
  writeBuffer (app^.uniforms.vectors.buffer)  0 (app^.uniforms.vectors.values)
  writeBuffer (app^.uniforms.scalars.buffer)  0 (app^.uniforms.scalars.values)

  -- Write attributes
  writeBuffer (app^.easel.brush.positionBuffer) 0 [let (V2 x y) = app^.input.mouse.cursor
                                                       pos      = screenToNormalised app !* (V4 x y 0 1)
                                                       pixel    = fromIntegral <$> head (app^.easel.palette)
                                                   in (pos, V3 0 0 0)]

  -- Paint the canvas
  -- TODO: Apply transform
  void $ do
    maybe pass (\p -> writePixel app p (app^.easel.brush.colour)  >> debugCoords app) (mousepressAt app MouseButton'1)
    maybe pass (\p -> writePixel app p (app^.easel.canvas.colour) >> debugCoords app) (mousepressAt app MouseButton'2)
  
  return app
  where
    alignedCoords :: String -> V2 Float -> IO ()
    alignedCoords label coord = let (V2 x y) = round <$> coord in printf "%-10s% 4d % 4d\n" (label ++ ":") (x :: Int) y

    debugCoords app = liftIO $ do
      let sm = app^.input.mouse.cursor
          cm = toCanvasCoords app sm
      alignedCoords "Screen" sm
      alignedCoords "Canvas" cm
      cursorUp 2

    -- TODO | - Factor out, refactor
    transformInverse :: App os -> M44 Float
    transformInverse = inv44 . modelProj

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
    screenToNormalised :: App os -> M44 Float
    screenToNormalised app = (identity & translation .~ V3 0 (fromIntegral $ app^.easel.canvas.size.y) 0) !*! scaled (V4 (2*1/512) (-2*1/512) 1 1)

    -- toTextureCoords :: App os -> V2 Double -> V2 Int
    projection :: (IxValue [M44 Float] -> Identity (IxValue [M44 Float])) -> UniformData os -> Identity (UniformData os)
    projection = matrices.values.ix 0
    modelview  = matrices.values.ix 1

    -- |
    origin :: App os -> V2 Int
    origin app = (`div` 2) <$> (app^.easel.canvas.size - app^.easel.canvas.size)

    writePixel app p colour = Render.writePixel (floor <$> toCanvasCoords app p) colour (app^.easel.canvas.texture)


-- -- |
mainloop :: App os -> AppT os ()
mainloop app' = do
  Render.render app'
  swapWindowBuffers $ app'^.window
  app <- tick app'
  closeRequested <- fromMaybe True <$> GLFW.windowShouldClose (app^.window)
  unless closeRequested $ mainloop app