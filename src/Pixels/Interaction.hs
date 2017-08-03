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
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.IORef
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

import System.FilePath  ((</>), takeExtension, dropExtension, takeFileName) --
import System.Directory (getDirectoryContents)
import System.FSNotify

import           Control.Monad.Trans.Class as St
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as St
import           Control.Monad.Trans.Either
import           Control.Lens hiding (argument)
import           Control.Monad
import           Control.Monad.Loops (whileM)
import           Control.Applicative ((<$>), (<*>), liftA2)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Exception (finally, catch, displayException)

import           Graphics.GPipe hiding (texture)
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import           Graphics.GPipe.Context.GLFW.Input (Key(..), KeyState(..), MouseButton(..), MouseButtonState(..))
import           Graphics.GPipe.Context.GLFW       (WindowConfig(..))

import Leibniz.Constants (π)
import Cartesian.Core    (x,y,z)

import           Pixels.Types
import           Pixels.Lenses
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
initialInput :: Input
initialInput = Input {
                 fMouse = Mouse {
                   fCursor  = V2 0 0,
                   fButtons = S.empty },
                 fKeyboard = S.empty }


-- |
keyDown :: Window os RGBFloat Depth -> Key -> AppT os Bool
keyDown win k = maybe False (== KeyState'Pressed) <$> GLFW.getKey win k


-- |
buttonDown :: Window os RGBFloat Depth -> MouseButton -> AppT os Bool
buttonDown win b = maybe False (== MouseButtonState'Pressed) <$> GLFW.getMouseButton win b


-- | Get the position of the mouse, if the given mouse button is being pressed
-- TODO | - Rename (?)
--        - Refactor
mousepressAt :: App os -> MouseButton -> Maybe (V2 Double)
mousepressAt app button
  | app^.input.mouse.buttons.contains button = Just $ app^.input.mouse.cursor
  | otherwise                                = Nothing

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
  let root   = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels" -- TODO: Don't hard-code
      width  = 512
      height = 512
  win <- newWindow (WindowFormatColorDepth RGB8 Depth32) $ WindowConfig {
                                                              configWidth   = width
                                                            , configHeight  = height
                                                            , configTitle   = "Pixels"
                                                            , configMonitor = Nothing
                                                            , configHints   = []
                                                            , configSwapInterval = Nothing }
  cvs <- Render.newCanvas (V2 64 64)
  us  <- Render.newUniforms

  -- TODO: How do you use the same shader for different topologies?
  shade <- compileShader $ do
    fragmentStream <- Render.texturedShader
    drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBFloat)) fragmentStream
    -- ContextColorOption NoBlending (pure True :: ColorMask RGBFloat)

  mainloop $ App {
    fRasterOptions = (FrontAndBack, ViewPort (V2 0 0) (V2 width height), DepthRange 0 1),
    fShader        = shade,
    fUniforms      = us,
    fEasel         = Easel { fCanvas = cvs, fBrush = V3 230 62 120 } ,
    fWindow        = win,
    fInput         = initialInput
  }


-- |
updateInput :: App os -> AppT os Input
updateInput app = do
  cursor   <- fmap realToFrac . uncurry V2 . fromMaybe (0,0) <$> GLFW.getCursorPos (app^.window)
  keys     <- S.fromList <$> filterM (keyDown    $ app^.window) [Key'Space .. Key'Menu] --[toEnum 0 ..]
  mbuttons <- S.fromList <$> filterM (buttonDown $ app^.window) [toEnum 0 ..]
  return $ Input {
    fMouse    = Mouse { fCursor = cursor, fButtons = mbuttons },
    fKeyboard = keys
  }


-- |
tick :: App os -> AppT os (App os)
tick app' = do
  -- Read input
  -- TODO: No hard-coded constants (I'm looking at YOU, client size!)
  input' <- updateInput app'

  -- New app state
  app <- flip St.execStateT app' $ do
    app <- St.get
    input .= input'
    uniforms.vectors.values.ix 0 .= (realToFrac <$> to3D 0 (app^.input.mouse.cursor))
    uniforms.projection .= (let (V2 x' y') = (*0.5) . fromIntegral <$> (app^.easel.canvas.size) in ortho (-x') (x') (-y') (y') 0 1)
    uniforms.modelview  .= (identity & translation.z .~ 0)

    when (app^.input.keyboard.contains Key'Space) $ do
      -- TODO: Clear texture instead of recreating
      blank <- lift (Render.new (app^.easel.canvas.size) (\_ -> V3 30 20 240))
      easel.canvas.texture .= blank

  -- Write uniforms
  writeBuffer (app^.uniforms.matrices.buffer) 0 (app^.uniforms.matrices.values)
  writeBuffer (app^.uniforms.vectors.buffer)  0 (app^.uniforms.vectors.values)
  writeBuffer (app^.uniforms.scalars.buffer)  0 (app^.uniforms.scalars.values)

  -- Paint the canvas
  -- TODO: Apply transform
  maybe pass (\p -> writePixel app p (app^.easel.brush) >> debugCoords app) (mousepressAt app MouseButton'1)
  
  return app
  where
    debugCoords app = liftIO $ do
      let m = app^.input.mouse.cursor
      printf "Mouse:  %.02f %.02f\n" (m^.x) (m^.y)
      printf "Canvas: %s\n" $ show (floor <$> toCanvasCoords app m :: V2 Int)

    -- TODO | - Factor out, refactor
    transformInverse :: App os -> M44 Float
    transformInverse = inv44 . modelProj

    modelProj app = (app^.uniforms.singular (matrices.values.ix 0)) !*! (app^.uniforms.singular (matrices.values.ix 1))

    -- | Convert screen coordinates to canvas coordinates
    -- TODO | - This function has to be a lot cleverer
    --        - Factor out
    --        - All 2D (?)
    --        - Should W be 0 or 1
    toCanvasCoords :: App os -> V2 Double -> V2 Double
    toCanvasCoords app (V2 px py) = dropZW $ screenToNormalised app !*! fmap (fmap realToFrac) (transformInverse app) !* V4 px py 0 1
    -- toCanvasCoords app p = dropZW $ (fmap realToFrac <$> transformInverse app) !* to4D 0 (to3D 0 p)
    --fmap fromIntegral . (\(V2 mx my) -> V2 (mx) (app^.canvas.size.y-my) + origin app) . fmap floor

    screenToNormalised :: App os -> M44 Double
    screenToNormalised app = (identity & translation .~ V3 0 (fromIntegral $ app^.easel.canvas.size.y) 0) !*! scaled (V4 (2*1/512) (-2*1/512) 1 1)

    -- toTextureCoords :: App os -> V2 Double -> V2 Int
    projection = matrices.values.ix 0
    modelview  = matrices.values.ix 1

    -- |
    origin :: App os -> V2 Int
    origin app = (`div` 2) <$> (app^.easel.canvas.size - app^.easel.canvas.size)

    writePixel app p colour = void $ Render.writePixel (floor <$> toCanvasCoords app p) colour (app^.easel.canvas.texture)


-- -- |
mainloop :: App os -> AppT os ()
mainloop app' = do
  Render.render app'
  swapWindowBuffers $ app'^.window
  app <- tick app'
  closeRequested <- fromMaybe True <$> GLFW.windowShouldClose (app^.window)
  unless closeRequested $ mainloop app