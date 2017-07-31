--
-- Pixels.Interaction
-- Interaction...
--
-- Jonatan H Sundqvist
-- August 20 2016
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



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Interaction where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
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
import qualified Graphics.GPipe.Context.GLFW        as Context
import           Graphics.GPipe.Context.GLFW        (Key(..), KeyState(..), MouseButton(..), WindowConf(..))
import           Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow (..))

import           Leibniz.Constants (π)
import Cartesian.Core (x,y,z)

import           Pixels.Types
import           Pixels.Lenses
import           Pixels.Trinkets
import           Pixels.Render as Render
import qualified Pixels.Load   as Load


------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

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
keyDown :: Context.Key -> AppT os Bool
keyDown which = fmap (== Context.KeyState'Pressed) (Context.getKey which)


-- | Get the state of a key, along with the key itself (useful for building maps)
key :: Context.Key -> AppT os (Context.Key, Bool)
key which = fmap (which,) (keyDown which)


-- | Get the position of the mouse, if the given mouse button is being pressed
-- TODO | - Rename (?)
--        - Refactor
mousePressAt :: Context.MouseButton -> AppT os (Maybe (V2 Double))
mousePressAt button = do
  pressed <- Context.getMouseButton button
  if (pressed == Context.MouseButtonState'Pressed)
    then fmap (Just . uncurry V2) Context.getCursorPos
    else return Nothing

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do
  runContextT (Context.newContext' [] $ WindowConf { width=512, height=512, title="Pixels" }) (ContextFormatColorDepth RGB8 Depth32) $ do
    let root = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels" -- TODO: Don't hard-code

    cvs <- Render.newCanvas (V2 512 512)
    us  <- Render.newUniforms

    -- TODO: How do you use the same shader for different topologies?
    shade <- compileShader $ do
      fragmentStream <- Render.texturedShader
      drawContextColor (const (ContextColorOption NoBlending (pure True))) (fragmentStream)

    mainloop $ App {
      fRasterOptions = (FrontAndBack, ViewPort (V2 0 0) (V2 512 512), DepthRange 0 1),
      fShader        = shade,
      fUniforms      = us,
      fCanvas        = cvs
    }


-- |
tick :: App os -> AppT os (App os)
tick app' = do
  -- Read input
  -- TODO: No hard-coded constants (I'm looking at YOU, client size!)
  mouse@(V2 mx my) <- fmap realToFrac . uncurry V2 <$> Context.getCursorPos
  mpress           <- mousePressAt Context.MouseButton'1

  keyboard <- fmap S.fromList (filterM keyDown [Context.Key'Space .. Context.Key'Menu])

  -- New app state
  app <- flip St.execStateT app' $ do
    uniforms.vectors.values.ix 0  .= V3 mx my 0
    uniforms.matrices.values.ix 0 .= ortho (-client^.x/2) (client^.x/2) (-client^.y/2) (client^.y/2) 0 1
    uniforms.matrices.values.ix 1 .= (identity & translation.z .~ 0)

    when (keyboard^.contains Key'Space) $ do
      app <- St.get
      blank <- lift (Render.new (app^.canvas.size) (\x y -> V3 30 20 240))
      canvas.texture .= blank

  -- Write uniforms
  writeBuffer (app^.uniforms.matrices.buffer) 0 (app^.uniforms.matrices.values)
  writeBuffer (app^.uniforms.vectors.buffer)  0 (app^.uniforms.vectors.values)
  writeBuffer (app^.uniforms.scalars.buffer)  0 (app^.uniforms.scalars.values)



  -- Paint the canvas
  -- TODO: Apply transform
  maybe pass (write app) mpress

  return app
  where
    projection :: Simple Traversal (App os) (M44 Float)
    projection = uniforms.matrices.values.ix 0

    modelview :: Simple Traversal (App os) (M44 Float)
    modelview = uniforms.matrices.values.ix 1

    transformInverse :: App os -> M44 Float
    transformInverse app = inv44 ((app^.singular projection) !*! (app^.singular modelview))

    -- |
    -- TODO | - Do not hard-code
    client :: Num n => V2 n
    client = fmap fromIntegral $ V2 512 512

    -- | Convert screen coordinates to canvas coordinates
    -- TODO | - This function has to be a lot cleverer
    --        - Factor out
    toCanvasCoords :: App os -> V2 Double -> V2 Double
    toCanvasCoords app = fmap fromIntegral . (\(V2 mx my) -> V2 (mx) (client^.y-my) + origin app) . fmap floor

    -- |
    origin :: App os -> V2 Int
    origin app = fmap (divBy 2) (app^.canvas.size - client)

    -- |
    divBy :: Integral n => n -> n -> n
    divBy = flip div

    -- toTextureCoords :: App os -> V2 Double -> V2 Int

    write app p = do
      liftIO . print $ (p, toCanvasCoords app p)
      r <- Render.writePixel (fmap floor $ toCanvasCoords app p) (V3 230 62 120) (app^.canvas.texture)
      liftIO (print r)
      pass


-- |
mainloop :: App os -> AppT os ()
mainloop app' = do
  Render.render app'
  swapContextBuffers
  app <- tick app'
  closeRequested <- Context.windowShouldClose
  unless closeRequested $ mainloop app
