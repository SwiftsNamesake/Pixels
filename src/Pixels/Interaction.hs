-- --
-- -- Pixels.Interaction
-- -- Interaction...
-- --
-- -- Jonatan H Sundqvist
-- -- August 20 2016
-- --

-- -- TODO | - High-level interaction and events
-- --          -- Composite events (eg. pressing while moving mouse)
-- --          -- Gestures (?)
-- --          -- Key combos
-- --          -- Mouse delta
-- --        - Key maps (dynamic)
-- --        - Introspection (for debuggibg)
-- --        - Visual feedback and bindings menu

-- -- SPEC | -
-- --        -



-- GHC pragmas ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PackageImports    #-}

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
keyDown :: App os -> Key -> AppT os Bool
keyDown app k = maybe False (== KeyState'Pressed) <$> GLFW.getKey (app^.window) k


-- | Get the state of a key, along with the key itself (useful for building maps)
key :: App os -> Key -> AppT os (GLFW.Key, Bool)
key app k = (k,) <$> (keyDown app k)


-- | Get the position of the mouse, if the given mouse button is being pressed
-- TODO | - Rename (?)
--        - Refactor
mousePressAt :: App os -> MouseButton -> AppT os (Maybe (V2 Double))
mousePressAt app button = do
  pressed <- (== MouseButtonState'Pressed) . fromMaybe MouseButtonState'Released <$> (GLFW.getMouseButton (app^.window) button)
  if pressed
    -- TODO: BAAAAD, don't replace Nothing with (0,0)
    then (Just . uncurry V2 . fromMaybe (0,0)) <$> (GLFW.getCursorPos $ app^.window)
    else return Nothing

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
  let root = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels" -- TODO: Don't hard-code
  win <- newWindow (WindowFormatColorDepth RGB8 Depth32) $ WindowConfig {
                                                              configWidth   = 512
                                                            , configHeight  = 512
                                                            , configTitle   = "Pixels"
                                                            , configMonitor = Nothing
                                                            , configHints   = []
                                                            , configSwapInterval = Nothing }
  cvs <- Render.newCanvas (V2 512 512)
  us  <- Render.newUniforms

  -- TODO: How do you use the same shader for different topologies?
  shade <- compileShader $ do
    fragmentStream <- Render.texturedShader
    drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBFloat)) fragmentStream
    -- ContextColorOption NoBlending (pure True :: ColorMask RGBFloat)

  mainloop $ App {
    fRasterOptions = (FrontAndBack, ViewPort (V2 0 0) (V2 512 512), DepthRange 0 1),
    fShader        = shade,
    fUniforms      = us,
    fCanvas        = cvs,
    fWindow        = win
  }


-- |
tick :: App os -> AppT os (App os)
tick app' = do
  -- Read input
  -- TODO: No hard-coded constants (I'm looking at YOU, client size!)
  mouse@(V2 mx my) <- fmap realToFrac . uncurry V2 . fromMaybe (0,0) <$> GLFW.getCursorPos (app'^.window)
  mpress           <- mousePressAt app' MouseButton'1

  keyboard <- S.fromList <$> (filterM (keyDown app') [Key'Space .. Key'Menu])

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
    client = fromIntegral <$> V2 512 512

    -- | Convert screen coordinates to canvas coordinates
    -- TODO | - This function has to be a lot cleverer
    --        - Factor out
    toCanvasCoords :: App os -> V2 Double -> V2 Double
    toCanvasCoords app = fmap fromIntegral . (\(V2 mx my) -> V2 (mx) (client^.y-my) + origin app) . fmap floor

    -- |
    origin :: App os -> V2 Int
    origin app = divBy 2 <$> (app^.canvas.size - client)

    -- |
    divBy :: Integral n => n -> n -> n
    divBy = flip div

    -- toTextureCoords :: App os -> V2 Double -> V2 Int

    write app p = do
      liftIO . print $ (p, toCanvasCoords app p)
      r <- Render.writePixel (floor <$> toCanvasCoords app p) (V3 230 62 120) (app^.canvas.texture)
      liftIO (print r)
      pass


-- -- |
mainloop :: App os -> AppT os ()
mainloop app' = do
  Render.render app'
  swapWindowBuffers $ app'^.window
  app <- tick app'
  closeRequested <- fromMaybe True <$> GLFW.windowShouldClose (app'^.window)
  unless closeRequested $ mainloop app

-- {-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
-- module Pixels.Interaction where

-- import Control.Monad
-- import Text.Printf (printf)
-- import Control.Monad.IO.Class

-- import Graphics.GPipe
-- import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
-- import qualified "JuicyPixels" Codec.Picture as Juicy
-- import qualified "JuicyPixels" Codec.Picture.Types as Juicy

-- main :: IO ()
-- main =
--   runContextT GLFW.defaultHandleConfig $ do
--     -- Create vertex data buffers
--     liftIO $ putStrLn "Running CONTEXT"
--     win <- newWindow (WindowFormatColorDepth SRGB8 Depth16) (GLFW.defaultWindowConfig "test it!")
--     positions :: Buffer os (B2 Float) <- newBuffer 4
--     normals   :: Buffer os (B3 Float) <- newBuffer 6
--     tangents  :: Buffer os (B3 Float) <- newBuffer 6
--     writeBuffer positions 0 [V2 1 1,   V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
--     writeBuffer normals   0 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
--     writeBuffer tangents  0 [V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1), V3 (-1) 0 0, V3 1 0 0]


--     -- Spew scroll info
--     liftIO $ putStrLn "Setting scroll"
--     void . GLFW.setScrollCallback win . pure $
--         \dx dy -> printf "scroll dx%v dy%v on %v\n" dx dy

--     -- Make a Render action that returns a PrimitiveArray for the cube
--     let makePrimitives = do
--           pArr <- newVertexArray positions
--           nArr <- newVertexArray normals
--           tArr <- newVertexArray tangents
--           let sideInstances = zipVertices (,) nArr tArr
--           return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr sideInstances

--     -- Load image into texture
--     liftIO $ putStrLn "Loading image"
--     im <- liftIO $ Juicy.readImage "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels/assets/textures/image.jpg"
--     liftIO $ putStrLn "Done loading image"

--     let image = case im of
--             Right (Juicy.ImageYCbCr8 i) -> i
--             Right _ -> error "Got unexpected image color space"
--             Left s -> error s
--         size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)

--     tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
--     writeTexture2D tex 0 0 size $ Juicy.pixelFold getJuicyPixel [] image
--     generateTexture2DMipmap tex

--     -- Create a buffer for the uniform values
--     uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1

--     -- Create the shader
--     shader <- compileShader $ do
--       sides <- fmap makeSide <$> toPrimitiveStream primitives
--       (modelViewProj, normMat) <- getUniform (const (uniform, 0))
--       let filterMode = SamplerFilter Linear Linear Linear (Just 4)
--           edgeMode = (pure ClampToEdge, undefined)
--           projectedSides = proj modelViewProj normMat <$> sides
--       samp <- newSampler2D (const (tex, filterMode, edgeMode))

--       fragNormalsUV <- rasterize rasterOptions projectedSides
--       let litFrags = light samp <$> fragNormalsUV
--           litFragsWithDepth = withRasterizedInfo
--               (\a x -> (a, getZ $ rasterizedFragCoord x)) litFrags
--           colorOption = ContextColorOption NoBlending (pure True)
--           depthOption = DepthOption Less True

--       drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

--     -- Run the loop
--     loop win shader makePrimitives uniform 0

-- loop win shader makePrimitives uniform angleRot = do
--   Just (cursorX, cursorY) <- GLFW.getCursorPos win
--   mouseButton1 <- GLFW.getMouseButton win GLFW.MouseButton'1
--   spaceKey <- GLFW.getKey win GLFW.Key'Space
--   shouldClose <- GLFW.windowShouldClose win
-- --liftIO $ printf "cursorPos x%v y%v, mouseButton1 %v, spaceKey %v, shouldClose %v\n"
-- --  cursorX cursorY (show mouseButton1) (show spaceKey) (show shouldClose)
--   -- Write this frames uniform value
--   -- size@(V2 w h) <- getWindowSize win
--   let size@(V2 w h) = V2 720 480
--   let modelRot = fromQuaternion (axisAngle (V3 1 0.5 0.3) angleRot)
--       modelMat = mkTransformationMat modelRot (pure 0)
--       projMat = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
--       viewMat = mkTransformationMat identity (- V3 0 0 5)
--       viewProjMat = projMat !*! viewMat !*! modelMat
--       normMat = modelRot
--   writeBuffer uniform 0 [(viewProjMat, normMat)]

--   -- Render the frame and present the results
--   render $ do
--     clearWindowColor win 0 -- Black
--     clearWindowDepth win 1 -- Far plane
--     prims <- makePrimitives
--     shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
--   swapWindowBuffers win

--   Just closeRequested <- GLFW.windowShouldClose win
--   unless closeRequested $
--     loop win shader makePrimitives uniform ((angleRot + 0.0015) `mod''` (2*pi))

-- getJuicyPixel xs _x _y pix =
--   let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs

-- getZ (V4 _ _ z _) = z -- Some day I'll learn to use lenses instead...

-- data ShaderEnvironment = ShaderEnvironment
--   { primitives :: PrimitiveArray Triangles (B2 Float, (B3 Float, B3 Float))
--   , rasterOptions :: (Side, ViewPort, DepthRange)
--   }

-- -- Project the sides coordinates using the instance's normal and tangent
-- makeSide (p@(V2 x y), (normal, tangent)) =
--   (V3 x y 1 *! V3 tangent bitangent normal, normal, uv)
--   where bitangent = cross normal tangent
--         uv = (p + 1) / 2

-- -- Project the cube's positions and normals with ModelViewProjection matrix
-- proj modelViewProj normMat (V3 px py pz, normal, uv) =
--   (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv))

-- -- Set color from sampler and apply directional light
-- light samp (normal, uv) =
--   sample2D samp SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1)

-- -- eof