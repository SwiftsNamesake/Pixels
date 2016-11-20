
--
-- Pixels.Render
-- Rendering...
--
-- Jonatan H Sundqvist
-- August 19 2016
--

-- TODO | - Logging (ornate, sophisticated, colours, LOG, ERROR, etc.)
--        - Robust path handling
--        - Consistent and elegant error handling (cf. EitherT)
--        - Use something other than lists (eg. vectors) (?)

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
--{-# LANGUAGE OverlappingInstances #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Render where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStrLn, putStr, print, putChar)
-- import qualified Prelude as P

import Text.Printf

-- import Linear.Projection
-- import Linear.Quaternion
import Linear (M44, (!*!), translation, identity, perspective, ortho, V2(..), V3(..))

-- import           Data.Bits
import           Data.Word
import           Data.Maybe (fromMaybe)
import qualified Data.Map         as M

import           Control.Monad.Trans.Either
import           Control.Lens hiding (argument)
import           Control.Monad
-- import           Control.Applicative ((<$>), (<*>), liftA2)

import qualified Graphics.UI.GLFW as GLFW

-- import Graphics.Rendering.FTGL as FTGL
-- import Graphics.Rendering.TrueType.STB as STB

import Foreign.Storable
import Foreign.Storable.Tuple ()
import Foreign.C.Types        (CUChar)
import Foreign.Marshal    as Marshal hiding (void)
import Foreign.Ptr        as Ptr
import Foreign.ForeignPtr as FPtr

import Leibniz.Constants (π)
import Cartesian.Core (x,y,z)

import           Graphics.Michelangelo.Transformations
import qualified Graphics.Michelangelo.Shaders as Shader
import           Graphics.Michelangelo.Texture (setTexture)
import qualified Graphics.Michelangelo.Shapes  as Shapes
import           Graphics.Michelangelo.Types (Matrix44(..))

import Pixels.Types
import Pixels.Lenses as L
import Pixels.Trinkets



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- OpenGL state-machinery ----------------------------------------------------------------------------------------------------------------------------

-- |
setupOpenGL :: IO ()
setupOpenGL = do
  putStrLn "Setting up OpenGL"
  clearColor $= GL.Color4 0.032 0.029 1.027 1.0

  blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  blend     $= GL.Enabled
  depthFunc $= Just GL.Lequal
  -- depthMask $= GL.Enabled
  -- depthTest $= GL.Enabled

  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear') -- TODO: We need to change this if we want pixel-perfect zooming
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)


-- | Frame buffer objects
createFBO :: IO ()
createFBO = do
  return ()


-- |
-- TODO: Move to Michelangelo
attribute :: Integral n => (GL.AttribLocation, GL.BufferObject, n) -> IO ()
attribute (loc, buffer, count) = do
  GL.vertexAttribArray loc     $= GL.Enabled                                                                         --
  GL.bindBuffer GL.ArrayBuffer $= Just buffer                                                                        --
  GL.vertexAttribPointer loc   $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral count) GL.Float 0 GL.offset0) --

-- Camera operations ---------------------------------------------------------------------------------------------------------------------------------

-- | TODO: Rename
-- positionCamera :: Camera -> IO ()

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (too similar to render)
-- TODO: Error handling
-- TODO: Arbitrary uniforms and attributes  (?)
-- TODO: Custom setup functions (per mesh)  (?)
-- TODO: Cache attrib and uniform locations (?)
draw :: Debug -> V3 Float -> Program -> M44 Float -> M44 Float -> Mesh -> IO ()
draw db mouse program pm mv mesh = do
  
  -- TODO: Factor out

  t <- realToFrac . fromMaybe 0.0 <$> GLFW.getTime -- Time (s)
  logStr db InfoLevel 1 "Drawing mesh"

  -- Set attribute buffers
  forM ["aVertexPosition", "aVertexColor", "aTexCoord"] $ \attr -> do
    logStr db InfoLevel 2 $ printf "Setting attribute: %s" attr
    loc <- get $ attribLocation  program attr
    logStr db InfoLevel 2 $ printf "%s" (show loc)
    -- TODO: Use EitherT instead of assuming the item is found
    let Just (buffer, count) = M.lookup attr (mesh^.attributeBuffers)
    logStr db InfoLevel 2 $ printf "(Buffer: %s, Count: %d)" (show buffer) count
    attribute (loc, buffer, count)
  
  -- Set uniform values
  forM [("uMVMatrix", wrap $ Matrix44 mv),
        ("uPMatrix",  wrap $ Matrix44 pm),
        ("uMouseVec", wrap $ mouse),
        -- ("uTex0",     wrap $ (0 :: GLint)),
        ("uTime",     wrap $ (t :: GLfloat))] $ \(u, v) -> do
    logStr db InfoLevel 2 $ printf "Setting uniform: %s" (show u)
    get (GL.uniformLocation program u) >>= v
  
  -- Set texture
  when (not . null $ mesh^.textures) $ do
    logStr db InfoLevel 2 "Setting texture"
    setTexture program $ chooseTexture (mesh^.textures) t
  
  logStr db InfoLevel 2 $ printf "Drawing arrays (%d vertices)" (mesh^.numVertices)
  GL.drawArrays (mesh^.primitive) 0 (fromIntegral $ mesh^.numVertices) --
  where
    chooseTexture [tex] _ = tex
    chooseTexture texes t = texes !! mod (frame t) (length texes)

    fps = 12 -- TODO: Move this (along with all animation logic)
    frame t = floor $ t*fps

    wrap :: (Show u, GL.Uniform u) => u -> GL.UniformLocation -> IO ()
    wrap v loc = do
      logStr db InfoLevel 2 $ printf "Setting uniform: (Location=%s, Value=%s)" (show loc) (show v)
      GL.uniform loc $= v


-- | 
-- TODO: Scenes and cameras
render :: AppState -> IO ()
render app = do

  -- OpenGL config
  GL.lineWidth $= 0.01
  -- GL.lineStipple $= Just (4, 0xFF)

  -- Clear
  blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  blend     $= GL.Enabled
  depthFunc $= Just GL.Lequal

  GL.clearColor $= (app^.graphics.L.clearColour)
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]


  let program'  = app^.graphics.L.program
      meshes'   = app^.graphics.meshes
      onmissing = printf "Could not find mesh: %s" 
      invertY   = y %~ ((+ h) . negate) -- Negate and shift by the height
      drawIt prepare t mesh = prepare >> draw (app^.debug) (invertY mouse') program' pm (mv !*! translate t) mesh
  forM [("grid",      V3     0  0 10, pass),
        ("flame",     V3 ( 300) 0 20, pass),
        ("flame2",    V3 ( 300) 0 20, pass),
        ("square",    V3     0  0 10, pass),
        ("message",   V3     0  0 60, (GL.lineWidth $= 2.0)),
        ("fox",       V3 (-400) 0 10, pass),
        ("interface", V3     0  0  5, pass),
        ("mickey",    V3     0  0  0, pass)] $ \(key, t, prepare) -> do
    logStr (app^.debug) InfoLevel 2 (printf "Drawing %s" key)
    maybe (onmissing key) (drawIt prepare t) (M.lookup key meshes')

  -- Swap buffers
  GLFW.swapBuffers (app^.window)
  where
    -- App data
    (V2 w h) = app^.size
    cam      = app^.graphics.camera

    -- Matrix functions
    translate :: Num n => V3 n -> M44 n
    translate by = identity & (translation .~ by)


    -- Matrices
    mv = (app^.graphics.matModelview) !*! (translate (negate $ cam^.pan)) !*! rotateY (cam^.rotation.y)
    pm = (app^.graphics.matProjection)

    -- Coordinate space operations
    from2D (V2 x' y') = V3 x' y' 0
    screenToWorld (V3 x' y' z') = V3 (x'-w/2) (-y'+h/2) (z') -- TODO: Make sure this is correct and robust

    -- Mouse
    -- mouse'     = from2D . (!!0) $ app^.input.mouse.path --
    mouse' = V3 0 0 0 -- TODO: Fix
    worldMouse = screenToWorld mouse'                   -- Mouse position in world coords


-- |
-- http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
renderToTexture :: IO ()
renderToTexture = do
  --
  putStrLn "Rendering to texture"

  -- Create render target
  -- [name] <- GL.genFramebuffers 1
  -- GL.bindFramebuffer GL.FRAMEBUFFER name
  return ()


-- |
-- stipple :: 
