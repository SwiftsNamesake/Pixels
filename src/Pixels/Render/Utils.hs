
--
-- Pixels.Render.Utils
-- Rendering utilities...
--
-- Jonatan H Sundqvist
-- September 18 2016
--

-- TODO | - 
--        - 

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Render.Utils where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Prelude hiding (putStrLn, putStr, print, putChar)

import Data.Word

import Linear.V2

import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Foreign.Storable.Tuple ()
import Foreign.Ptr        as Ptr

import           Data.Array.Repa ((:.)(..)) -- Weirdest syntax ever
import qualified Data.Array.Repa as R

import Graphics.Michelangelo.Types (Image, Pixel)

import Graphics.Rendering.OpenGL                  as GL hiding (projection, perspective, Line, position, ortho)
-- import Graphics.Rendering.OpenGL.GL.BufferObjects as GL
-- import Graphics.Rendering.OpenGL.GL.Shaders       as GL --
-- import Graphics.Rendering.OpenGL.GL.Texturing     as GL --
import Graphics.GLUtil                            as GL

import Leibniz.Constants (π)

import Pixels.Types



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (?)
setTexture :: GL.Program -> GL.TextureObject -> IO ()
setTexture program texture = do
  -- TODO: Why do we need this, can't we just set the texture uniform (?)
  GL.currentProgram $= Just program       -- 
  GL.activeTexture  $= (GL.TextureUnit 0) -- 
  GL.textureBinding GL.Texture2D $= Just texture -- Is this needed (?)§
  
  -- TODO: We shouldn't hard-code the wrapping and filtering operations
  texture2DWrap $= (Repeated, ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)

-- Creating and manipulating textures ----------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (eg. refer to Repa) (?)
-- textureFromImage :: (GL.GLsizei, GL.GLsizei) -> VS.Vector Word8 -> IO GL.TextureObject
textureFromImage :: Image (Pixel Word8) -> IO GL.TextureObject
textureFromImage im = do
  let [dx, dy] = map fromIntegral (R.listOfShape $ R.extent im)
  texId <- GL.genObjectName
  ptr   <- repaImagePointer im -- TODO: Not sure how efficient this is....
  GL.textureBinding GL.Texture2D $= Just texId

  GL.texImage2D
    (GL.Texture2D)                             -- Target (TODO: Uhm what)
    (GL.NoProxy)                               -- Proxy  (TODO: What's a proxy?)
    (0)                                        -- Level  (TODO: Level of what, mipmap?)
    (GL.RGBA8)                                 -- Internal pixel format
    (GL.TextureSize2D dx dy)                   -- Size (in pixels, I believe)
    (0)                                        -- Border
    (GL.PixelData GL.RGBA GL.UnsignedByte ptr) -- Pixel data
  return texId


-- |
-- TODO: Does this create a new buffer (?)
repaImagePointer :: Image (Pixel Word8) -> IO (Ptr a)
repaImagePointer v = VS.unsafeWith (V.convert $ R.toUnboxed v) (return . castPtr)


-- |
-- TODO: Rename
createRepaTexture :: V2 Int -> (V2 Int -> Pixel Word8) -> IO GL.TextureObject
createRepaTexture size' f = textureFromImage $ createRepaImage size' f


-- |
createRepaImage :: V2 Int -> (V2 Int -> Pixel Word8) -> Image (Pixel Word8)
createRepaImage (V2 dx dy) f = R.fromListUnboxed (R.Z :. dx :. dy) [ f (V2 x y) | y <- [0..dx-1], x <- [0..dx-1] ]


-- |
-- TODO: Handle errors (?)
-- TODO: Use repa dimensions for size and origin (?)
-- TODO: Decide size based on repa image (?)
modifyTexture :: V2 GLint -> Image (Pixel Word8) -> GL.TextureObject -> IO () -- ()
modifyTexture (V2 left top) im tex = do
  let (R.Z :. dx' :. dy') = R.extent im
      (V2 dx dy)          = fromIntegral <$> V2 dx' dy'
  ptr <- repaImagePointer im
  texSubImage2D GL.Texture2D 0 (TexturePosition2D left top) (TextureSize2D dx dy) (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

------------------------------------------------------------------------------------------------------------------------------------------------------