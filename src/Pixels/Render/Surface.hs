-- |
-- Module      : Pixels.Render.Surface
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : 
-- Portability : 
-- 

-- TODO | - Should functions operate on `Surface`s (maybe add wrappers and export them)
--        - Add `Pixels.Render.Surface.Internal`
--        - How to deal with errors (use EitherT throughout the project?)

-- SPEC | -
--        -

-- GHC Pragmas -----------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Render.Surface where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

-- *
import Data.Maybe (listToMaybe)
import Data.Foldable (toList)
import Control.Monad (join)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

-- *
import qualified Data.Vector.Unboxed  as VU
import qualified Data.Vector.Storable as VS

-- *
import Graphics.GPipe

import qualified Codec.Picture.Types as Juicy
import qualified Codec.Picture       as Juicy

-- * Internal
import Pixels.Types
import Pixels.Lenses
import Pixels.Algebra
import Pixels.Trinkets
import Pixels.Render.Core as Core

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - More ways of initialising (?)
new :: V2 Int -> Pixel -> AppT os (Surface os)
new sz fill = do
  tex <- monochromeTexture sz fill -- TODO: We could use an empty texture and set `fDirty` to True...
  vs  <- newQuadXY (fmap fromIntegral sz) texcoord
  return $ Surface {
    fSize     = sz,
    fTexture  = tex,
    fVertices = vs,
    fPixels   = CPUTexture (VU.replicate (foldr (*) 1 sz) fill) sz,
    fDirty    = False
  }


-- GPU Rendering ---------------------------------------------------------------------------------------------------------------------------

-- Textures --------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Move to separate module
--        - Don't hard-code pixel type

-- * Creating textures


-- |
-- TODO: Rename
newTexture :: V2 Int -> (V2 Int -> Pixel) -> AppT os (Texture2D os (Format RGBAFloat))
newTexture size@(V2 dx dy) f = fromPixels size [f (V2 x y) | x <- [0..dx-1], y <- [0..dy-1]]


-- | Creates a texture from a 'Foldable' of pixels
-- TODO | - Make sure the size is correct
--        - Figure out what the layout is (eg. col-major, row-major, etc.)
--        - Do we really need multiple mipmap levels
fromPixels :: Foldable t => V2 Int -> t Pixel -> AppT os (Texture2D os (Format RGBAFloat))
fromPixels size pixels = do
  -- TODO: What the hell is 'maxBound' doing here (?)
  -- JPG converts to SRGB
  tex <- newTexture2D SRGB8A8 size maxBound
  writeTexture2D tex 0 (V2 0 0) size (toList pixels)
  generateTexture2DMipmap tex
  return tex


-- | Creates a monochrome texture
monochromeTexture :: V2 Int -> Pixel -> AppT os (Texture2D os (Format RGBAFloat))
monochromeTexture size@(V2 dx dy) colour = fromPixels size (replicate (dx*dy) colour)


-- |
-- TODO | - Clean this up
--        -
loadTexture :: FilePath -> AppT os (Either String (Texture2D os (Format RGBAFloat)))
loadTexture fn = runEitherT $ do
  dynIm <- EitherT (liftIO $ Juicy.readImage fn)
  case dynIm of
    Juicy.ImageRGBA8 im -> lift $ fromPixels (size im) (Juicy.pixelFold (pixel) [] im) -- (Juicy.pixelMap _ im)
    _                   -> left "Can't decode image"
  where
    size im = V2 (Juicy.imageWidth im) (Juicy.imageHeight im)


-- * Transforming textures

-- |
-- TODO | - Make sure this is correct
--        - Generalise
pixel :: Juicy.ColorSpaceConvertible c Juicy.PixelRGBA8 => [Pixel] -> a -> b -> c -> [Pixel]
pixel xs _ _ pix = let Juicy.PixelRGBA8 r g b a = Juicy.convertPixel pix in xs ++ [V4 r g b a] -- V3 r g b : xs


-- * Reading textures

-- |
-- TODO | - Figure out order and format
--        - Make polymorphic
--        - Refactor
readGPUPixels :: V2 Int -> V2 Int -> Texture2D os (Format RGBAFloat) -> AppT os (Maybe [V4 Float])
readGPUPixels from size tex = sequence $  (\_ -> readTexture2D tex level from size (\ps c -> return (c:ps)) []) <$> guardTextureSection from size tex
  where
    level = 0
  -- -- maybe
  --                            (return Nothing)
  --                            (const $ fmap Just $ readTexture2D tex level from size (\ps c -> return (c:ps)) [])
  --                            (guardTextureSection from size tex)
  -- where
  --   level = 0


-- |
-- TODO: Rename (?)
readGPUPixel :: V2 Int -> Texture2D os (Format RGBAFloat) -> AppT os (Maybe (V4 Float))
readGPUPixel from tex = (join . fmap listToMaybe) <$> readGPUPixels from (V2 1 1) tex


-- * Texture validation

-- |
-- TODO | - Allow any level
--        - Refactor
guardTexturePoint :: V2 Int -> Texture2D os (Format RGBAFloat) -> Maybe (V2 Int)
guardTexturePoint p tex = listToMaybe (texture2DSizes tex) >>= \sz -> guardPoint p sz id


-- |
guardTextureSection :: V2 Int -> V2 Int -> Texture2D os (Format RGBAFloat) -> Maybe (V2 Int, V2 Int)
guardTextureSection from size tex = (,) <$> guardTexturePoint from tex <*> (guardTexturePoint (from+size) tex *> pure size)


-- |
writeGPUPixel :: V2 Int -> Pixel -> Texture2D os (Format RGBAFloat) -> AppT os (Maybe ())
writeGPUPixel p pixel tex = sequence $ (\_ -> writeTexture2D tex 0 p (V2 1 1) [pixel]) <$> guardTexturePoint p tex

-- * Texture IO

-- |
-- TODO | - Image becomes mirrored 
--        - Generalise
--        - Make sure this is correct
-- saves :: FilePath -> Texture2D os (HostFormat (BufferColor (Color a (ColorElement a)) a)) -> (a -> V3 Float) -> AppT os ()
saveGPUTexture :: FilePath -> Texture2D os (Format RGBAFloat) -> (V4 Float -> V4 Float) -> AppT os ()
saveGPUTexture fn tex f = do
  pixels <- readTexture2D tex 0 (V2 0 0) size (\ps c -> return $ convert (f c) ++ ps) []
  liftIO $ Juicy.savePngImage fn (Juicy.ImageRGB8 $ image size pixels)
  where
    (size:_) = texture2DSizes tex
    image (V2 dx dy) pixels = Juicy.Image { Juicy.imageWidth = dx, Juicy.imageHeight = dy, Juicy.imageData = VS.fromList pixels }
    pixel8    = floor . (*0xFF)
    convert c = let (V4 r g b a) = pixel8 <$> c in [r, g, b, a]


-- | R.Array R.U R.DIM2 pixel
-- createRepaImage :: Unbox a => V2 Int -> (V2 Int -> a) -> R.Array R.U R.DIM2 (a)
-- createRepaImage (V2 dx dy) f = R.fromListUnboxed (R.Z :. dx :. dy) [ f (V2 x y) | x <- [0..dx-1], y <- [0..dy-1] ]

-- CPU Rendering ---------------------------------------------------------------------------------------------------------------------------

-- * Rendering to a 'texture' on the CPU

-- |
-- TODO | - Make sure this is correct (row-major, col-major, yada yada)
--        - Bounds checking
writeCPUPixel :: V2 Int -> Pixel -> CPUTexture -> CPUTexture
writeCPUPixel p c (CPUTexture tex sz) = CPUTexture (tex VU.// [(toLinearIndex p sz, c)]) sz


-- |
-- TODO | - Make sure this is correct (row-major, col-major, yada yada)
--        - Bounds checking
--        - Refactor
readCPUPixel :: V2 Int -> CPUTexture -> Maybe Pixel
readCPUPixel p (CPUTexture tex sz)
  | fits p sz = Just $ tex VU.! toLinearIndex p sz
  | otherwise = Nothing


-- |
-- TODO | - Bounds checking
--        - Make sure this is correct (row-major, col-major, yada yada)
writeCPUTextureToGPU :: CPUTexture -> Texture2D os (Format RGBAFloat) -> AppT os ()
writeCPUTextureToGPU (CPUTexture cpu sz) gpu = writeTexture2D gpu 0 (V2 0 0) (sz) (VU.toList cpu)