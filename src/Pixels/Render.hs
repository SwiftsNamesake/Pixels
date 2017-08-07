-- |
-- Module      : Pixels.Render
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : 
-- Portability : 
--

-- TODO | - Logging (ornate, sophisticated, colours, LOG, ERROR, etc.)
--        - Robust path handling
--        - Consistent and elegant error handling (cf. EitherT)
--        - Use something other than lists (eg. vectors) (?)

-- SPEC | -
--        -

-- GHC Pragmas -----------------------------------------------------------------------------------------------------------------------------

--g{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Render where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

-- import           Data.Bits
import           Data.Word
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map         as M
import qualified Data.Vector.Storable as VS
import           Data.Foldable (toList)

import qualified Data.Array.Repa as R
import           Data.Array.Repa ((:.)(..))
import           Data.Array.Repa.Repr.Unboxed (Unbox(..))

import qualified Codec.Picture       as Juicy
import qualified Codec.Picture.Types as Juicy

import Text.Printf

import Linear (M44, (!*!), (!*), translation, identity, perspective, ortho, V2(..), V3(..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Lens hiding (argument)
import Control.Monad
-- import           Control.Applicative ((<$>), (<*>), liftA2)

-- import Graphics.Rendering.FTGL as FTGL
-- import Graphics.Rendering.TrueType.STB as STB
import           Graphics.GPipe hiding (texture, render)
import qualified Graphics.GPipe as GPipe
import qualified Graphics.GPipe.Context.GLFW as Context
-- import           Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow (..))

import Geometry.Sculptor.Shapes hiding (Face)

import Leibniz.Constants (π)
import Cartesian.Core (x,y,z)

import Pixels.Types
import Pixels.Lenses
import Pixels.Trinkets

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
render :: App os -> AppT os () -- ContextT ctx os m () -- _
render app = GPipe.render $ do
  clearWindowColor (app^.window) (pure 0.74)
  clearWindowDepth (app^.window) 1.0
  vertexArray <- newVertexArray (app^.easel.canvas.vertices)
  pointArray  <- newVertexArray (app^.easel.brush.positionBuffer)
  (app^.shader) $ ShaderEnvironment {
                    fRasterOptions  = app^.rasterOptions,
                    fUniforms       = app^.uniforms,
                    fAttributes     = AttributeData {
                                        fCanvas = toPrimitiveArray TriangleList vertexArray,
                                        fPoints = toPrimitiveArray PointList    pointArray },
                    fTexture        = TextureEnvironment {
                                        fFilterMode = SamplerNearest, -- TODO: Snap to nearest instead
                                        fEdgeMode   = pure ClampToEdge, --, pure 1),
                                        fTexture    = app^.easel.canvas.texture }
                  }

-- Geometry --------------------------------------------------------------------------------------------------------------------------------

-- |
newAttributeBuffer :: BufferFormat b => [HostFormat b] -> AppT os (Buffer os b)
newAttributeBuffer d = do
  buffer <- newBuffer (length d)
  writeBuffer buffer 0 d
  return buffer


-- |
newQuadXY :: V2 Float -> (V3 Float -> V2 Float) -> AppT os (Buffer os VertexAttributes)
newQuadXY (V2 dx dy) texcoord = newAttributeBuffer vertices
  where
    makeVertex v = (to4D 1 v, texcoord v)
    vertices = map makeVertex (concat . triangles $ planeXY V3 dx dy)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
newBrush :: V2 Int -> V3 Juicy.Pixel8 -> AppT os (Brush os)
newBrush (V2 x y) fill = do
  bp  <- newAttributeBuffer [(fromIntegral <$> V4 x y 0 1, fromIntegral <$> fill)]
  return $ Brush { fPositionBuffer = bp, fColour = fill }


-- |
newCanvas :: V2 Int -> V3 Juicy.Pixel8 -> AppT os (Canvas os)
newCanvas size fill = do
  tex <- new size (const fill)
  vs  <- newQuadXY (fmap fromIntegral size) texcoord
  return $ Canvas { fSize = size, fTexture = tex, fVertices = vs, fColour = fill }

-- Uniforms --------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Wrap in maybe (?)

readMatrix :: Int -> Shader os (ShaderEnvironment os) (V4 (UniformFormat (V4 (B Float)) x)) -- (M44 (B Float))
readMatrix i = getUniform $ \sh -> (sh^.uniforms.matrices.buffer, i)

readScalar :: Int -> Shader os (ShaderEnvironment os) (S x Float) -- (B Float)
readScalar i = getUniform $ \sh -> (sh^.uniforms.scalars.buffer, i)

readVector :: Int -> Shader os (ShaderEnvironment os) (V3 (S x Float)) -- (B3 Float)
readVector i = getUniform $ \sh -> (sh^.uniforms.vectors.buffer, i)


-- |
newUniformBuffer :: BufferFormat a => [HostFormat a] -> AppT os (UniformBlock os (HostFormat a) a)
newUniformBuffer vs = do
  b <- newBuffer (length vs)
  writeBuffer b 0 vs
  return $ UniformBlock { fBuffer = b, fSize = (length vs), fValues = vs }


-- |
-- TODO | - Make this less frail
--        - Move (?)
newUniforms :: AppConfig -> AppT os (UniformData os)
newUniforms config = UniformData
                       <$> newUniformBuffer [pj, mv, identity] -- 3 (identity)
                       <*> newUniformBuffer [0,0,0] -- 3 (0)
                       <*> newUniformBuffer [pure 0,   pure 0,   pure 0] -- 3 (pure 0)
  where
    pj = (let (V2 x' y') = (*0.5) . fromIntegral <$> (config^.canvasSize) in ortho (-x') (x') (-y') (y') 0 1)
    mv = (identity & translation.z .~ 0)

  -- uniforms.projection          .= (let (V2 x' y') = (*0.5) . fromIntegral <$> (app^.easel.canvas.size) in ortho (-x') (x') (-y') (y') 0 1)
  -- uniforms.modelview           .= (identity & translation.z .~ 0)

  -- return $  {
  --   fMatrices = matrices',
  --   fScalars  = scalars',
  --   fVectors  = vectors'
  -- }

-- Shaders ---------------------------------------------------------------------------------------------------------------------------------

-- |
texturedShader :: Shader os (ShaderEnvironment os) (FragmentStream (ColorSample F RGBFloat))
texturedShader = do
  -- Read the uniforms
  [pv, mv] <- mapM readMatrix [0,1]

  -- One day, in the distant future, I will come to know what a primitive stream is
  primitiveStream <- toPrimitiveStream (^.attributes.canvas)
  fragmentStream <- rasterize (^.rasterOptions) ((_1 %~ \pos -> pv !*! mv !* pos) <$> primitiveStream)
  samp <- newSampler2D (\env -> (env^.texture.texture, env^.texture.filterMode, (pure ClampToEdge, 0)))
  let sampleTexture = sample2D samp SampleAuto Nothing Nothing
  return $ sampleTexture <$> fragmentStream


-- |
colorShader :: Shader os (ShaderEnvironment os) (FragmentStream (ColorSample F RGBFloat))
colorShader = do
  [pv, mv] <- mapM readMatrix [0,1]

  -- One day, in the distant future, I will come to know what a primitive stream is
  primitiveStream <- toPrimitiveStream (^.attributes.points)
  rasterize (^.rasterOptions) ((_1 %~ \pos -> pv !*! mv !* pos) <$> primitiveStream)

-- Textures --------------------------------------------------------------------------------------------------------------------------------

-- TODO: Don't hard-code pixel type

-- |
pixel :: Juicy.ColorSpaceConvertible c Juicy.PixelRGB8 => [V3 Juicy.Pixel8] -> a -> b -> c -> [V3 Juicy.Pixel8]
pixel xs _ _ pix = let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs


-- |
-- TODO | - Image becomes mirrored 
--        -
-- saves :: FilePath -> Texture2D os (HostFormat (BufferColor (Color a (ColorElement a)) a)) -> (a -> V3 Float) -> AppT os ()
save :: FilePath -> Texture2D os (Format RGBFloat) -> (V3 Float -> V3 Float) -> AppT os ()
save fn tex f = do
  pixels <- readTexture2D tex 0 (V2 0 0) size (\ps c -> return $ ps ++ convert (f c)) []
  liftIO $ Juicy.savePngImage fn (Juicy.ImageRGB8 $ image size pixels)
  where
    (size:_) = texture2DSizes tex
    image (V2 dx dy) pixels = Juicy.Image { Juicy.imageWidth = dx, Juicy.imageHeight = dy, Juicy.imageData = VS.fromList pixels }
    pixel8    = floor . (*255)
    convert c = let (V3 r g b) = pixel8 <$> c in [r, g, b]


-- |
-- TODO: Clean this up
load :: FilePath -> AppT os (Either String (Texture2D os (Format RGBFloat)))
load fn = runEitherT $ do
  (Juicy.ImageRGB8 image) <- EitherT (liftIO $ Juicy.readImage fn)
  let size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)
  tex <- lift $ fromPixels size (Juicy.pixelFold pixel [] image)
  return tex


-- |
new :: V2 Int -> (V2 Int -> V3 Juicy.Pixel8) -> AppT os (Texture2D os (Format RGBFloat))
new size@(V2 dx dy) f = fromPixels size [f (V2 x y) | x <- [0..dx-1], y <- [0..dy-1]]


-- |
-- TODO: Rename (?)
pixelAt :: V2 Int -> Texture2D os (Format RGBFloat) -> AppT os (Maybe (V3 Float))
pixelAt from tex = do
  mpixels <- readPixels from (V2 1 1) tex
  return $ mpixels >>= listToMaybe


-- |
guardTexturePoint :: V2 Int -> Texture2D os (Format RGBFloat) -> Maybe (V2 Int)
guardTexturePoint p tex = do
  wholesize <- listToMaybe $ texture2DSizes tex -- TODO: Allow any level
  if fits p wholesize
    then Just p
    else Nothing
  where
    fits (V2 x y) (V2 dx dy) = (0 <= x && x < dx) && (0 <= y && y < dy)


-- |
guardTextureSection :: V2 Int -> V2 Int -> Texture2D os (Format RGBFloat) -> Maybe (V2 Int, V2 Int)
guardTextureSection from size tex = (,) <$> guardTexturePoint from tex <*> fmap (const size) (guardTexturePoint (from+size) tex)


-- |
-- TODO | - Figure out order and format
--        - Make polymorphic
--        - Refactor
readPixels :: V2 Int -> V2 Int -> Texture2D os (Format RGBFloat) -> AppT os (Maybe [V3 Float])
readPixels from size tex = maybe
                             (return Nothing)
                             (const $ fmap Just $ readTexture2D tex level from size (\ps c -> return (c:ps)) [])
                             (guardTextureSection from size tex)
  where
    level = 0


-- | Creates a texture from a 'Foldable' of pixels
-- TODO: Make sure the size is correct
-- TODO: Figure out what the layout is (eg. col-major, row-major, etc.)
fromPixels :: Foldable t => V2 Int -> t (V3 Juicy.Pixel8) -> AppT os (Texture2D os (Format RGBFloat))
fromPixels size pixels = do
  -- TODO: What the hell is 'maxBound' doing here (?)
  tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
  writeTexture2D tex 0 (V2 0 0) size (toList pixels)
  generateTexture2DMipmap tex
  return tex


-- |
-- TODO | - How to deal with errors
writePixel :: V2 Int -> V3 Juicy.Pixel8 -> Texture2D os (Format RGBFloat) -> AppT os (Either String ())
writePixel p pixel tex = maybe
                           (return $ Left "writePixel: Coordinate out of range")
                           (const $ fmap Right $ writeTexture2D tex 0 p (V2 1 1) [pixel])
                           (guardTexturePoint p tex)


-- | Creates a monochrome texture
monochrome :: V2 Int -> V3 Juicy.Pixel8 -> AppT os (Texture2D os (Format RGBFloat))
monochrome size@(V2 dx dy) colour = fromPixels size (replicate (dx*dy) colour)


-- | R.Array R.U R.DIM2 pixel
createRepaImage :: Unbox a => V2 Int -> (V2 Int -> a) -> R.Array R.U R.DIM2 (a)
createRepaImage (V2 dx dy) f = R.fromListUnboxed (R.Z :. dx :. dy) [ f (V2 x y) | x <- [0..dx-1], y <- [0..dy-1] ]