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
{-# LANGUAGE RankNTypes             #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Render where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

-- import           Data.Bits
import           Data.Word
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map             as M
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import           Data.Foldable (toList)

-- import qualified Data.Array.Repa as R
-- import           Data.Array.Repa ((:.)(..))
-- import           Data.Array.Repa.Repr.Unboxed (Unbox(..))

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
import Pixels.Algebra
import Pixels.Lenses
import Pixels.Trinkets
import Pixels.Render.Core    as Core
import Pixels.Render.Surface as Surface

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - This needs to be more flexible
--        - No hard-coded values
render :: App os -> AppT os () -- ContextT ctx os m () -- _
render app = GPipe.render $ do
  clearWindowColor (app^.window) (pure 0.74)
  clearWindowDepth (app^.window) 1.0
  vertexArray <- newVertexArray (app^.easel.canvas.surface.vertices)
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
                                        fTexture    = app^.easel.canvas.surface.texture }
                  }

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
newBrush :: V2 Int -> Pixel -> AppT os (Brush os)
newBrush (V2 x y) fill = do
  bp <- Core.newAttributeBuffer [(fromIntegral <$> V4 x y 0 1, fromIntegral <$> fill)]
  return $ Brush { fPositionBuffer = bp, fColour = fill }


-- |
-- TODO | - Move (?)
newCanvas :: V2 Int -> Pixel -> AppT os (Canvas os)
newCanvas sz fill = do
  s <- Surface.new sz fill
  return $ Canvas {
             fSurface = s,
             fColour  = fill }

-- Uniforms --------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Wrap in maybe (?)
--        - Simplify signatures (?)

 -- (s -> (Buffer os (Uniform b), Int)) -> Shader os s 

-- readUniform :: forall os a b x. UniformInput b => Lens' (UniformData os) (UniformBlock os a b) -> Int -> Shader os (ShaderEnvironment os) (UniformFormat b x)
-- readUniform l i = getUniform $ \sh -> (sh^.uniforms.l.buffer :: (Buffer os (Uniform b)), i)

readMatrix :: Int -> Shader os (ShaderEnvironment os) (V4 (UniformFormat (V4 (B Float)) x)) -- (M44 (B Float))
readMatrix i = getUniform $ \sh -> (sh^.uniforms.matrices.buffer, i) -- readUniform matrices

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
--        - Move (eg. to Surface) (?)
newUniforms :: AppConfig -> AppT os (UniformData os)
newUniforms config = UniformData
                       <$> newUniformBuffer [pj, mv, identity] -- 3 (identity)
                       <*> newUniformBuffer [0,0,0] -- 3 (0)
                       <*> newUniformBuffer [pure 0, pure 0, pure 0] -- 3 (pure 0)
  where
    pj = let (V2 x' y') = (*0.5) . fromIntegral <$> (config^.canvasSize) in ortho (-x') (x') (-y') (y') 0 1
    mv = identity & translation.z .~ 0

  -- uniforms.projection          .= (let (V2 x' y') = (*0.5) . fromIntegral <$> (app^.easel.canvas.size) in ortho (-x') (x') (-y') (y') 0 1)
  -- uniforms.modelview           .= (identity & translation.z .~ 0)

  -- return $  {
  --   fMatrices = matrices',
  --   fScalars  = scalars',
  --   fVectors  = vectors'
  -- }

-- Shaders ---------------------------------------------------------------------------------------------------------------------------------

-- |
texturedShader :: Shader os (ShaderEnvironment os) (FragmentStream (ColorSample F RGBAFloat))
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
colorShader :: Shader os (ShaderEnvironment os) (FragmentStream (ColorSample F RGBAFloat))
colorShader = do
  [pv, mv] <- mapM readMatrix [0,1]

  -- One day, in the distant future, I will come to know what a primitive stream is
  primitiveStream <- toPrimitiveStream (^.attributes.points)
  rasterize (^.rasterOptions) ((_1 %~ \pos -> pv !*! mv !* pos) <$> primitiveStream)