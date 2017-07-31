
--
-- Pixels.Types
-- Types...
--
-- Jonatan H Sundqvist
-- August 20 2016
--

-- TODO | - Break up type definitions into separate modules (?)
--        - Logging

-- SPEC | -
--        -



---GHC Pragams ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

--- API ----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Types where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import qualified Data.Set as S
import qualified Data.Map as M -- TODO: Use strict version (?)
import           Data.Word
-- import           Data.Colour
-- import qualified Data.Array.Repa as R

import Linear (V2(..), V3(..), M44)
import Control.Concurrent.MVar

import Graphics.GPipe hiding (texture)

--- Types --------------------------------------------------------------------------------------------------------------------------------------------

type AppContext = (WindowFormat RGBFloat Depth)
type AppT os a  = ContextT (Window os RGBFloat Depth) os IO a
-- AppVertexFormat

type VertexAttributes = (B4 Float, B2 Float)

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
data App os = App {
  fCanvas :: Canvas os,
  fRasterOptions :: (Side, ViewPort, DepthRange),
  fShader :: CompiledShader os (WindowFormat RGBFloat Depth),
  fUniforms :: UniformData os
}


-- |
data Canvas os = Canvas {
  fSize     :: V2 Int,
  fTexture  :: Texture2D os (Format RGBFloat),
  fVertices :: Buffer os VertexAttributes
}


-- |
data TextureEnvironment os = TextureEnvironment {
  fTexture :: Texture2D os (Format RGBFloat),
  fFilterMode :: SamplerFilter RGBFloat,
  fEdgeMode   :: (EdgeMode2)
  --, BorderColor (Format RGBFloat)),
}


-- |
data UniformBlock os a b = UniformBlock {
  fBuffer :: Buffer os (Uniform b),
  fValues :: [a], -- [HostFormat a],
  fSize   :: Int
}


-- |
data UniformData os = UniformData {
  fMatrices :: UniformBlock os (M44 Float) (M44 (B Float)),
  fScalars  :: UniformBlock os (Float)     (B   (Float)),
  fVectors  :: UniformBlock os (V3 Float)  (B3  (Float))
}


-- |
data ShaderEnvironment os = ShaderEnvironment {
  fRasterOptions  :: (Side, ViewPort, DepthRange),
  fUniforms       :: UniformData os,
  fPrimitiveArray :: PrimitiveArray Triangles VertexAttributes,
  fTexture        :: TextureEnvironment os
}

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- data ActionStack = ActionStack


-- |
-- data Brush = Brush

