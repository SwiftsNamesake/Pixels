
--
-- Pixels.Types
-- Types...
--
-- Jonatan H Sundqvist
-- August 20 2016
--

-- TODO | - Break up type definitions into separate modules (?)
--        - Logging
--        - Simplify types (via gpipe's own type machinery)

-- SPEC | -
--        -



---GHC Pragams ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
--{-# LANGUAGE FunctionalDependencies #-}

--- API ----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Types where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import Data.Set (Set)
import           Data.Word
-- import           Data.Colour
-- import qualified Data.Array.Repa as R

import Linear (V2(..), V3(..), M44)
import Control.Concurrent.MVar

import qualified Codec.Picture.Types as Juicy

import Graphics.GPipe.Context.GLFW       (Handle)
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW.Input (Key(..), MouseButton(..))

--- Types --------------------------------------------------------------------------------------------------------------------------------------------

type AppContext = (WindowFormat RGBFloat Depth)
-- type AppT os a  = ContextT (Window os RGBFloat Depth) os IO a
type AppT os a  = ContextT Handle os IO a
-- AppVertexFormat

type VertexAttributes = (B4 Float, B2 Float)

type UniformBlockMatrix os = UniformBlock os (M44 Float) (M44 (B Float))

------------------------------------------------------------------------------------------------------------------------------------------------------

data AppConfig = AppConfig {
 
}

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
data App os = App {
  fWindow :: Window os RGBFloat Depth,
  fEasel  :: Easel os, -- Canvas os,
  fShader :: CompiledShader os (ShaderEnvironment os), -- (WindowFormat RGBFloat Depth),
  fInput  :: Input,
  fUniforms :: UniformData os,
  fRasterOptions :: (Side, ViewPort, DepthRange)
}


-- |
data Input = Input {
  fMouse    :: Mouse,
  fKeyboard :: Set Key
} deriving (Show)


-- |
data Mouse = Mouse {
  fCursor  :: V2 Double,
  fButtons :: Set MouseButton
} deriving (Show)


-- |
data Easel os = Easel {
  fBrush  :: V3 Juicy.Pixel8,
  fCanvas :: Canvas os
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
  fEdgeMode   :: EdgeMode2
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

