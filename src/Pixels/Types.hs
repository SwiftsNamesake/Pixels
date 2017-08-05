
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
--        - Organise data better (eg. separate logic from GPU resources)

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
import Control.Concurrent.STM

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
  fCanvasSize   :: V2 Int,
  fCanvasColour :: V3 Juicy.Pixel8,
  fWindowSize   :: V2 Int,
  fBrushColour  :: V3 Juicy.Pixel8,
  fEaselPalette :: [V3 Juicy.Pixel8]
}

------------------------------------------------------------------------------------------------------------------------------------------------------

-- | 
data App os = App {
  fWindow :: Window os RGBFloat Depth,
  fEasel  :: Easel os, -- Canvas os,
  fShader :: CompiledShader os (ShaderEnvironment os), -- (WindowFormat RGBFloat Depth),
  fInput  :: Input,
  fUndoQueue :: [UndoAction],
  fUniforms  :: UniformData os,
  fRasterOptions :: (Side, ViewPort, DepthRange)
}


-- | 
data Input = Input {
  fMouse    :: Mouse,
  fKeyboard :: Set Key,
  fInputChannel :: InputChannel
} -- deriving (Show)


-- | Coming soon...
-- TODO | - Unify events (?)
-- newtype InputChannel = InputChannel (TChan AppEvent)
type InputChannel = (TChan AppEvent)


-- |
data AppEvent =   MouseMotion (V2 Double)
                | MouseDown   MouseButton
                | MouseUp     MouseButton
                | MouseScroll (V2 Double)
                | KeyDown     Key
                | KeyUp       Key
                | KeyRepeat   Key
                | FileDrop    [String]
                | FileChange  () -- TODO: Fix
                | WindowClose
                deriving (Eq, Show)


-- | 
data Mouse = Mouse {
  fCursor  :: V2 Float,
  fButtons :: Set MouseButton
} deriving (Show)


-- | 
data Easel os = Easel {
  fBrush   :: Brush os,
  fCanvas  :: Canvas os,
  fPalette :: [V3 Juicy.Pixel8]
}


-- | 
data Brush os = Brush {
  fColour         :: V3 Juicy.Pixel8,
  fPositionBuffer :: Buffer os (B4 Float, B3 Float)
}


-- | 
data Canvas os = Canvas {
  fSize     :: V2 Int,
  fColour   :: V3 Juicy.Pixel8,
  fTexture  :: Texture2D os (Format RGBFloat),
  fVertices :: Buffer os VertexAttributes -- The shape of the canvas itself
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
-- TODO | - How do we 'merge' the two versions of AttributeData?
data AttributeData os = AttributeData {
  fCanvas :: PrimitiveArray Triangles VertexAttributes,
  fPoints :: PrimitiveArray Points    (B4 Float, B3 Float)
}


-- | 
data ShaderEnvironment os = ShaderEnvironment {
  fRasterOptions  :: (Side, ViewPort, DepthRange),
  fUniforms       :: UniformData os,
  fAttributes     :: AttributeData os,
  fTexture        :: TextureEnvironment os
}

------------------------------------------------------------------------------------------------------------------------------------------------------

-- | 
-- data ActionStack = ActionStack deriving (Show, Eq)


-- | 
-- data UIOverlay = UIOverlay {}


data UndoAction = UndoAction deriving (Show, Eq)


-- | 
-- data Brush = Brush

