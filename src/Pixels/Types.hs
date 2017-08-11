
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

-- TODO | - Explicit exports
module Pixels.Types where -- (
  -- * App

  -- *
  -- CircleList, newCircleList, next, prev, current
-- ) where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import           Data.Vector (Vector, (!), fromList)
import qualified Data.Vector.Unboxed as VU
import           Data.Set (Set)
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

-- TODO | - Type synonyms for reused types (type family?)
--        -

type AppContext = WindowFormat RGBAFloat Depth
-- type AppT os a  = ContextT (Window os RGBAFloat Depth) os IO a
type AppT os a  = ContextT Handle os IO a
-- AppVertexFormat
type Pixel = V4 Word8

type VertexAttributes = (B4 Float, B2 Float)

-- type UniformBlockMatrix os = UniformBlock os (M44 (B Float)) -- (M44 Float) (M44 (B Float))

------------------------------------------------------------------------------------------------------------------------------------------------------

data AppConfig = AppConfig {
  fCanvasSize   :: V2 Int,
  fCanvasColour :: Pixel,
  fWindowSize   :: V2 Int,
  fBrushColour  :: Pixel,
  fEaselPalette :: [Pixel]
} deriving (Show)

------------------------------------------------------------------------------------------------------------------------------------------------------

-- | 
data App os = App {
  fWindow :: Window os RGBAFloat Depth,
  fEasel  :: Easel os, -- Canvas os,
  fShader :: CompiledShader os (ShaderEnvironment os), -- (WindowFormat RGBAFloat Depth),
  fInput  :: Input,
  -- fUndoQueue :: [UndoAction],
  fUniforms  :: UniformData os,
  fRasterOptions :: (Side, ViewPort, DepthRange)
}


-- | 
data Easel os = Easel {
  fBrush   :: Brush os,
  fCanvas  :: Canvas os,
  fPalette :: CircleList (Pixel)
}


-- | 
data Brush os = Brush {
  fColour         :: Pixel,
  fPositionBuffer :: Buffer os (B4 Float, B4 Float)
}


-- | 
-- TODO | - Make `Canvas` a special case of some kind of `Widget` type
--          that knows how to render itself, and keep the CPU/GPU data in sync (ongoing; cf. Surface)
data Canvas os = Canvas {
  fColour   :: Pixel,
  fSurface  :: Surface os
}


-- | 
data Input = Input {
  -- fBounds   :: ,
  fScroll   :: V2 Double,
  fSize     :: V2 Int,
  fMouse    :: Mouse,
  fKeyboard :: Set Key,
  fInputChannel :: InputChannel
} -- deriving (Show)


------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Gotta think carefully about the type of fPixels
--        -
data Surface os = Surface {
  fSize     :: V2 Int,
  fTexture  :: Texture2D os (Format RGBAFloat),
  fPixels   :: CPUTexture,
               -- VU.Vector Pixel,
               -- R.Array R.U R.DIM2 (Pixel), -- TODO: Rename (?)
  fVertices :: Buffer os VertexAttributes, -- The shape of the Surface itself
  -- TODO | - Dirty flag, or some smarter way of syncing texture/pixels (?)
  --        - Don't update more than necessary (eg. use a dirtyRect, BoundingBox (V2 Int))
  fDirty :: Bool
}


-- |
data CPUTexture = CPUTexture {
  fVector :: (VU.Vector Pixel),
  fSize :: (V2 Int)
}


-- Input ---------------------------------------------------------------------------------------------------------------------------------------------

-- | Coming soon...
-- TODO | - Unify events (?)
-- newtype InputChannel = InputChannel (TChan AppEvent)
type InputChannel = TChan AppEvent


-- |
-- TODO | - Complete (resize, minimise, maximise, enter, leave, Pending FileDrop, etc.)
--        - Elm style event mapping (eg. UIEvent -> AppAction) (?)
data AppEvent =   MouseMotion (V2 Double)
                | MouseDown   MouseButton
                | MouseUp     MouseButton
                | MouseScroll (V2 Double)
                | KeyDown     Key
                | KeyUp       Key
                | KeyRepeat   Key
                | FileDrop    [String]
                -- | FileChanged () -- TODO: Fix
                | WindowClosing
                deriving (Eq, Show)


-- | 
data Mouse = Mouse {
  fCursor  :: V2 Float,
  fButtons :: Set MouseButton
} deriving (Show)


-- |
-- data KeyBindings = KeyBindings {}

-- GPU Graphics --------------------------------------------------------------------------------------------------------------------------------------

-- | 
data TextureEnvironment os = TextureEnvironment {
  fTexture :: Texture2D os (Format RGBAFloat),
  fFilterMode :: SamplerFilter RGBAFloat,
  fEdgeMode   :: EdgeMode2
  --, BorderColor (Format RGBAFloat)),
}


-- | 
-- TODO | - Frail, fix
--        - Fix types
data UniformBlock os a b = UniformBlock {
  fBuffer :: Buffer os (Uniform b),
  fValues :: [a], -- [HostFormat b], -- makeLensesWith barfs at type families
  fSize   :: Int
}


-- | 
data UniformData os = UniformData {
  fMatrices :: UniformBlock os (M44 Float) (M44 (B Float)),
  fScalars  :: UniformBlock os (Float)     (B Float),
  fVectors  :: UniformBlock os (V3 Float)  (B3 Float)
}


-- | 
-- TODO | - How do we 'merge' the two versions of AttributeData?
data AttributeData os = AttributeData {
  fCanvas :: PrimitiveArray Triangles VertexAttributes,
  fPoints :: PrimitiveArray Points    (B4 Float, B4 Float)
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


-- data UndoAction = UndoAction deriving (Show, Eq)


-- | 
-- data Brush = Brush

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- newtype WindowCoords = WindowCoords (V2 f)
-- newtype ClientCoords = ClientCoords (V2 f)
-- newtype ScreenCoords = ScreenCoords (V2 f)
-- newtype CanvasCoords = CanvasCoords (V2 f) -- TODO: General type for 'entity-specific' coords

------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Opaque type with circular indexing behaviour
-- TODO | - Factor out
--        - Polymorphic (eg. any container, any key, IsList)
data CircleList a = CircleList (Vector a) Int Int


-- |
newCircleList :: [a] -> Maybe (CircleList a)
newCircleList xs
  | null xs   = Nothing -- n % 0 makes no sense
  | otherwise = Just $ CircleList (fromList xs) 0 (length xs)


-- |
-- TODO | - Rename (?)
stepBy :: Int -> CircleList a -> CircleList a
stepBy by (CircleList xs i len) = CircleList xs (mod (i+by) len) len

next :: CircleList a -> CircleList a
next = stepBy 1

prev :: CircleList a -> CircleList a
prev = stepBy (-1)

current :: CircleList a -> a
current (CircleList xs i _) = xs ! i