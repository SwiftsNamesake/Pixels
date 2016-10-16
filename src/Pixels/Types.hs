
--
-- Pixels.Types
-- Types...
--
-- Jonatan H Sundqvist
-- August 20 2016
--

-- TODO | - Break up type definitions into separate modules (?)
--        -

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
-- {-# LANGUAGE OverloadedRecordFields #-} -- Some day...



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Types where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Set as S
import qualified Data.Map as M -- TODO: Use strict version (?)
import           Data.Word
import           Data.Colour

-- import qualified Data.Array.Repa as R

import Linear (V2(..), V3(..), M44)

import Control.Lens

import Control.Concurrent.MVar

import Graphics.Michelangelo.Types (Image)

import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.GLFW as GLFW



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- type Mesh  = (GL.PrimitiveMode, GL.BufferObject, GL.BufferObject, Int, Maybe [GL.TextureObject])
data Mesh = Mesh { fPrimitive        :: GL.PrimitiveMode,
                   fAttributeBuffers :: M.Map String (GL.BufferObject, Int),
                   fNumVertices      :: Int,
                   fTextures         :: [GL.TextureObject] }


type Meshes = M.Map String Mesh
type World  = () -- TODO: Remove this (?)
type Images = M.Map String (Image Word8)

------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------

-- data LoggerT

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
data AppState = AppState {
  fAlive    :: Bool,
  fWindow   :: GLFW.Window,
  fDebug    :: Debug,
  fInput    :: Float,
  fPaths    :: Paths,
  fSession  :: Session,
  fSettings :: Settings,
  fUi       :: UI,
  fSize     :: V2 Float,
  fGraphics :: Graphics
}


-- |
    -- let home = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"
    -- let shaderpath = home </> "assets/shaders"
data Paths = Paths {
  fHome     :: FilePath,
  fAssets   :: FilePath,
  fTextures :: FilePath
} deriving (Show, Eq)


-- | Represents a single drawing surface
data Canvas = Canvas { fTexture :: GL.TextureObject, fBuffer :: Image Word8 }


-- |
-- TODO: Serialisable session (resume work when app starts)
data Session = Session {}


-- |
-- data Tool = Tool 
-- data Pencil = Pencil


-- |
data LogLevel = InfoLevel | WarningLevel | CriticalLevel deriving (Enum, Ord, Eq, Bounded, Read, Show)
data Debug = Debug { fLogLevel :: LogLevel, fStartTime :: Double } deriving (Show, Eq, Read)


-- |
data Input f = Input { fMouse :: Mouse f, fKeyboard :: S.Set GLFW.Key, fCommand :: MVar String }


-- |
data Mouse f = Mouse { fPath :: [V2 f], fButtons :: S.Set GLFW.MouseButton }


-- |
data Graphics = Graphics {
  fProgram       :: GL.Program,
  fCamera        :: Camera Float,
  fMeshes        :: Meshes,
  fResources     :: CPUResources,
  fClearColour   :: GL.Color4 Float,
  fMatModelview  :: M44 Float,
  fMatProjection :: M44 Float
} --, _viewport }


-- | Graphics resources that are stored in CPU memory
data CPUResources = CPUResources { fImages :: Images }


-- |
data UI = UI {}
-- data UI = UI { fView :: Aw.WebView }

-- |
data Settings = Settings {}


-- |
data Camera f = Camera { fPan :: V3 Float, fRotation :: V3 Float }
