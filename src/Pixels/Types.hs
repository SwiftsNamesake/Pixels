
--
-- Pixels.Types
-- Types...
--
-- Jonatan H Sundqvist
-- August 20 2016
--

-- TODO | -
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

import           Data.Array.Repa ((:.)(..)) -- Weirdest syntax ever
import qualified Data.Array.Repa as R

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix (M44)

import Control.Lens

import Control.Concurrent.MVar

import qualified Graphics.Rendering.OpenGL                  as GL
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders       as GL --

import qualified Graphics.UI.GLFW                           as GLFW

-- import qualified Graphics.UI.Awesomium         as Aw
-- import qualified Graphics.UI.Awesomium.WebCore as Aw

-- import           Cartesian.Plane.Types



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- type Mesh  = (GL.PrimitiveMode, GL.BufferObject, GL.BufferObject, Int, Maybe [GL.TextureObject])
data Mesh = Mesh { _primitive        :: GL.PrimitiveMode,
                   _attributeBuffers :: M.Map String (GL.BufferObject, Int),
                   _numVertices      :: Int,
                   _oTextures        :: [GL.TextureObject] }


type Meshes = M.Map String Mesh
type World  = () -- TODO: Remove this (?)
type Images = M.Map String (Image Word8)


-- | 
-- TODO: Should I use an unpacked representation instead (and use bitmasking)?
type Pixel w = (w, w, w, w)
type Image w = R.Array R.U R.DIM2 (Pixel w)
-- TODO: What's the difference between a storable array and an unpacked array?
--       Is it possible to convert between the two and if so, how expensive 
--       is the conversion?
-- type Image w = R.Array R.U R.DIM2 (Pixel w)

------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------

-- data LoggerT

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
data AppState = AppState { _alive    :: Bool,
                           _window   :: GLFW.Window,
                           _debug    :: Debug,
                           _input    :: Input Float,
                           _paths    :: Paths,
                           _world    :: World,
                           _settings :: Settings,
                           _ui       :: UI,
                           _size     :: V2 Float,
                           _graphics :: Graphics }


-- | Serialisable session (resume work when app starts)
-- data Session


-- |
    -- let home = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"
    -- let shaderpath = home </> "assets/shaders"
data Paths = Paths { _home     :: FilePath,
                     _assets   :: FilePath,
                     _textures :: FilePath }
             deriving (Show, Eq)


-- |
data LogLevel = InfoLevel | WarningLevel | CriticalLevel  deriving (Enum, Ord, Eq, Bounded, Read, Show)
data Debug = Debug { _logLevel :: LogLevel, _startTime :: Double } deriving (Show, Eq, Read)


-- |
data Input f = Input { _mouse :: Mouse f, _keyboard :: S.Set GLFW.Key, _command :: MVar String }


-- |
data Mouse f = Mouse { _path :: [V2 f], _buttons :: S.Set GLFW.MouseButton }


-- |
data Graphics = Graphics { _program       :: GL.Program,
                           _camera        :: Camera Float,
                           _meshes        :: Meshes,
                           _resources     :: CPUResources,
                           _clearColour   :: GL.Color4 Float,
                           _matModelview  :: M44 Float,
                           _matProjection :: M44 Float} --, _viewport }


-- | Graphics resources that are stored in CPU memory
data CPUResources = CPUResources { _images :: Images }


-- |
data UI = UI {}
-- data UI = UI { _view :: Aw.WebView }

-- |
data Settings = Settings {}


-- |
data Camera f = Camera { _pan :: V3 Float, _rotation :: V3 Float }


-- |
------------------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Use Cartesian instead...

class HasX a f | a -> f where { x :: Lens a a f f }
class HasY a f | a -> f where { y :: Lens a a f f }
class HasZ a f | a -> f where { z :: Lens a a f f }

instance HasX (V3 f) f where x = lens (\(V3 x' _ _) -> x') (\(V3 _ y' z') x' -> V3 x' y' z')
instance HasY (V3 f) f where y = lens (\(V3 _ y' _) -> y') (\(V3 x' _ z') y' -> V3 x' y' z')
instance HasZ (V3 f) f where z = lens (\(V3 _ _ z') -> z') (\(V3 x' y' _) z' -> V3 x' y' z')

instance HasX (V2 f) f where x = lens (\(V2 x' _) -> x') (\(V2 _ y') x' -> V2 x' y')
instance HasY (V2 f) f where y = lens (\(V2 _ y') -> y') (\(V2 x' _) y' -> V2 x' y')

------------------------------------------------------------------------------------------------------------------------------------------------------
