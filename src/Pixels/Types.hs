
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

import Linear.V2

import Control.Concurrent.MVar

import qualified Graphics.Rendering.OpenGL                  as GL
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders       as GL --

import qualified Graphics.UI.GLFW                           as GLFW

-- import qualified Graphics.UI.Awesomium         as Aw
-- import qualified Graphics.UI.Awesomium.WebCore as Aw

import           Cartesian.Plane.Types



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
type World  = [V2 Double]

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
data Debug = Debug { _logLevel :: LogLevel } deriving (Show, Eq, Read)


-- |
data Input f = Input { _mouse :: Mouse f, _keyboard :: S.Set GLFW.Key, _command :: MVar String }


-- |
data Mouse f = Mouse { _position :: V2 f, _buttons :: S.Set GLFW.MouseButton }


-- |
data Graphics = Graphics { _program :: GL.Program, _camera :: Camera Float, _meshes :: Meshes, _clearColour :: GL.Color4 Float } --, _viewport }

-- |
data UI = UI {}
-- data UI = UI { _view :: Aw.WebView }

-- |
data Settings = Settings {}


-- |
data Camera f = Camera {}