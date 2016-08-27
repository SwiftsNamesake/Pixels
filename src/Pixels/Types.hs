
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
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Types where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import qualified Data.Set as S
import qualified Data.Map as M

import Linear.V2

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
data Mesh = Mesh { _primitive :: GL.PrimitiveMode,
	                 _attributeBuffers :: M.Map String (GL.BufferObject, Int),
                   _numVertices :: Int,
                   _textures :: [GL.TextureObject] } 

type World = [V2 Double]

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
data AppState = AppState { _input    :: Input Float,
	                         _world    :: World,
                           _settings :: Settings,
                           _ui       :: UI,
                           _size     :: V2 Float,
                           _graphics :: Graphics }


-- |
data Input f = Input { _mouse :: Mouse f, _keyboard :: S.Set GLFW.Key }


-- |
data Mouse f = Mouse { _position :: V2 f, _buttons :: S.Set GLFW.MouseButton }


-- |
data Graphics = Graphics { _program :: GL.Program, _camera :: Camera Float } --, _viewport }

-- |
data UI = UI {}
-- data UI = UI { _view :: Aw.WebView }

-- |
data Settings = Settings {}


-- |
data Camera f = Camera {}