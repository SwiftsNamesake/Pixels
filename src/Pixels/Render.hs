--
-- Pixels.Render
-- Rendering...
--
-- Jonatan H Sundqvist
-- August 19 2016
--

-- TODO | -
--        -

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Render where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import System.IO (stdout, hFlush) --
import System.FilePath ((</>))    --
-- import System.Console.ANSI        --

import Linear.Projection
import Linear.Quaternion
import Linear.Matrix
import Linear.V3
import Linear.V4

import           Data.IORef
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Lens hiding (argument)
import Control.Monad

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL as GL hiding (projection, perspective, Line, position)
import Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import Graphics.Rendering.OpenGL.GL.Shaders as GL --

-- import Graphics.Rendering.OpenGL.Raw as GLRaw

import Foreign.Storable
import Foreign.Marshal as Marshal
import Foreign.Ptr     as Ptr

import Graphics.GLUtil.JuicyTextures (readTexture)
import Graphics.GLUtil as GLU

import           Control.Monad.Trans.Class as St
import qualified Control.Monad.Trans.State as St

import           Graphics.Michelangelo.Transformations
import qualified Graphics.Michelangelo.Shaders as Shaders
-- import qualified Graphics.Michelangelo.Shapes  as Shapes
import           Graphics.Michelangelo.Shapes (π)

import Cartesian.Space.Types
import Cartesian.Plane.Types
import Cartesian.Plane

import Pixels.Types
import Pixels.Interaction
import Pixels.Lenses



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
setupOpenGL :: IO ()
setupOpenGL = do
  clearColor $= GL.Color4 1.0 1.0 1.0 1.0
  blend $= GL.Enabled

  blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  depthFunc $= Just GL.Less

  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)


-- |
textures :: GL.Program -> GL.TextureObject -> IO ()
textures program texture = do
  GL.currentProgram $= Just program       -- 
  GL.activeTexture  $= (GL.TextureUnit 0) -- 
  GL.textureBinding GL.Texture2D $= Just texture -- Is this needed (?)§

  texture2DWrap $= (Repeated, ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)


-- |
-- TODO: Move to Michelangelo
attribute :: Integral n => (GL.AttribLocation, GL.BufferObject, n) -> IO ()
attribute (loc, buffer, count) = do
  GL.vertexAttribArray loc     $= GL.Enabled                                                                         --
  GL.bindBuffer GL.ArrayBuffer $= Just buffer                                                                        --
  GL.vertexAttribPointer loc   $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral count) GL.Float 0 GLU.offset0) --



-- |
-- TODO: Rename (too similar to render)
draw :: Program -> GL.PrimitiveMode -> M44 Float -> M44 Float -> Mesh -> IO ()
draw program primitive pm mv mesh = do
  [locv, locc] <- mapM (get . attribLocation  program) ["aVertexPosition", "aVertexColor"] --
  [uMV,  uPr]  <- mapM (get . uniformLocation program) ["uMVMatrix", "uPMatrix"]

  mapM attribute [(locv, vb, 3), (locc, cb, 4)]
  GL.uniform uPr $= pm
  GL.uniform uMV $= mv

  GL.drawArrays primitive 0 (fromIntegral n) --
  where
    (vb, cb, n) = mesh


-- |
render :: GLFW.Window -> Program -> AppState -> M.Map String Mesh -> IO ()
render window program appref meshes = do

  -- OpenGL config

  -- Set uniforms
  Just secs <- liftM (liftM realToFrac) GLFW.getTime -- Ugly hack

  let modelview  = rotateX (2*π*mx) !*! rotateY (2*π*my) !*! identity & (translation .~ (V3 0.0 0.0 (z'+dz*realToFrac mx :: Float)))
      projection = perspective (40.0 * π/180.0) 1.0 1.0 40.0
      dz =  30.0
      z' = -20.0
      (mx,my) = (100,100) -- Fake mouse position

  -- Clear
  GL.clearColor $= Color4 0.76 0.76 0.76 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- TODO: Use map or type for the meshes
  -- TODO: Scenes and cameras

  --
  let v = V3 0 0 (-mx*7.0)
  -- draw program GL.Triangles (translate v) projection (mesh) -- (translate (neg v) !*! rotateZ 0 !*! translate v) projection

  -- Swap buffers
  GLFW.swapBuffers window
  where
    translate :: Num n => V3 n -> M44 n
    translate by = identity & (translation .~ by)



------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: GLFW.Window -> Program -> IORef AppState -> M.Map String Mesh -> IO ()
mainloop window program appref meshes = do
  game <- readIORef appref
  render window program game meshes
  GLFW.pollEvents
  closing <- GLFW.windowShouldClose window
  unless closing $ mainloop window program appref meshes


-- |
attachListeners :: GLFW.Window -> IORef AppState -> IO ()
attachListeners window appref = do
  -- GLFW.setCharCallback        window $ Just (onkeypress appref)
  GLFW.setWindowSizeCallback  window $ Just (onwindowresize appref)
  GLFW.setKeyCallback         window $ Just (onkeypress appref)
  GLFW.setCursorPosCallback   window $ Just (onmousemotion appref)
  GLFW.setMouseButtonCallback window $ Just (onmousepress appref)


-- Listeners -----------------------------------------------------------------------------------------------------------------------------------------

-- |
onmousepress :: IORef AppState -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
onmousepress appref w button state modkeys = do
  modifyIORef appref (input.mouse.buttons %~ update state button) -- TODO: Fix button release logic
  return ()
  where
    update GLFW.MouseButtonState'Pressed  = S.insert
    update GLFW.MouseButtonState'Released = S.delete


-- |
onkeypress :: IORef AppState -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
onkeypress appref w key repeats keystate modifiers = do
  modifyIORef appref (input.keyboard %~ update keystate key)
  return ()
  where
    update GLFW.KeyState'Pressed   = S.insert
    update GLFW.KeyState'Repeating = S.insert
    update GLFW.KeyState'Released  = S.delete


-- |
-- TODO: Use type synonym from GLFW (?)
onmousemotion :: IORef AppState -> GLFW.Window -> Double -> Double -> IO ()
onmousemotion appref w mx my = do
  modifyIORef appref (input.mouse.position .~ Vector2D mx my)


-- |
onwindowresize :: IORef AppState -> GLFW.WindowSizeCallback
onwindowresize appref _ cx cy = do
  viewport $= (Position 0 0, Size (fromIntegral cx) (fromIntegral cy)) --


-- Helpers -------------------------------------------------------------------------------------------------------------------------------------------

-- |
perhaps :: a -> Maybe b -> (b -> a) -> a
perhaps a ma f = maybe  a f ma



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Entry
------------------------------------------------------------------------------------------------------------------------------------------------------
-- |
main :: IO ()
main = do

  --
  let (w, h) = (900, 730)

  --
  GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  mwindow <- GLFW.createWindow w h "Snakes on a Plane. Get it? Aren't I funny..." Nothing Nothing

  perhaps (putStrLn "Failed to create window") mwindow $ \window -> do
    GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does
    
    appref <- newIORef (AppState { _input    = Input { _mouse    = Mouse { _position=Vector2D 0 0, _buttons=S.empty },
                                                       _keyboard = S.empty },
    	                             _world    = (),
    	                             _settings = Settings {} })

    attachListeners window appref
    setupOpenGL --

    -- Shaders
    Right program <- let shaderpath = "C:/Users/Jonatan/Desktop/Haskell/projects/Snake/assets/shaders" in Shaders.loadShaderProgram
                                                                                                            (shaderpath </> "shader-vertex.glsl")
                                                                                                            (shaderpath </> "shader-pixel.glsl")
    GL.currentProgram $= Just program

    --
    mainloop window program appref (M.fromList [])

    GLFW.destroyWindow window
    GLFW.terminate
