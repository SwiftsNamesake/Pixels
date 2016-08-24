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
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Render where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import System.IO (stdout, hFlush) --
import System.FilePath ((</>), takeExtension)    --
-- import System.FilePath.Glob (match, compile)
-- import System.Console.ANSI        --

import Linear.Projection
import Linear.Quaternion
import Linear.Matrix hiding (transpose)
import Linear.V3
import Linear.V4

import           Data.IORef
import qualified Data.Set         as S
import qualified Data.Map         as M
import qualified Data.Traversable as T
import           Data.Aeson
import           Data.List  (transpose, isInfixOf)
import qualified Data.Array.Repa as R

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.Trans.Either
import Control.Applicative ((<$>))

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL as GL hiding (projection, perspective, Line, position, ortho)
import Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import Graphics.Rendering.OpenGL.GL.Shaders as GL --

-- import Graphics.Rendering.OpenGL.Raw as GLRaw

import Foreign.Storable
import Foreign.C.Types (CUChar)
import Foreign.Marshal as Marshal hiding (void)
import Foreign.Ptr     as Ptr

import Graphics.GLUtil.JuicyTextures (readTexture)
import Graphics.GLUtil as GLU

import           Control.Monad.Trans.Class as St
import qualified Control.Monad.Trans.State as St

import           Graphics.Michelangelo.Transformations
import qualified Graphics.Michelangelo.Shaders as Shaders
-- import qualified Graphics.Michelangelo.Shapes  as Shapes

import           Graphics.Michelangelo.Shapes (π)
import qualified Graphics.Michelangelo.Shapes as Shapes


-- import qualified Graphics.UI.Awesomium         as Aw
-- import qualified Graphics.UI.Awesomium.WebCore as Aw

import Cartesian.Space.Types hiding (size)
import Cartesian.Plane.Types
import Cartesian.Plane.Lenses
import Cartesian.Plane

import Pixels.Types
import Pixels.Interaction
import Pixels.Lenses as L



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


-- | Frame buffer objects
createFBO :: IO ()
createFBO = do
  return ()



-- |
-- TODO: Move to Michelangelo
attribute :: Integral n => (GL.AttribLocation, GL.BufferObject, n) -> IO ()
attribute (loc, buffer, count) = do
  GL.vertexAttribArray loc     $= GL.Enabled                                                                         --
  GL.bindBuffer GL.ArrayBuffer $= Just buffer                                                                        --
  GL.vertexAttribPointer loc   $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral count) GL.Float 0 GLU.offset0) --


-- |
loadMesh :: GL.PrimitiveMode -> [[Float]] -> [[Float]] -> IO Mesh
loadMesh prim shape colours = do
  vs <- GLU.makeBuffer GL.ArrayBuffer (concat $ shape :: [Float])
  cs <- GLU.makeBuffer GL.ArrayBuffer (concat . zipWith const colours $ shape :: [Float])
  return (prim, vs, cs, length shape)


-- |
-- TODO: Rename (too similar to render)
draw :: Program -> M44 Float -> M44 Float -> Mesh -> IO ()
draw program pm mv mesh = do
  [locv, locc] <- mapM (get . attribLocation  program) ["aVertexPosition", "aVertexColor"] --
  [uMV,  uPr]  <- mapM (get . uniformLocation program) ["uMVMatrix", "uPMatrix"]

  mapM attribute [(locv, vb, 3), (locc, cb, 4)]
  GL.uniform uPr $= pm
  GL.uniform uMV $= mv

  GL.drawArrays prim 0 (fromIntegral n) --
  where
    (prim, vb, cb, n) = mesh


-- |
render :: GLFW.Window -> AppState -> [Mesh] -> IO ()
render window appstate meshes = do

  -- OpenGL config

  -- Set uniforms
  Just secs <- maybe (return 0) (return . realToFrac) <$> GLFW.getTime -- Ugly hack

  -- Clear
  GL.clearColor $= Color4 0.76 0.76 0.76 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- TODO: Use map or type for the meshes
  -- TODO: Scenes and cameras

  --

  -- GL.lineStipple $= Just (4, 0xFF)
  GL.lineWidth $= 0.01
  -- forM meshes $ \mesh -> draw (appstate^.graphics.L.program) pm (mv secs) mesh -- (translate (neg v) !*! rotateZ 0 !*! translate v) projection
  draw (appstate^.graphics.L.program) pm (mv !*! translate (snapToNearest (V3 15 15 0) $ screenToWorld m)) (meshes!!1)
  draw (appstate^.graphics.L.program) pm (mv) (meshes!!0)


  -- Swap buffers
  GLFW.swapBuffers window
  where
    (Vector2D w h) = appstate^.size

    translate :: Num n => V3 n -> M44 n
    translate by = identity & (translation .~ by)

    from2D (Vector2D x' y') = V3 x' y' 0
    screenToWorld (V3 x' y' z')  = V3 (x'-w/2) (-y'+h/2) (z') -- TODO: Make sure this is correct and robust

    snapToNearest res@(V3 xr yr zr) p@(V3 x' y' z') = V3 (snap' xr x') (snap' yr y') (z')
    snap' res v = res * (fromIntegral . round $ v/res) -- Snaps in one dimensions

    m = from2D $ appstate^.input.mouse.position -- Mouse position

    mv = identity -- !*! translate (V3 0 0 (5 * sin secs)) :: M44 Float
    -- pm = ortho (-1) (1) (-1) (1) (0) (-5) :: M44 Float
    pm = ortho (-w/2) (w/2) (-h/2) (h/2) (0) (5.0) :: M44 Float


------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: GLFW.Window -> IORef AppState -> [Mesh] -> IO ()
mainloop window appref meshes = do
  game <- readIORef appref
  render window game meshes
  GLFW.pollEvents
  closing <- GLFW.windowShouldClose window
  unless closing $ mainloop window appref meshes


-- |
attachListeners :: GLFW.Window -> IORef AppState -> IO ()
attachListeners window appref = do
  -- GLFW.setCharCallback        window $ Just (onkeypress appref)
  GLFW.setWindowSizeCallback  window $ Just (onwindowresize appref)
  GLFW.setKeyCallback         window $ Just (onkeypress     appref)
  GLFW.setCursorPosCallback   window $ Just (onmousemotion  appref)
  GLFW.setMouseButtonCallback window $ Just (onmousepress   appref)
  GLFW.setDropCallback        window $ Just (ondrop         appref)

-- Listeners -----------------------------------------------------------------------------------------------------------------------------------------

-- |
onmousepress :: IORef AppState -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
onmousepress appref w button state modkeys = do
  modifyIORef appref (input.mouse.buttons %~ update state button) -- TODO: Fix button release logic
  app <- readIORef appref
  print $ app^.input.mouse.position
  return ()
  where
    update GLFW.MouseButtonState'Pressed  = S.insert
    update GLFW.MouseButtonState'Released = S.delete


-- |
onkeypress :: IORef AppState -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
onkeypress appref w key repeats keystate modifiers = do
  modifyIORef appref (input.keyboard %~ update keystate key)
  when (key == GLFW.Key'Escape) $ GLFW.setWindowShouldClose w True
  -- GLFW.exit
  return ()
  where
    update GLFW.KeyState'Pressed   = S.insert
    update GLFW.KeyState'Repeating = S.insert
    update GLFW.KeyState'Released  = S.delete


-- |
-- TODO: Use type synonym from GLFW (?)
onmousemotion :: IORef AppState -> GLFW.Window -> Double -> Double -> IO ()
onmousemotion appref w mx my = do
  modifyIORef appref (input.mouse.position .~ Vector2D (realToFrac mx) (realToFrac my))


-- |
onwindowresize :: IORef AppState -> GLFW.WindowSizeCallback
onwindowresize appref _ cx cy = do
  modifyIORef appref (size .~ new)
  viewport $= (Position 0 0, Size (fromIntegral cx) (fromIntegral cy)) --
  where
    new = dotmap fromIntegral (Vector2D cx cy)


-- |
ondrop :: IORef AppState -> GLFW.Window -> [String] -> IO ()
ondrop appref window paths = do
  putStrLnF "DROOOPPINNNG!"
  case paths of
    [a, b] -> when (all ((==".glsl") . takeExtension) paths) $ do
      -- TODO: Determine which is pixel and which is vertex
      -- TODO: Handle errors
      putStrLnF $ "Loading shaders: " ++ show paths
      Right program' <- let (vs, ps) = if ("vertex" `isInfixOf` a) then (a,b) else (b,a) in Shaders.loadShaderProgram vs ps
      modifyIORef appref (graphics.L.program .~ program')
      GL.currentProgram $= Just program'
    _  -> return ()

-- Control -------------------------------------------------------------------------------------------------------------------------------------------

-- |
perhaps :: a -> Maybe b -> (b -> a) -> a
perhaps a ma f = maybe  a f ma

-- IO helpers ----------------------------------------------------------------------------------------------------------------------------------------

-- |
putStrLnF :: String -> IO ()
putStrLnF s = putStrLn s >> hFlush stdout


-- |
loadCursor :: IO GLFW.Cursor
loadCursor = let pixels = [[0011, 1001, 1001, 1001, 1001, 1001, 1001, 0011],
                           [1001, 1001, 1001, 1001, 1001, 1001, 1001, 1001],
                           [1001, 1001, 1001, 1001, 1001, 1001, 1001, 1001],
                           [1001, 1001, 1001, 1001, 1001, 1001, 1001, 1001],
                           [0101, 0101, 0101, 0101, 0101, 0101, 0101, 0101],
                           [0101, 0101, 0101, 0101, 0101, 0101, 0101, 0101],
                           [0101, 0101, 0101, 0101, 0101, 0101, 0101, 0101],
                           [0011, 0101, 0101, 0101, 0101, 0101, 0101, 0011]]
             in GLFW.createCursor (makeImage pixels) 0 0
  where
    pad l d = reverse . take l . reverse . (replicate l d ++) -- We need four digits exactly
    digits  = pad 4 0 . map (read . (:[])) . show

    flatten   px = map (*0xFF) . concat . concat . map (map digits) $ px
    makeImage px = GLFW.Image { GLFW.imagePixels = flatten px,
                                GLFW.imageWidth  = (length $ px!!0),
                                GLFW.imageHeight = (length $ px) }


-- Rendering -----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- grid :: Integral n => Vector2D Float -> n -> n -> Float -> IO Mesh
-- TODO: More options
-- TODO: 3D grid
-- TODO: Factor out (to Cartesian)
grid :: Float -> Float -> Float -> Float -> Float -> IO Mesh
grid w h dx dy z = putStrLnF "Loading grid" >> print (cols, rows) >> loadMesh GL.Lines vertices colours
  where
    -- dx n = fromIntegral n * w/fromIntegral cols - w/2
    -- dy n = fromIntegral n * h/fromIntegral rows - h/2
    cols = w/dx
    rows = h/dy
    colours = replicate (length $ vertices) [0.58, 0.58, 0.58, 1.00]
    
    vertices    = verticals ++ horizontals

    verticals   = concat [[[c*dx - w/2,  -h/2, z], [c*dx - w/2, h/2, z]] | c <- [1..cols]]
    horizontals = concat [[[-w/2, r*dy - h/2,  z], [w/2, r*dy - h/2, z]] | r <- [1..rows]]


-- |
-- stipple :: 

------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Initial app state
initial :: GL.Program -> Vector2D Float -> AppState
initial program' size' = AppState { _input    = Input { _mouse = Mouse { _position=Vector2D 0 0, _buttons=S.empty }, _keyboard = S.empty },
                                    _world    = ([]),
                                    _settings = Settings {},
                                    _ui       = UI {},
                                    _size     = size',
                                    _graphics = Graphics { _program = program', _camera = Camera {} } }



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Entry
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do

  --
  let (w, h) = (900, 680) :: (Float, Float)

  --
  putStrLnF "GLFW init"
  GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  mwindow <- GLFW.createWindow (floor w) (floor h) "Le Pixelateur" Nothing Nothing

  perhaps (putStrLnF "Failed to create window") mwindow $ \window -> do
    GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does
    
    -- putStrLnF "Webview"
    -- webview <- Aw.createWebview w h False

    setupOpenGL --

    -- Shaders
    let home = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"
    let shaderpath = home </> "assets/shaders"
    print shaderpath
    eprogram <- Shaders.loadShaderProgram
                  (shaderpath </> "shader-vertex.glsl")
                  (shaderpath </> "shader-pixel.glsl")

    case eprogram of 
      Left errors    -> void $ mapM (putStrLnF) errors
      Right program' -> do
        appref <- newIORef $ initial program' (Vector2D w h)
        putStrLnF "Listeners" >> attachListeners window appref
        
        GL.currentProgram $= Just program'

        --
        -- cur <- loadCursor
        -- GLFW.setCursor window cur
        [tex] <- mapM (readTexture . ("assets/textures" </>)) ["normal.jpg"]

        meshes <- sequence [grid w h 15 15 (0.0),
                            loadMesh
                              GL.Triangles
                              (concat . Shapes.triangles $ [0,0,0] : Shapes.polygon (\x y -> [x,y,0]) 12 20)
                              (replicate (12*3) [0,0.4,0.05,1.0])
                           ]

        mainloop window appref meshes

        GLFW.destroyWindow window
        GLFW.terminate