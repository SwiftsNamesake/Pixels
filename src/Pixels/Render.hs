
--
-- Pixels.Render
-- Rendering...
--
-- Jonatan H Sundqvist
-- August 19 2016
--

-- TODO | - Logging (ornate, sophisticated, colours, LOG, ERROR, etc.)
--        - Robust path handling

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Render where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStrLn, putStr, print, putChar)
import qualified Prelude as P

import Debug.Trace

import System.FilePath ((</>), takeExtension)    --
import System.FSNotify
-- import System.FilePath.Glob (match, compile)
-- import System.Console.ANSI        --

import Linear.Projection
import Linear.Quaternion
import Linear.Matrix hiding (transpose, trace)
import Linear.V2
import Linear.V3
import Linear.V4

import           Data.Function (on)
import           Data.Bits
import           Data.Maybe (fromMaybe)
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
import Control.Applicative ((<$>), (<*>))

import Control.Concurrent (threadDelay, forkIO)

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL as GL hiding (projection, perspective, Line, position, ortho)
import Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import Graphics.Rendering.OpenGL.GL.Shaders as GL --

-- import Graphics.Rendering.FTGL as FTGL
-- import Graphics.Rendering.TrueType.STB as STB

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
import           Graphics.Michelangelo.Types (Matrix44(..))


-- import qualified Graphics.UI.Awesomium         as Aw
-- import qualified Graphics.UI.Awesomium.WebCore as Aw

import Cartesian.Space.Types hiding (size)
import Cartesian.Plane.Types
import Cartesian.Plane.Lenses
import Cartesian.Plane

import Pixels.Types
import Pixels.Interaction
import Pixels.Lenses as L
import Pixels.Trinkets



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
setupOpenGL :: IO ()
setupOpenGL = do
  clearColor $= GL.Color4 0.032 0.029 1.027 1.0
  blend      $= GL.Enabled

  blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  depthFunc $= Just GL.Less

  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)


-- |
-- TODO: Rename (?)
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
loadMesh :: GL.PrimitiveMode -> [[Float]] -> [[Float]] -> [[Float]] -> [GL.TextureObject] -> IO Mesh
loadMesh prim shape colours texcoords textures' = do
  -- TODO: Refactor buffer creation (arbitrary attributes)
  vs <- GLU.makeBuffer GL.ArrayBuffer (concat $ shape :: [Float])
  cs <- GLU.makeBuffer GL.ArrayBuffer (concat . zipWith const colours   $ shape :: [Float]) -- TODO: Throw error on length mismatch (?)
  ts <- GLU.makeBuffer GL.ArrayBuffer (concat . zipWith const texcoords $ shape :: [Float]) -- TODO: Throw error on length mismatch (?)
  -- return (prim, vs, cs, length shape)
  return $ Mesh { _primitive        = prim,
                  _attributeBuffers = M.fromList [("aVertexPosition", (vs, 3)), ("aVertexColor", (cs, 4)), ("aTexCoord", (ts, 2))],
                  _numVertices      = length shape,
                  _textures         = textures' }



-- |
-- TODO: Rename (too similar to render)
draw :: V3 Float -> Program -> M44 Float -> M44 Float -> Mesh -> IO ()
draw mouse program pm mv mesh = do
  [locv, locc, loct]                <- mapM (get . attribLocation  program) ["aVertexPosition", "aVertexColor", "aTexCoord"] --
  [uMV,  uPr, uMouse, uTex0, uTime] <- mapM (get . uniformLocation program) ["uMVMatrix", "uPMatrix", "uMouseVec", "uTex0", "uTime"]
  -- trace "Pattern matching" $ 
  
  -- TODO: clean this up
  let Just (vb, _) = M.lookup "aVertexPosition" (_attributeBuffers mesh)
  let Just (cb, _) = M.lookup "aVertexColor"    (_attributeBuffers mesh)
  let Just (tb, _) = M.lookup "aTexCoord"       (_attributeBuffers mesh)

  t <- fromMaybe 0.0 <$> GLFW.getTime

  mapM attribute [(locv, vb, 3), (locc, cb, 4), (loct, tb, 2)]
  GL.uniform uPr    $= (Matrix44 pm)
  GL.uniform uMV    $= (Matrix44 mv)
  GL.uniform uTex0  $= (0 :: GL.GLint)
  GL.uniform uMouse $= mouse
  GL.uniform uTime  $= (realToFrac t :: GL.GLfloat)
  -- GL.uniform uMouse $= (V4 200 200 0 0 :: V4 Float)

  when (not . null $ _textures mesh) $ textures program (_textures mesh !! 0)

  GL.drawArrays (_primitive mesh) 0 (fromIntegral $ _numVertices mesh) --
  -- where
  --   (prim, vb, cb, n) = mesh


-- |
render :: GLFW.Window -> AppState -> [Mesh] -> IO ()
render window appstate meshes = do

  -- OpenGL config
  -- Set uniforms

  -- Clear
  GL.clearColor $= GL.Color4 0.032 0.029 0.027 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- TODO: Use map or type for the meshes
  -- TODO: Scenes and cameras

  --

  -- GL.lineStipple $= Just (4, 0xFF)
  GL.lineWidth $= 0.01
  -- forM meshes $ \mesh -> draw (appstate^.graphics.L.program) pm (mv secs) mesh -- (translate (neg v) !*! rotateZ 0 !*! translate v) projection
  -- draw (mouse' & y %~ ((+ h) . negate)) (appstate^.graphics.L.program) pm (mv !*! translate (snapToNearest (V3 15 15 0) worldMouse)) (meshes!!1)
  draw (mouse' & y %~ ((+ h) . negate)) (appstate^.graphics.L.program) pm (mv) (meshes!!1)
  draw (mouse' & y %~ ((+ h) . negate)) (appstate^.graphics.L.program) pm (mv !*! translate (snapToNearest (V3 20 20 0) worldMouse)) (meshes!!0)

  -- Swap buffers
  GLFW.swapBuffers window
  where
    -- App data
    (V2 w h) = appstate^.size

    -- Matrix functions
    translate :: Num n => V3 n -> M44 n
    translate by = identity & (translation .~ by)

    -- Matrices
    -- mv = identity !*! rotateAxis (V3 1 0 0) (2*π*mx/700) !*! translate (V3 0 0 (-my/30)) -- !*! translate (V3 0 0 (5 * sin secs)) :: M44 Float
    mv = identity
    pm = ortho (-w/2) (w/2) (-h/2) (h/2) (0) (5.0) :: M44 Float
    -- pm = ortho (-1) (1) (-1) (1) (0) (-5) :: M44 Float
    -- pm = perspective (40.0 * π/180.0) 1.0 1.0 40.0

    -- Coordinate space operations
    from2D (V2 x' y') = V3 x' y' 0
    screenToWorld (V3 x' y' z')  = V3 (x'-w/2) (-y'+h/2) (z') -- TODO: Make sure this is correct and robust
    
    snapToNearest res@(V3 xr yr zr) p@(V3 x' y' z') = V3 (snap' xr x') (snap' yr y') (z')
    snap' res v = res * (fromIntegral . round $ v/res) -- Snaps in one dimensions

    -- Mouse
    mouse'     = from2D $ appstate^.input.mouse.position --
    worldMouse = screenToWorld mouse'                    -- Mouse position in world coords





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
  modifyIORef appref (input.mouse.position .~ V2 (realToFrac mx) (realToFrac my))


-- |
onwindowresize :: IORef AppState -> GLFW.WindowSizeCallback
onwindowresize appref _ cx cy = do
  modifyIORef appref (size .~ new)
  viewport $= (Position 0 0, Size (fromIntegral cx) (fromIntegral cy)) --
  where
    new = dotmap fromIntegral (V2 cx cy)


-- |
ondrop :: IORef AppState -> GLFW.Window -> [String] -> IO ()
ondrop appref window paths = do
  putStrLn "DROOOPPINNNG!"
  case paths of
    [a, b] -> when (all ((==".glsl") . takeExtension) paths) $ do
      -- TODO: Determine which is pixel and which is vertex
      -- TODO: Handle errors
      putStrLn $ "Loading shaders: " ++ show paths
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
pencil :: (Integral n, Bits n) => [[n]]
pencil = [[0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x939594ff, 0x000300ff, 0x000400ff, 0x000500ff, 0x000104ff, 0x000007ff, 0x000007ff, 0x010207ff, 0x000002ff, 0x888c8bff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x9e9292ff, 0x0f0000ff, 0x0b0000ff, 0x0c0000ff, 0x0b0000ff, 0x0c0004ff, 0x0a0004ff, 0x060004ff, 0x000000ff, 0x969a99ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x6e6c6dff, 0x3e3030ff, 0x6b4344ff, 0x91585eff, 0x8e4f5aff, 0x7a3846ff, 0x743641ff, 0x753d48ff, 0x744651ff, 0x20020aff, 0x040000ff, 0x1f211eff, 0x3b3b3dff, 0x626065ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x362627ff, 0x180000ff, 0x5e2025ff, 0xd3818dff, 0xc86b7eff, 0xa4475cff, 0x9d4657ff, 0x9d5561ff, 0x9c656bff, 0x1d0000ff, 0x0a0000ff, 0x040200ff, 0x060000ff, 0x2e271fff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0xffffff00, 0xffffff00, 0x808082ff, 0x7d6d6eff, 0x77494bff, 0x753337ff, 0x9e4c58ff, 0xc86c7bff, 0xcd677cff, 0xb75469ff, 0xbc6379ff, 0x5c1825ff, 0x461e16ff, 0x594425ff, 0x57480fff, 0x564b09ff, 0x4f460dff, 0x59512aff, 0x7c756bff, 0x6c676dff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0xffffff00, 0xffffff00, 0x0a0000ff, 0x180000ff, 0x9c575cff, 0xd0767fff, 0xcb6d7bff, 0xcd6a7cff, 0xce687dff, 0xcb6d7dff, 0xd1818cff, 0x2a0000ff, 0x1b0000ff, 0x847534ff, 0x9f8b35ff, 0x9a8728ff, 0x9f8f37ff, 0x7a702bff, 0x0a0100ff, 0x060000ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0xffffff00, 0xffffff00, 0x3f1718ff, 0x2f0000ff, 0xa9535cff, 0xd26d7bff, 0xce6574ff, 0xcf6a7aff, 0xc56a7bff, 0xae6167ff, 0xb07867ff, 0x300a00ff, 0x250f00ff, 0x918016ff, 0xa48c1eff, 0xa3891aff, 0x9a8417ff, 0x7e6e0fff, 0x150a00ff, 0x0b0200ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x010000ff, 0x110000ff, 0xae7377ff, 0xd17e86ff, 0xce6e7cff, 0xd0697aff, 0xd06776ff, 0xce6c79ff, 0xb6656cff, 0x2d0000ff, 0x220000ff, 0xebd17cff, 0xe2cc55ff, 0xddc642ff, 0xe2c744ff, 0xba9d1fff, 0x9d8308ff, 0x9c8617ff, 0x9f8d37ff, 0xa89c60ff, 0x080000ff, 0x040000ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x010200ff, 0x0f0000ff, 0xb16e77ff, 0xce7281ff, 0xca6779ff, 0xce697bff, 0xcf6e81ff, 0xd67f88ff, 0xb9756cff, 0x290000ff, 0x200000ff, 0xdcc14cff, 0xe0c135ff, 0xdcbd27ff, 0xe1c331ff, 0xb5980cff, 0x9a7b00ff, 0x9d7f0dff, 0x9c851fff, 0x9c8a38ff, 0x0e0300ff, 0x0a0500ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x000300ff, 0x0e0000ff, 0x8e4a55ff, 0xa44658ff, 0xc36273ff, 0xcf7382ff, 0x8b3947ff, 0x310000ff, 0x440e00ff, 0xe0b570ff, 0xc5aa37ff, 0xdcc335ff, 0xdfbd26ff, 0xdcb91dff, 0xe0bf22ff, 0xddbe28ff, 0xd8b630ff, 0xccac31ff, 0x9a7c0aff, 0x9b831fff, 0x8d7d2fff, 0x897f4aff, 0x141200ff, 0x000100ff, 0xacafa6ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x000404ff, 0x0c0000ff, 0x854752ff, 0xa14b58ff, 0xc56d79ff, 0xd2828bff, 0x71393cff, 0x220000ff, 0x270000ff, 0xecc963ff, 0xdac037ff, 0xd9c226ff, 0xdcbf23ff, 0xdcbe1eff, 0xe3c322ff, 0xdcb91dff, 0xe2bf2dff, 0xd2b028ff, 0x9b7900ff, 0x9d7f0fff, 0x9c8621ff, 0xa49749ff, 0x0a0300ff, 0x050300ff, 0xafb0a2ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x000104ff, 0x0c0002ff, 0x874b57ff, 0x9a4c59ff, 0x783137ff, 0x622922ff, 0x6f4a30ff, 0x866931ff, 0x8e6c0aff, 0xe8c744ff, 0xdac127ff, 0xd7c01cff, 0xdcc021ff, 0xdfc123ff, 0xdcba1bff, 0xe1bf20ff, 0xe3c026ff, 0xd2af1dff, 0xc7a319ff, 0xc9a729ff, 0x997e0bff, 0x9a8627ff, 0x5d500cff, 0x4f481aff, 0x67634aff, 0x6b695dff, 0x9b9794ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x010204ff, 0x0f0002ff, 0x8a4d5cff, 0xa65f6dff, 0x290000ff, 0x180000ff, 0x644c04ff, 0xe9d165ff, 0xe6c63fff, 0xdebd24ff, 0xd8bc1bff, 0xdcc01eff, 0xe1be22ff, 0xe4bf26ff, 0xdeba24ff, 0xe1c027ff, 0xe0bf22ff, 0xdfbf1fff, 0xe3c225ff, 0xe1c130ff, 0x9b7b00ff, 0x9e8215ff, 0x9b8822ff, 0xa19445ff, 0x2d2100ff, 0x0a0100ff, 0x4c4344ff, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x000004ff, 0x0a0000ff, 0x582a34ff, 0x663636ff, 0x4b301bff, 0x382700ff, 0x857007ff, 0xe2c83fff, 0xe0bf24ff, 0xe3c21dff, 0xdec11fff, 0xdabc1cff, 0xe4bd20ff, 0xe2bb22ff, 0xe1bd27ff, 0xdbba21ff, 0xddbd1dff, 0xe1c11eff, 0xdebf19ff, 0xe3c227ff, 0xbb9611ff, 0xb9971cff, 0xa18710ff, 0x9f8924ff, 0x5d4b09ff, 0x312401ff, 0x554d40ff, 0xaeaaabff, 0xa8a7acff, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x000005ff, 0x030000ff, 0x100501ff, 0x0e0000ff, 0x76682bff, 0xa28f31ff, 0xb9a022ff, 0xddbf2bff, 0xddba1eff, 0xe1bf1fff, 0xdbbd1dff, 0xdfbf1fff, 0xe4bd20ff, 0xe1b91cff, 0xe4c022ff, 0xe3c322ff, 0xe1c420ff, 0xdcbf1bff, 0xdebe1dff, 0xdcb91dff, 0xe5c12fff, 0xe8c23bff, 0xa37f05ff, 0x9d7e12ff, 0x9d8628ff, 0xa1934aff, 0x635f32ff, 0x050400ff, 0x000100ff, 0xffffff00, 0xffffff00, 0xffffff00],
          [0x010204ff, 0x000201ff, 0x000200ff, 0x040400ff, 0x766d20ff, 0x998719ff, 0xb89d1aff, 0xe7c736ff, 0xe4c22bff, 0xdebd20ff, 0xdec020ff, 0xe0c01fff, 0xe2bb1eff, 0xe8c023ff, 0xdcb917ff, 0xe0bf1aff, 0xdebf19ff, 0xddbd1aff, 0xe1c121ff, 0xe2bf23ff, 0xe2bd26ff, 0xe3bd2cff, 0xa98300ff, 0x9f7b00ff, 0x9f840fff, 0x9b8923ff, 0x615910ff, 0x070500ff, 0x050400ff, 0xffffff00, 0xffffff00, 0xffffff00],
          [0xffffff00, 0xffffff00, 0x191816ff, 0x080000ff, 0x746820ff, 0x9c8b23ff, 0x987f08ff, 0x997c00ff, 0xa18400ff, 0xe1c32dff, 0xdec11dff, 0xddbe16ff, 0xe5c125ff, 0xdeb71eff, 0xe4be1fff, 0xe1bb18ff, 0xe0bd17ff, 0xe1be1aff, 0xe0bc1eff, 0xe1bc23ff, 0xe1bc26ff, 0xe1be26ff, 0xe7c428ff, 0xe8c62fff, 0xb99b15ff, 0x967d07ff, 0x9e9027ff, 0x9f9540ff, 0x93894eff, 0x0a0200ff, 0x050300ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0x030000ff, 0x090000ff, 0x736632ff, 0xa08f3fff, 0xa38d28ff, 0x9c830dff, 0xa58a09ff, 0xe2c637ff, 0xdfc324ff, 0xddc01cff, 0xe0bd21ff, 0xe4c024ff, 0xdfb91aff, 0xe7c120ff, 0xe0bd19ff, 0xe5c220ff, 0xe0bc1eff, 0xe1bc23ff, 0xe0bb25ff, 0xe3be25ff, 0xdab912ff, 0xe1c325ff, 0xb79d24ff, 0xa08a25ff, 0x9e8d31ff, 0x9e9142ff, 0x94854cff, 0x0b0100ff, 0x040200ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x504533ff, 0x251600ff, 0x5b4900ff, 0xa08d26ff, 0xa0880cff, 0xb0960eff, 0xac8f03ff, 0xddbd2aff, 0xdcbb1eff, 0xdfbd1dff, 0xe3c121ff, 0xdebc1dff, 0xe1c023ff, 0xdbba1dff, 0xdfbf1fff, 0xe3c024ff, 0xe2b824ff, 0xe5bc24ff, 0xe3c21bff, 0xe3ca3aff, 0x685904ff, 0x281e00ff, 0x312300ff, 0x302006ff, 0x2a1f0dff, 0x0a0400ff, 0x010200ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x342b2eff, 0x0b0000ff, 0x423403ff, 0x9c8d3cff, 0x9b881fff, 0x9c860eff, 0x977b00ff, 0xdec03aff, 0xe3c42cff, 0xdabc1cff, 0xddbc1fff, 0xdcbb20ff, 0xdfbe23ff, 0xe1c023ff, 0xdebe1eff, 0xe1bd1fff, 0xe6be21ff, 0xe0b822ff, 0xe2c334ff, 0xe7d25fff, 0x453900ff, 0x0c0200ff, 0x0d0000ff, 0x0b0000ff, 0x080000ff, 0x040000ff, 0x000000ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x807d86ff, 0x645e5eff, 0x60593fff, 0x5d511fff, 0x66560bff, 0x998526ff, 0x957d11ff, 0xc1a52aff, 0xc2a519ff, 0xd9bb25ff, 0xdabb25ff, 0xe3c42cff, 0xdebd22ff, 0xe0be1fff, 0xe0b91cff, 0xe5bd1dff, 0xe4be15ff, 0xe6c72fff, 0x8d780fff, 0x897833ff, 0x6a582aff, 0x554224ff, 0x513d24ff, 0x3c2c1cff, 0x382f28ff, 0x030000ff, 0x030400ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x979589ff, 0x090200ff, 0x0e0200ff, 0xa3924dff, 0xa08b32ff, 0x9a8214ff, 0x957a00ff, 0xd3b829ff, 0xdec232ff, 0xd9bb25ff, 0xddbf21ff, 0xe0be1eff, 0xe7c122ff, 0xe2bb1eff, 0xe0bf24ff, 0xf0d758ff, 0x140300ff, 0x120000ff, 0x8f7550ff, 0xe3c6a4ff, 0xc2aa88ff, 0x917d62ff, 0x857c6dff, 0x050100ff, 0x020200ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xa3a49eff, 0x1d1b0cff, 0x393218ff, 0x7a6d41ff, 0x7b6824ff, 0x947f22ff, 0x98820bff, 0xccb22aff, 0xd1b62bff, 0xd5ba25ff, 0xddbf1fff, 0xdfc01aff, 0xdcb915ff, 0xe1c130ff, 0xb59e2eff, 0xbba85bff, 0x2f1c00ff, 0x230b00ff, 0x9b7d59ff, 0xd7b68dff, 0xc7a67dff, 0x9c825dff, 0x918167ff, 0x0a0300ff, 0x010100ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x0a0000ff, 0x0f0100ff, 0x9b8941ff, 0x9b851fff, 0x947e04ff, 0x987d00ff, 0xbba011ff, 0xddc328ff, 0xd8be21ff, 0xdfc12dff, 0xe6ca4fff, 0x160300ff, 0x120000ff, 0xdbc4a4ff, 0xdec3a5ff, 0xd5b58cff, 0xd3b183ff, 0xcfae81ff, 0xd1b890ff, 0xc5b498ff, 0x080000ff, 0x050200ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x050200ff, 0x0a0000ff, 0x9f9162ff, 0xa39242ff, 0x99841bff, 0x9b8307ff, 0xbea61eff, 0xe0cb3cff, 0xdcc63fff, 0xe8d15fff, 0xeed882ff, 0x140000ff, 0x120000ff, 0xceb386ff, 0xceb280ff, 0xceb07aff, 0xcdb27dff, 0xd0b98dff, 0xdbc8a7ff, 0xcabba8ff, 0x070000ff, 0x050200ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x0b0000ff, 0x120300ff, 0x867422ff, 0xa28b21ff, 0x99830bff, 0xa08b0eff, 0x806b00ff, 0x180600ff, 0x140000ff, 0xd0bd95ff, 0xc9b18bff, 0xceb07eff, 0xd2b57bff, 0xcfb57aff, 0xceb984ff, 0x958561ff, 0x1f1300ff, 0x261c12ff, 0x090100ff, 0x010000ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x070000ff, 0x0d0000ff, 0x897948ff, 0x9e8b3cff, 0x9a8623ff, 0x9b841cff, 0x816c19ff, 0x0f0000ff, 0x0e0000ff, 0xe3cba9ff, 0xd0b485ff, 0xd1b180ff, 0xceb07cff, 0xd4bb83ff, 0xd9c799ff, 0x968a72ff, 0x110800ff, 0x1c1411ff, 0x040000ff, 0x06050bff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x8b8585ff, 0x9e938dff, 0x4d3e27ff, 0x433300ff, 0x756317ff, 0xa18a2dff, 0x7c6821ff, 0x0b0000ff, 0x080000ff, 0xbca586ff, 0xb19263ff, 0xd4b284ff, 0xd8b98bff, 0x8f7a4fff, 0x6d6040ff, 0x504738ff, 0x211916ff, 0x221a18ff, 0x040002ff, 0x000007ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x130a03ff, 0x0d0300ff, 0x4e410cff, 0xa7913bff, 0x7c691dff, 0x070000ff, 0x070103ff, 0x998a6dff, 0x8e764aff, 0xd3b184ff, 0xe2c4a0ff, 0x574b35ff, 0x120c00ff, 0x1e150cff, 0x1d140dff, 0x211614ff, 0x0a0102ff, 0x000002ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x474344ff, 0x2b271cff, 0x554f2fff, 0x7f702fff, 0x65561dff, 0x040000ff, 0x010000ff, 0x7d7560ff, 0x726241ff, 0xaa906fff, 0xb29a82ff, 0x453e36ff, 0x0f0f0dff, 0x1b1811ff, 0x1c150dff, 0x1e1412ff, 0x060000ff, 0x050608ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x77796cff, 0x060100ff, 0x070200ff, 0x020100ff, 0x050503ff, 0x030000ff, 0x060000ff, 0x0a0100ff, 0x070000ff, 0x060405ff, 0x000104ff, 0x000200ff, 0x000200ff, 0x040000ff, 0x070101ff, 0x000004ff, 0xffffff00],
          [0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0xffffff00, 0x7b817fff, 0x090907ff, 0x181715ff, 0x151513ff, 0x0f0f0dff, 0x191917ff, 0x13140fff, 0x171310ff, 0x181715ff, 0x131217ff, 0x15181dff, 0x0e1817ff, 0x0f1815ff, 0x1a1615ff, 0x181214ff, 0x141318ff, 0xffffff00]]

-- |
-- TODO: Loading from file
loadCursor :: IO GLFW.Cursor
loadCursor = GLFW.createCursor (makeImage (pencil :: [[Int]])) 30 30
  where
    -- TODO: Refactor this mess
    unpack :: (Integral n, Bits n) => n -> [CUChar]
    unpack c = map (fromIntegral . (\m -> (c .&. (0xFF `shiftL` m)) `shiftR` m)) [24, 16, 8, 0] -- Split up the channels

    flatten :: (Integral n, Bits n) => [[n]] -> [CUChar]
    flatten = concat . map (unpack) . concat

    makeImage :: (Integral n, Bits n) => [[n]] -> GLFW.Image
    makeImage px = GLFW.Image { GLFW.imagePixels = flatten px,
                                GLFW.imageWidth  = (length $ px!!0),
                                GLFW.imageHeight = (length $ px) }

-- Rendering -----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- grid :: Integral n => V2 Float -> n -> n -> Float -> IO Mesh
-- TODO: More options
-- TODO: 3D grid
-- TODO: Factor out (to Cartesian)
grid :: Float -> Float -> Float -> Float -> Float -> GL.TextureObject -> IO Mesh
grid w h dx dy z white = do
  putStrLn $ "First vertical equals first horizontal: " ++ (show $ (verticals!!0) == (horizontals!!0))
  loadMesh GL.Lines vertices colours (map (\[x,y,z] -> [x/w, y/h]) vertices) [white]
  where
    -- dx n = fromIntegral n * w/fromIntegral cols - w/2
    -- dy n = fromIntegral n * h/fromIntegral rows - h/2
    cols = w/dx
    rows = h/dy
    colours = replicate (length vertices) [0.58, 0.58, 0.58, 1.00]
    
    vertices    = verticals ++ horizontals

    verticals   = concat [ let x = c*dx - w/2 in [[x,  -h/2, z], [x, h/2, z]] | c <- [0..cols]]
    horizontals = concat [ let y = r*dy - h/2 in [[-w/2, y, z], [w/2, y, z]] | r <- [0..rows]]



-- |
-- http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
renderToTexture :: IO ()
renderToTexture = do
  --
  putStrLn "Rendering to texture"

  -- Create render target
  -- [name] <- GL.genFramebuffers 1
  -- GL.bindFramebuffer GL.FRAMEBUFFER name
  return ()


-- |
-- stipple :: 

------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Initial app state
initial :: GL.Program -> V2 Float -> AppState
initial program' size' = AppState { _input    = Input { _mouse = Mouse { _position=V2 0 0, _buttons=S.empty }, _keyboard = S.empty },
                                    _world    = ([]),
                                    _settings = Settings {},
                                    _ui       = UI {},
                                    _size     = size',
                                    _graphics = Graphics { _program = program', _camera = Camera {} } }


------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
watchFiles :: IO ()
watchFiles = void . forkIO . withManager $ \mgr -> do
  -- start a watching job (in the background)
  watchDir
    mgr          -- manager
    "."          -- directory to watch
    (const True) -- predicate
    print        -- action

  -- sleep forever (until interrupted)
  forever $ threadDelay 1000000



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Entry
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do

  --
  let (w, h) = (900, 680) :: (Float, Float)

  --
  watchFiles
  
  putStrLn "GLFW init"
  GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  mwindow <- GLFW.createWindow (floor w) (floor h) "Le Pixelateur" Nothing Nothing

  perhaps (putStrLn "Failed to create window") mwindow $ \window -> do
    GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does
    
    -- putStrLn "Webview"
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
      Left errors    -> void $ mapM (putStrLn) errors
      Right program' -> do
        appref <- newIORef $ initial program' (V2 w h)
        putStrLn "Listeners" >> attachListeners window appref
        
        GL.currentProgram $= Just program'

        --
        cur <- loadCursor
        GLFW.setCursor window cur
        sz <- GLFW.getFramebufferSize window
        uncurry (GLFW.setWindowSize window) sz
        [Right tex, Right white] <- mapM (readTexture . ("assets/textures" </>)) ["normal.jpg", "white.png"]

        meshes <- sequence [let sz = fromIntegral $ (gcd `on` floor) w h in grid w h sz sz (0.0) white,
                            loadMesh
                              GL.Triangles
                              (map (map (*30)) . concat $ Shapes.triangles [[-1,1,0], [1,1,0], [1,-1,0], [-1,-1,0]])
                              (replicate (3*4) [1,1,1,1])
                              (concat $ Shapes.triangles [[-1,1,0], [1,1,0], [1,-1,0], [-1,-1,0]])
                              [tex]
                           ]
        renderToTexture
        putStrLn . show . concat $ Shapes.triangles [[0,0,0], [-1,1,0], [1,1,0], [1,-1,0], [-1,-1,0], [-1,1,0]]
        mainloop window appref meshes

        GLFW.destroyWindow window
        GLFW.terminate