
--
-- Pixels.Render
-- Rendering...
--
-- Jonatan H Sundqvist
-- August 19 2016
--

-- TODO | - Logging (ornate, sophisticated, colours, LOG, ERROR, etc.)
--        - Robust path handling
--        - Consistent and elegant error handling (cf. EitherT)
--        - Use something other than lists (eg. vectors) (?)

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ScopedTypeVariables #-}



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

import System.FilePath  ((</>), takeExtension, dropExtension, takeFileName) --
import System.Directory (getDirectoryContents)
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
import           Data.List  (transpose, isInfixOf, sortBy)
import           Data.Ord   (comparing)
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
import Graphics.GLUtil as GL

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

-- OpenGL state-machinery ----------------------------------------------------------------------------------------------------------------------------

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
  GL.vertexAttribPointer loc   $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral count) GL.Float 0 GL.offset0) --


------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Error handling
-- TODO: Refactor buffer creation (arbitrary attributes)
-- TODO: Throw error on length mismatch (?)
loadMesh :: GL.PrimitiveMode -> [V3 Float] -> [V4 Float] -> [V2 Float] -> [GL.TextureObject] -> IO Mesh
loadMesh prim vertices colours texcoords textures' = do
  [vs, cs, ts] <- forM [vertices, colours, texcoords] $ \values -> GL.makeBuffer GL.ArrayBuffer (concat $ zipWith const values vertices)
  return $ Mesh { _primitive        = prim,
                  _attributeBuffers = M.fromList [("aVertexPosition", (vs, 3)), ("aVertexColor", (cs, 4)), ("aTexCoord", (ts, 2))],
                  _numVertices      = length vertices,
                  _textures         = textures' }
  flatten :: Vector (v f) => [v f] -> [f]
  flatten = _


-- | 
loadShaders :: FilePath -> IO (Either String GL.Program)
loadShaders root = Shaders.loadShaderProg
                     (root </> "shader-vertex.glsl")
                     (root </> "shader-pixel.glsl")


-- |
-- TODO: Refactor
-- TODO: Error handling (cf. EitherT)
-- TODO: We really need better path handling
loadMeshes :: V2 Float -> Paths -> IO Meshes
loadMeshes (V2 w h) paths = do
  
  -- Load textures
  [Right tex, Right white] <- mapM (readTexture . (paths^.textures </>)) ["normal.jpg", "white.png"]

  spritepaths      <- sortBy (comparing number) . filter (hasExtension ".png") <$> getDirectoryContents (paths^.textures </> "run_frames")
  Right foxsprites <- sequence <$> (mapM (readTexture . ((paths^.textures </> "run_frames") </>)) spritepaths)

  -- Create meshes
  -- TODO: Refactor
  M.fromList <$> sequencePairs [("grid",      grid w h tilesize tilesize (0.0) white),
                                ("square",    loadMesh
                                                GL.Triangles
                                                (map (map (*30)) . concat $ Shapes.triangles [[-1,1,0], [1,1,0], [1,-1,0], [-1,-1,0]])
                                                (replicate (3*4) [1,1,1,1])
                                                (concat $ Shapes.triangles [[0,1], [1,1], [1,0], [0,0]])
                                                [tex]),
                                ("interface", interface white),
                                ("fox",       loadMesh
                                                GL.Triangles
                                                (concat . Shapes.triangles $ Shapes.planeXY (\x y _ -> [x, y, 0 :: Float]) 190 150)
                                                (replicate (3*4) [1,1,1,1])
                                                (concat $ Shapes.triangles [[0,1], [1,1], [1,0], [0,0]])
                                                (foxsprites))]
  where
    -- TODO: Factor out
    sequencePairs :: Monad m => [(a, m b)] -> m [(a,b)]
    sequencePairs ps = let invert (a, b) = b >>= return . (a,) in sequence . map invert $ ps
    
    tilesize :: Float
    tilesize = fromIntegral $ (gcd `on` floor) w h -- The 'floor' kinda ruins it, but I happen to know that w and h do not have a fractional part
    
    number :: String -> Int
    number = read . dropExtension . takeFileName -- 

------------------------------------------------------------------------------------------------------------------------------------------------------

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

  when (not . null $ _textures mesh) $ textures program (_textures mesh !! mod (frame t) (length $ _textures mesh))

  GL.drawArrays (_primitive mesh) 0 (fromIntegral $ _numVertices mesh) --
  where
    fps   = 10 -- TODO: Move this (along with all animation logic)
    frame t = floor $ t*fps
  --   (prim, vb, cb, n) = mesh


-- |
render :: GLFW.Window -> AppState -> IO ()
render window appstate = do

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
  let program'  = appstate^.graphics.L.program
      meshes'   = appstate^.graphics.meshes
      onmissing = putStrLn . ("Could not find mesh: " ++)
      invertY   = y %~ ((+ h) . negate) -- Negate and shift by the height
      drawIt t  = draw (invertY mouse') program' pm (mv !*! translate t)
  forM [("interface", V3 0 0 0),
        ("grid",      V3 0 0 0),
        ("square",    V3 0 0 0),
        ("fox",       V3 (-200) 0 0)] $ \(key, t) -> maybe (onmissing key) (drawIt t) (M.lookup key meshes')

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
mainloop :: GLFW.Window -> IORef AppState -> IO ()
mainloop window appref = do
  game <- readIORef appref
  render window game
  GLFW.pollEvents
  closing <- GLFW.windowShouldClose window
  unless closing $ mainloop window appref


-- |
attachListeners :: GLFW.Window -> IORef AppState -> IO ()
attachListeners window appref = do
  -- GLFW.setCharCallback        window $ Just (onkeypress appref)
  GLFW.setWindowSizeCallback  window $ Just (onwindowresize appref)
  GLFW.setKeyCallback         window $ Just (onkeypress     appref)
  GLFW.setCursorPosCallback   window $ Just (onmousemotion  appref)
  GLFW.setMouseButtonCallback window $ Just (onmousepress   appref)
  GLFW.setDropCallback        window $ Just (ondrop         appref)

-- Interface -----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Use applicative (?)
interface :: GL.TextureObject -> IO Mesh
interface white = do
  [vs, cs, ts] <- forM [vertices', colors', texcoords'] $ \values -> GL.makeBuffer GL.ArrayBuffer (concat $ values :: [Float])

  return $ Mesh { _primitive        = GL.Triangles,
                  _attributeBuffers = M.fromList [("aVertexPosition", (vs, 3)), ("aVertexColor", (cs, 4)), ("aTexCoord", (ts, 2))],
                  _numVertices      = length vertices',
                  _textures         = textures' }
  where
    palette     = [[1.00, 0.00, 0.00, 1.00], [1.00, 1.00, 1.00, 1.00], [0.22, 0.22, 0.22, 1.00], [0.05, 0.15, 0.95, 1.00],
                   [0.20, 0.30, 0.40, 1.00], [1.00, 0.95, 0.23, 1.00], [0.00, 0.00, 0.00, 1.00], [0.72, 0.00, 1.00, 1.00]]
    -- attributes' = M.fromList [("aVertexPosition", vs) ("aVertexColor", cs), ("aTexCoord", ts)]
    textures'   = [white]

    vertices'   = concat $ [concat $ Shapes.triangles $ Shapes.planeXY (makeVertex n) dx dy | (n,_) <- zip [0..] palette]
    colors'     = concat $ map (replicate 6) palette -- Two triangels (2*3 vertices) per quad
    texcoords'  = map (const [0,0]) vertices'
    
    --
    makeVertex n x y _ = let row = fromIntegral (n `div` cols)
                             col = fromIntegral (n `mod` cols)
                         in [x + col * (dx+padx), y + row * (dy+pady), 0]

    -- Palette layout parameters
    (dx,dy)      = (30, 30)
    (padx, pady) = (6, 6)
    (rows, cols) = (2, 4)

-- | Minimap experiment
-- minimap :: IO

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
  case paths of
    [a, b] -> when (all ((==".glsl") . takeExtension) paths) $ do
      -- TODO: Determine which is pixel and which is vertex
      -- TODO: Handle errors
      putStrLn $ "Loading shaders: " ++ show paths
      Right program' <- let (vs, ps) = if ("vertex" `isInfixOf` a) then (a,b) else (b,a) in Shaders.loadShaderProgram vs ps
      modifyIORef appref (graphics.L.program .~ program')
      GL.currentProgram $= Just program'
    _  -> return ()

-- Paths ---------------------------------------------------------------------------------------------------------------------------------------------

-- |
hasExtension :: String -> FilePath -> Bool
hasExtension ext fn = takeExtension fn == ext -- This should really be defined in the filepaths package....

-- Control -------------------------------------------------------------------------------------------------------------------------------------------

-- |
perhaps :: a -> Maybe b -> (b -> a) -> a
perhaps a ma f = maybe  a f ma

-- IO helpers ----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Loading from file
loadCursor :: FilePath -> IO (Either String GLFW.Cursor)
loadCursor fn = GLFW.createCursor (makeImage pixels) 30 30
  where
    -- TODO: Refactor this mess
    unpack :: (Integral n, Bits n) => n -> [CUChar]
    unpack c = map (fromIntegral . (\m -> (c .&. (0xFF `shiftL` m)) `shiftR` m)) [24, 16, 8, 0] -- Split up the channels
   
    pixels :: Integral n => [[n]]
    pixels = []

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
  loadMesh GL.Lines vertices colours texcoords [white]
  where
    -- dx n = fromIntegral n * w/fromIntegral cols - w/2
    -- dy n = fromIntegral n * h/fromIntegral rows - h/2
    cols = w/dx
    rows = h/dy
    colours = replicate (length vertices) (V4 0.58 0.58 0.58 1.00)
    
    vertices  = verticals ++ horizontals
    texcoords = map (\(V3 x y _) -> V2 (x/w) (y/h)) vertices

    verticals   = concat [ let x = c*dx - w/2 in [V3    (x) (-h/2) z, V3   (x) (h/2) z] | c <- [0..cols]]
    horizontals = concat [ let y = r*dy - h/2 in [V3 (-w/2)    (y) z, V3 (w/2)   (y) z] | r <- [0..rows]]



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

-- | Initialise GLFW
initialiseGLFW :: V2 Float _> IO (Either String GLFW.Window)
initialiseGLFW (V2 w h) = do
  putStrLn "GLFW init"
  GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  mwindow <- GLFW.createWindow (floor w) (floor h) "PIXELS - Nueva archivo" Nothing Nothing
  maybe (Left "Failed to create window") Right mwindow


-- | Initialise app state
initialiseApp :: GL.Program -> V2 Float -> IO (Either String AppState)
initialiseApp program' size' = do
  meshes'  <- loadMeshes size'
  program' <- loadShaders
  -- settings' <- loadSettings
  return $ AppState { _input = Input { _mouse = Mouse { _position=V2 0 0, _buttons=S.empty }, _keyboard = S.empty },
                      _world    = ([]),
                      _settings = Settings {},
                      _ui       = UI {},
                      _size     = size',
                      _graphics = Graphics { _program = program', _camera = Camera {}, _meshes=meshes' },
                      _paths    = Paths { _home = homepath, _assets = assetspath, _textures = texturepath } }
  where
    homepath    = "C:/Users/Jonatan/Desktop/Haskell/projects/"
    assetspath  = home' </> "assets"
    texturepath = assetspath </> "textures"

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
  watchFiles
  
  -- Initialise GLFW  
  ewindow <- initialiseGLFW sz

  perhaps (putStrLn "Failed to create window") mwindow $ \window -> do
    GLFW.makeContextCurrent $ Just window -- Not sure why this is needed or what it does
    
    -- webview <- Aw.createWebview w h False

    setupOpenGL --

    case eprogram of 
      Left errors    -> void $ mapM (putStrLn) errors
      Right program' -> do
        appref <- initialiseApp program' (V2 w h) >>= newIORef
        putStrLn "Listeners" >> attachListeners window appref
        
        GL.currentProgram $= Just program'

        --
        -- sz <- GLFW.getFramebufferSize window
        -- uncurry (GLFW.setWindowSize window) sz

        renderToTexture
        mainloop window appref

        GLFW.destroyWindow window
        GLFW.terminate
  where
    sz :: V2 Float
    sz@(V2 w h) = (900, 600)