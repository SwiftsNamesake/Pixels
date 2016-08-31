
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
{-# LANGUAGE InstanceSigs      #-}
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
import Text.Printf

import Text.Read (readMaybe, readEither)

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
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.IORef
import qualified Data.Set         as S
import qualified Data.Map         as M
import qualified Data.Traversable as T
import           Data.Aeson
import           Data.List  (transpose, isInfixOf, sortBy)
import           Data.Ord   (comparing)
import qualified Data.Array.Repa as R

-- import Control.Monad.Trans.Either
import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.Loops (whileM)
import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar

import           Control.Monad.Trans.Class as St hiding (lift)
import qualified Control.Monad.Trans.State as St

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL                  as GL hiding (projection, perspective, Line, position, ortho)
import Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import Graphics.Rendering.OpenGL.GL.Shaders       as GL --

import Graphics.GLUtil.JuicyTextures (readTexture)
import Graphics.GLUtil as GL

-- import Graphics.Rendering.FTGL as FTGL
-- import Graphics.Rendering.TrueType.STB as STB

import Foreign.Storable
import Foreign.C.Types (CUChar)
import Foreign.Marshal as Marshal hiding (void)
import Foreign.Ptr     as Ptr

-- import qualified Graphics.UI.Awesomium         as Aw
-- import qualified Graphics.UI.Awesomium.WebCore as Aw

import           Graphics.Michelangelo.Transformations
import qualified Graphics.Michelangelo.Shaders as Shaders
import qualified Graphics.Michelangelo.Shapes  as Shapes
import           Graphics.Michelangelo.Shapes (π)
import           Graphics.Michelangelo.Types (Matrix44(..))

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
  putStrLn "Setting up OpenGL"
  clearColor $= GL.Color4 0.032 0.029 1.027 1.0

  blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  blend     $= GL.Enabled
  depthFunc $= Just GL.Less

  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)


-- |
-- TODO: Rename (?)
setTexture :: GL.Program -> GL.TextureObject -> IO ()
setTexture program texture = do
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
loadMesh :: Debug -> GL.PrimitiveMode -> [V3 Float] -> [V4 Float] -> [V2 Float] -> [GL.TextureObject] -> IO Mesh
loadMesh db prim vertices colours texcoords textures' = do
  logStr db InfoLevel 1 "Loading mesh..."
  unless allEqual $ do
    logStr db InfoLevel 2 $ printf "Loading mesh with attribute buffers of unequal length!" -- TODO: bail
    logStr db InfoLevel 2 $ printf "Vertex count: %d" (length vertices)
    logStr db InfoLevel 2 $ printf "Colour count: %d" (length colours)
    logStr db InfoLevel 2 $ printf "Tex    count: %d" (length texcoords)
  [vs, cs, ts] <- forM [flatten vertices, flatten colours, flatten texcoords] $ GL.makeBuffer GL.ArrayBuffer
  return $ Mesh { _primitive        = prim,
                  _attributeBuffers = M.fromList [("aVertexPosition", (vs, 3)), ("aVertexColor", (cs, 4)), ("aTexCoord", (ts, 2))],
                  _numVertices      = length vertices,
                  _oTextures        = textures' }
  where
    allEqual = all (== length vertices) [length colours, length texcoords]


-- |
loadShaders :: Debug -> FilePath -> IO (Either String GL.Program)
loadShaders db root = either (Left . unlines) (Right) <$> Shaders.loadShaderProgram (root </> "shader-vertex.glsl") (root </> "shader-pixel.glsl")


-- |
-- TODO: Refactor
-- TODO: Error handling (cf. EitherT)
-- TODO: We really need better path handling (...)
loadMeshes :: Debug -> V2 Float -> Paths -> IO (Either String Meshes)
loadMeshes db (V2 w h) paths = runEitherT $ do
  -- Load textures
  [tex, white, mickey] <- EitherT $ (sequence <$> mapM (readTexture . (paths^.textures </>)) ["normal.jpg", "white.png", "sorcerermickey.png"] :: IO (Either String [GL.TextureObject]))
  spritepaths          <- EitherT $ Right <$> (sortBy (comparing number) . filter (hasExtension ".png") <$> getDirectoryContents (paths^.textures </> "run_frames"))
  foxsprites           <- EitherT $ (sequence <$> (mapM (readTexture . ((paths^.textures </> "run_frames") </>)) spritepaths))

  -- Create meshes
  -- TODO: Refactor
  EitherT $ (Right . M.fromList) <$> sequencePairs [("grid",      grid db w h tilesize tilesize (0.0) white),
                                                    ("square",    loadRectangle (V2 30 30) (V4 1 1 1 1) [tex]),
                                                    ("interface", interface db white),
                                                    ("fox",       loadRectangle (V2 190 159) (V4 1 1 1 0) (foxsprites)),
                                                    ("mickey",    loadRectangle (V2 190 150) (V4 1 1 1 0) ([mickey]))]
  where
    -- TODO: Factor out
    loadRectangle (V2 dx dy) colour texes = loadMesh db
                                              (GL.Triangles)
                                              (concat . Shapes.triangles $ Shapes.planeXY (\x y _ -> V3 x y (0 :: Float)) dx dy)
                                              (replicate 6 colour)
                                              (concat $ Shapes.triangles [V2 0 1, V2 1 1, V2 1 0, V2 0 0])
                                              (texes)

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
-- TODO: Error handling
-- TODO: Arbitrary uniforms and attributes  (?)
-- TODO: Custom setup functions (per mesh)  (?)
-- TODO: Cache attrib and uniform locations (?)
draw :: Debug -> V3 Float -> Program -> M44 Float -> M44 Float -> Mesh -> IO ()
draw db mouse program pm mv mesh = do
  
  -- TODO: Factor out
  logStr db InfoLevel 1 "Drawing mesh"
  t <- realToFrac . fromMaybe 0.0 <$> GLFW.getTime -- Time (s)

  -- Set attribute buffers
  forM ["aVertexPosition", "aVertexColor", "aTexCoord"] $ \attr -> do
    logStr db InfoLevel 2 $ printf "Setting attribute: " ++ attr
    loc <- get $ attribLocation  program attr
    logStr db InfoLevel 2 $ printf "%s" (show loc)
    let Just (buffer, count) = M.lookup attr (mesh^.attributeBuffers)
    logStr db InfoLevel 2 $ printf "%s" (show (buffer, count))
    attribute (loc, buffer, count)
  
  -- Set uniform values
  -- [uMV,  uPr, uMouse, uTex0, uTime] <- mapM (get . uniformLocation program) ["uMVMatrix", "uPMatrix", "uMouseVec", "uTex0", "uTime"]
  forM [("uMVMatrix", wrap $ Matrix44 mv),
        ("uPMatrix",  wrap $ Matrix44 pm),
        ("uMouseVec", wrap $ mouse),
        ("uTex0",     wrap $ (0 :: GLint)),
        ("uTime",     wrap $ (t :: GLfloat))] $ \(u, v) -> do
    logStr db InfoLevel 2 $ printf "Setting uniform: %s" (show u)
    (get $ GL.uniformLocation program u) >>= v

  -- GL.uniform uPr    $= (Matrix44 pm)
  -- GL.uniform uMV    $= (Matrix44 mv)
  -- GL.uniform uTex0  $= (0 :: GL.GLint)
  -- GL.uniform uMouse $= mouse
  -- GL.uniform uTime  $= (realToFrac t :: GL.GLfloat)
  -- GL.uniform uMouse $= (V4 200 200 0 0 :: V4 Float)

  when (not . null $ mesh^.oTextures) $ do
    logStr db InfoLevel 2 "Setting texture"
    setTexture program $ chooseTexture (mesh^.oTextures) t
  
  logStr db InfoLevel 2 $ printf "Drawing arrays (%d vertices)" (mesh^.numVertices)
  GL.drawArrays (mesh^.primitive) 0 (fromIntegral $ mesh^.numVertices) --
  where
    chooseTexture [tex] _ = tex
    chooseTexture texes t = texes !! mod (frame t) (length texes)

    fps   = 10 -- TODO: Move this (along with all animation logic)
    frame t = floor $ t*fps

    wrap :: (Show u, GL.Uniform u) => u -> GL.UniformLocation -> IO ()
    wrap v loc = do
      -- putStrLn $ "Setting uniform: " ++ show v
      -- print loc
      GL.uniform loc $= v


-- |
render :: AppState -> IO ()
render app = do

  -- OpenGL config
  -- Set uniforms

  -- Clear
  GL.clearColor $= (app^.graphics.L.clearColour)
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- TODO: Use map or type for the meshes
  -- TODO: Scenes and cameras

  --
  -- tm <- fromMaybe 0.0 <$> GLFW.getTime -- Time (s)

  -- GL.lineStipple $= Just (4, 0xFF)
  GL.lineWidth $= 0.01
  -- forM meshes $ \mesh -> draw (app^.graphics.L.program) pm (mv secs) mesh -- (translate (neg v) !*! rotateZ 0 !*! translate v) projection
  -- draw (mouse' & y %~ ((+ h) . negate)) (app^.graphics.L.program) pm (mv !*! translate (snapToNearest (V3 15 15 0) worldMouse)) (meshes!!1)
  let program'  = app^.graphics.L.program
      meshes'   = app^.graphics.meshes
      onmissing = putStrLn . ("  Could not find mesh: " ++)
      invertY   = y %~ ((+ h) . negate) -- Negate and shift by the height
      drawIt t  = draw (app^.debug) (invertY mouse') program' pm (mv !*! translate (V3 0 0 (-4)) !*! translate t)
  forM [("grid",      V3 0 0 0),
        ("square",    V3 0 0 0),
        ("fox",       V3 (-200) 0 0),
        ("mickey",    V3 ( 200) 0 0),
        ("interface", V3 0 0 0)] $ \(key, t) -> do
    logStr (app^.debug) InfoLevel 2 ("Drawing " ++ key)
    maybe (onmissing key) (drawIt t) (M.lookup key meshes')

  -- Swap buffers
  GLFW.swapBuffers (app^.window)
  where
    -- App data
    (V2 w h) = app^.size

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
    mouse'     = from2D $ app^.input.mouse.position --
    worldMouse = screenToWorld mouse'                    -- Mouse position in world coords





------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: IORef AppState -> IO ()
mainloop appref = do
  app <- readIORef appref
  render app
  tryTakeMVar (app^.input.command) >>= (maybe pass $ runCommand (app^.window) appref) -- Console commands
  GLFW.pollEvents
  closing <- GLFW.windowShouldClose (app^.window)
  unless closing $ mainloop appref


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

runCommand :: GLFW.Window -> IORef AppState -> String -> IO ()
runCommand win appref cmd = case action of
  ""         -> pass
  "quit"     -> quit appref
  "colour"   -> maybe noargs (\new -> modifyIORef appref (graphics.clearColour .~ new)) (parseColour options)
  "loglevel" -> maybe noargs (\new -> modifyIORef appref (debug.logLevel       .~ new)) (listToMaybe options >>= readMaybe)
  _          -> putStrLn $ printf "Unknown command (%s)" cmd
  where
    ~(action:options) = words cmd
    noargs = putStrLn $ printf "Invalid arguments for '%s': %s" action (show options)

    parseColour [r,g,b]   = Color4 <$> readMaybe r <*> readMaybe g <*> readMaybe b <*> pure 1.0
    parseColour [r,g,b,a] = Color4 <$> readMaybe r <*> readMaybe g <*> readMaybe b <*> readMaybe a
    parseColour _         = Nothing

-- Interface -----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Use applicative (?)
interface :: Debug -> GL.TextureObject -> IO Mesh
interface db white = do
  [vs, cs, ts] <- forM [flatten vertices', flatten colors', flatten texcoords'] $ GL.makeBuffer GL.ArrayBuffer
  return $ Mesh { _primitive        = GL.Triangles,
                  _attributeBuffers = M.fromList [("aVertexPosition", (vs, 3)), ("aVertexColor", (cs, 4)), ("aTexCoord", (ts, 2))],
                  _numVertices      = length vertices',
                  _oTextures        = textures' }
  where
    textures' :: [GL.TextureObject]
    textures' = [white]

    palette :: [V4 Float]
    palette = [V4 1.00 0.00 0.00 1.00, V4 1.00 1.00 1.00 1.00, V4 0.22 0.22 0.22 1.00, V4 0.05 0.15 0.95 1.00,
               V4 0.20 0.30 0.40 1.00, V4 1.00 0.95 0.23 1.00, V4 0.00 0.00 0.00 1.00, V4 0.72 0.00 1.00 1.00]
    
    vertices' :: [V3 Float]
    vertices' = concat $ [concat $ Shapes.triangles $ Shapes.planeXY (makeVertex n) dx dy | (n,_) <- zip [0..] palette]

    colors' :: [V4 Float]
    colors' = concat $ map (replicate 6) palette -- Two triangels (2*3 vertices) per quad

    texcoords' :: [V2 Float]
    texcoords' = map (const $ V2 0 0) vertices'
    
    --
    -- makeVertex :: (RealFloat r) => Int -> r -> r -> r -> V3 r
    makeVertex :: Int -> Float -> Float -> Float -> V3 Float
    makeVertex n x y _ = let row = fromIntegral (n `div` cols)
                             col = fromIntegral (n `mod` cols)
                         in V3 (x + col * (dx+padx)) (y + row * (dy+pady)) (0)

    -- Palette layout parameters
    (V2   dx   dy) = V2 30 30 :: V2 Float
    (V2 padx pady) = V2  6  6 :: V2 Float
    (V2 rows cols) = V2  2 4  :: V2 Int


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
  when (key == GLFW.Key'Escape) $ quit appref
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
loadCursor fn = Right <$> GLFW.createCursor (makeImage pixels) 30 30
  where
    -- TODO: Refactor this mess
    unpack :: (Integral n, Bits n) => n -> [CUChar]
    unpack c = map (fromIntegral . (\m -> (c .&. (0xFF `shiftL` m)) `shiftR` m)) [24, 16, 8, 0] -- Split up the channels
   
    pixels :: [[Int]]
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
grid :: Debug -> Float -> Float -> Float -> Float -> Float -> GL.TextureObject -> IO Mesh
grid db w h dx dy z white = do
  putStrLn $ "First vertical equals first horizontal: " ++ (show $ (verticals!!0) == (horizontals!!0))
  loadMesh db GL.Lines vertices colours texcoords [white]
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
initialiseGLFW :: V2 Float -> IO (Either String GLFW.Window)
initialiseGLFW (V2 w h) = do
  putStrLn "GLFW init"
  GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  mwindow <- GLFW.createWindow (floor w) (floor h) "PIXELS - Nueva archivo" Nothing Nothing
  return $ maybe (Left "Failed to create window") Right mwindow


-- | Initialise app state
initialiseApp :: V2 Float -> IO (Either String (IORef AppState))
initialiseApp size' = runEitherT $ do

  -- Initialise GLFW  
  window' <- EitherT $ initialiseGLFW size'
  lift (GLFW.makeContextCurrent $ Just window') -- Not sure why this is needed or what it does

  -- 
  meshes'  <- EitherT (loadMeshes debug' size' paths')
  program' <- EitherT (loadShaders debug' (assetspath </> "shaders"))
  -- settings' <- loadSettings

  command <- lift newEmptyMVar -- Asynchronous console interaction (fingers crossed)
  
  lift (GL.currentProgram $= Just program')

  let app = AppState { _alive    = True,
                       _debug    = debug',
                       _window   = window',
                       _input    = Input { _mouse = Mouse { _position=V2 0 0, _buttons=S.empty }, _keyboard = S.empty, _command=command },
                       _world    = ([]),
                       _settings = Settings {},
                       _ui       = UI {},
                       _size     = size',
                       _graphics = Graphics { _program = program',
                                              _camera  = Camera {},
                                              _meshes  = meshes',
                                              _clearColour = GL.Color4 0.032 0.029 0.027 1.0 },
                       _paths    = paths' }
  appref <- lift $ newIORef app

  lift $ do
    attachListeners window' appref
    forkIO . void $ do
      whileM ((^.alive) <$> readIORef appref) (getLine >>= putMVar command)
      putStrLn "Done reading commands"
    return appref
  where
    debug' = Debug { _logLevel = WarningLevel }
    homepath    = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"
    assetspath  = homepath </> "assets"
    texturepath = assetspath </> "textures"
    paths' = Paths { _home = homepath, _assets = assetspath, _textures = texturepath }


-- |
-- TODO: Rename
uninitialise :: IORef AppState -> IO ()
uninitialise appref = do
  putStrLn "Tearing down app"
  modifyIORef appref (alive .~ False)


-- |
quit :: IORef AppState -> IO ()
quit appref = do
  uninitialise appref
  win <- (^.window) <$> readIORef appref
  GLFW.setWindowShouldClose win True

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

------------------------------------------------------------------------------------------------------------------------------------------------------

-- data LoggerT

------------------------------------------------------------------------------------------------------------------------------------------------------


-- | 
testEitherT :: IO (Either String Int)
testEitherT = runEitherT $ do
  first  <- EitherT (readEither <$> getLine)
  second <- EitherT (readEither <$> getLine)
  return $ first + second


-- |
main :: IO ()
main = do
  
  --
  putStrLn "Testing EitherT"
  e <- testEitherT
  print e

  --
  watchFiles
  
  -- webview <- Aw.createWebview w h False
  setupOpenGL --
  Right appref <- initialiseApp (V2 720 435)

  -- sz <- GLFW.getFramebufferSize window
  -- uncurry (GLFW.setWindowSize window) sz

  renderToTexture
  mainloop appref
  
  uninitialise appref
  readIORef appref >>= GLFW.destroyWindow . (^.window)
  GLFW.terminate