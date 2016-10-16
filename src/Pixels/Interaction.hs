--
-- Pixels.Interaction
-- Interaction...
--
-- Jonatan H Sundqvist
-- August 20 2016
--

-- TODO | - High-level interaction and events
--          -- Composite events (eg. pressing while moving mouse)
--          -- Gestures (?)
--          -- Key combos
--          -- Mouse delta
--        - Key maps (dynamic)
--        - Introspection (for debuggibg)
--        - Visual feedback and bindings menu

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude      #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Interaction where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStrLn, putStr, print, putChar)
import qualified Prelude as P

import           Data.Word
import           Data.Function (on)
import           Data.Bits
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.IORef
import qualified Data.Set         as S
import qualified Data.Map         as M
import qualified Data.Traversable as T
-- import           Data.Aeson
import           Data.List  (transpose, isInfixOf, sortBy)
import           Data.Ord   (comparing)

import           Data.Array.Repa ((:.)(..))
import qualified Data.Array.Repa as R

import Text.Printf
import Text.Read (readMaybe, readEither)

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Metric
import Linear.Projection

import System.FilePath  ((</>), takeExtension, dropExtension, takeFileName) --
import System.Directory (getDirectoryContents)
import System.FSNotify

import           Control.Monad.Trans.Class as St
import qualified Control.Monad.Trans.State as St
import           Control.Monad.Trans.Either
import           Control.Lens hiding (argument)
import           Control.Monad
import           Control.Monad.Loops (whileM)
import           Control.Applicative ((<$>), (<*>), liftA2)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Exception (finally, catch, displayException)

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL                  as GL hiding (projection, perspective, Line, position, ortho)
import Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import Graphics.Rendering.OpenGL.GL.Shaders       as GL --

import           Leibniz.Constants (π)

import qualified Graphics.Michelangelo.Shaders as Shaders
import           Graphics.Michelangelo.Transformations

import           Pixels.Types
import           Pixels.Trinkets
import           Pixels.Lenses as L
import           Pixels.Render as Render
import qualified Pixels.Walker as Walker
import           Pixels.Load   as Load



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
mainloop :: IORef AppState -> IO ()
mainloop appref = do
  -- throttleFPS 30 -- TODO: Implement
  app <- readIORef appref
  t   <- maybe 0 id <$> GLFW.getTime
  render app
  tick appref t
  tryTakeMVar (app^.input.command) >>= (maybe pass $ runCommand (app^.window) appref) -- Console commands
  GLFW.pollEvents
  closing <- GLFW.windowShouldClose (app^.window)
  unless closing $ mainloop appref


-- | 
-- TODO: Limit FPS
tick :: IORef AppState -> Double -> IO ()
tick appref t = do
  app <- readIORef appref
  let r            = flip mod 32 . floor $ t*0.8*32
      path'        = app^.input.mouse.path
      imSize       = V2 32 32
      band x y     = let width = 5 in abs (x^2 + y^2 - r^2) < (width^2)
      nearPath p   = any ((< 3) . norm . subtract (toLocalVector p)) path'
      toLocalVector p = let p' = fromIntegral <$> p in liftA2 (*) (app^.size) $ liftA2 (/) p' (fromIntegral <$> imSize)
      im       = Render.createRepaImage (fromIntegral <$> imSize) (\p -> if nearPath p then (0,0,0,255) else (255,255,255,255))
      [tex] = app^.graphics.L.meshes.at "mickey"._Just.oTextures
  Render.modifyTexture (V2 0 0) im tex


-- |
attachListeners :: GLFW.Window -> IORef AppState -> IO ()
attachListeners window appref = do
  -- GLFW.setCharCallback        window $ Just (onkeypress appref)
  GLFW.setWindowSizeCallback  window $ Just (onwindowresize appref)
  GLFW.setKeyCallback         window $ Just (onkeypress     appref)
  GLFW.setCursorPosCallback   window $ Just (onmousemotion  appref)
  GLFW.setMouseButtonCallback window $ Just (onmousepress   appref)
  GLFW.setDropCallback        window $ Just (ondrop         appref)

------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Does most of the heavy lifting on behalf of the 'runCommand' function
chooseAction :: String -> [String] -> IORef AppState -> IO ()
chooseAction action options appref = case action of
  "quit"       -> quit appref
  "colour"     -> withArgs (graphics.clearColour)   (parseColour)
  "loglevel"   -> withArgs (debug.logLevel)         (const . (readMaybe <=< listToMaybe))
  "projection" -> withArgs (graphics.matProjection) (parseProjection)
  _            -> putStrLn $ printf "Unknown command (%s)" action
  where
    modify l new     = modifyIORef appref (l .~ new)
    withArgs l parse = readIORef appref >>= maybe noargs (modify l) . parse options -- TODO: Move parse into modifyIORef (saves a read) (?)
    
    noargs = putStrLn $ printf "Invalid arguments for '%s': %s" action (show options)

    parseColour [r,g,b]   _ = Color4 <$> readMaybe r <*> readMaybe g <*> readMaybe b <*> pure 1.0
    parseColour [r,g,b,a] _ = Color4 <$> readMaybe r <*> readMaybe g <*> readMaybe b <*> readMaybe a
    parseColour _         _ = Nothing

    parseProjection ["ortho"]       app = let (V2 w h) = app^.size in Just $ ortho (-w/2) (w/2) (-h/2) (h/2) (0) 100
    parseProjection ["perspective"] app = let (V2 w h) = app^.size in Just $ perspective (60.0 * π/180.0) (w/h) 0.0 100.0
    parseProjection _               _   = Nothing


-- | Tries to parse and execute a console command
runCommand :: GLFW.Window -> IORef AppState -> String -> IO ()
runCommand   _      _  "" = pass
runCommand win appref cmd = case cmd of
  "Sjung o gudinna"                                    -> putStrLn "Om vreden som brann"
  "Stop all the clocks"                                -> putStrLn "Cut off the telephone"
  "Two parallel lines have so much in common"          -> putStrLn "It's a shame they'll never meet"
  "I went to a really emotional wedding the other day" -> putStrLn "Even the cake was in tiers"
  _ -> let (action:options) = words cmd in chooseAction action options appref

-- Listeners -----------------------------------------------------------------------------------------------------------------------------------------

-- |
onmousepress :: IORef AppState -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
onmousepress appref w button state modkeys = do
  modifyIORef appref (input.mouse.buttons %~ update state button) -- TODO: Fix button release logic
  where
    update GLFW.MouseButtonState'Pressed  = S.insert
    update GLFW.MouseButtonState'Released = S.delete


-- |
onkeypress :: IORef AppState -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
onkeypress appref w key repeats keystate modifiers = do
  modifyIORef appref (input.keyboard %~ update keystate key)
  when (key == GLFW.Key'Escape) $ quit appref
  where
    update GLFW.KeyState'Pressed   = S.insert
    update GLFW.KeyState'Repeating = S.insert
    update GLFW.KeyState'Released  = S.delete


-- |
-- TODO: Use type synonym from GLFW (?)
-- TODO: Refactor
onmousemotion :: IORef AppState -> GLFW.Window -> Double -> Double -> IO ()
onmousemotion appref w mx my = do
  modifyIORef appref . St.execState $ do
    app <- St.get
    input.mouse.path %= take 200 . (V2 (realToFrac mx) (realToFrac my) :)
    let (m1:m2:_)  = app^.input.mouse.path 
        (V2 dx dy) = m2 - m1 
        delta      = V3 0 0 dy
        θ          = dy * 1/(app^.size.y) * 2*π
    when (all (flip S.member $ app^.input.mouse.buttons) [GLFW.MouseButton'1, GLFW.MouseButton'2]) $ graphics.camera.pan      += (V3 0  0 dy)
    when (all (flip S.member $ app^.input.mouse.buttons) [GLFW.MouseButton'3])                     $ graphics.camera.rotation += (V3 0  θ  0)



-- |
onwindowresize :: IORef AppState -> GLFW.WindowSizeCallback
onwindowresize appref _ cx cy = do
  modifyIORef appref (size .~ new)
  viewport $= (Position 0 0, Size (fromIntegral cx) (fromIntegral cy)) --
  where
    new = fromIntegral <$> (V2 cx cy)


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
    _  -> pass

-- Setup ---------------------------------------------------------------------------------------------------------------------------------------------

-- | Initialise GLFW
initialiseGLFW :: V2 Float -> IO (Either String GLFW.Window)
initialiseGLFW (V2 w h) = runEitherT $ do
  lift $ do
    putStrLn "GLFW init"
    GLFW.init
    mapM GLFW.windowHint [GLFW.WindowHint'Samples 4, GLFW.WindowHint'DepthBits 16, GLFW.WindowHint'AlphaBits 8]
  mwindow <- lift $ GLFW.createWindow (floor w) (floor h) "PIXELS - Nueva archivo" Nothing Nothing
  maybe (left "Failed to create window") right mwindow


-- | Initialise app state
initialiseApp :: V2 Float -> IO (Either String (IORef AppState))
initialiseApp size'@(V2 w h) = runEitherT $ do

  -- Initialise GLFW
  window' <- EitherT $ initialiseGLFW size'
  lift (GLFW.makeContextCurrent $ Just window') -- Not sure why this is needed or what it does
  Just t  <- lift GLFW.getTime

  -- 
  meshes'  <- EitherT (Load.meshes (debug' t) size' paths')
  program' <- EitherT (Load.shaders (debug' t) (assetspath </> "shaders"))

  let repaImage = Render.createRepaImage (V2 32 32) (\(V2 x y) -> (fromIntegral $ min (x*40) 255, fromIntegral $ min (y*40) 255,255,255))
  repaTex <- lift $ textureFromImage repaImage
  let meshes'' = meshes' & (at "mickey"._Just.oTextures .~ [repaTex])
  -- settings' <- loadSettings

  command <- lift newEmptyMVar -- Asynchronous console interaction (fingers crossed)
  
  --
  Load.fonts (paths'^.assets </> "fonts")

  --
  lift (GL.currentProgram $= Just program')

  let app = AppState { _alive    = True,
                       _debug    = debug' t,
                       _window   = window',
                       _input    = Input { _mouse = Mouse { _path=[V2 0 0], _buttons=S.empty }, _keyboard = S.empty, _command=command },
                       _settings = Settings {},
                       _session  = Session {},
                       _ui       = UI {},
                       _size     = size',
                       _graphics = Graphics { _program = program',
                                              _camera  = Camera {  _pan = V3 0 0 0, _rotation = V3 0 0 0 },
                                              _meshes  = meshes'',
                                              _clearColour = GL.Color4 0.032 0.029 0.027 1.0,
                                              _resources   = CPUResources { _images=M.fromList [("live", repaImage)] },
                                              _matModelview  = mv ,
                                              _matProjection = pm },
                       _paths    = paths' }
  appref <- lift $ newIORef app

  lift $ do
    attachListeners window' appref
    forkIO . void $ do
      whileM ((^.alive) <$> readIORef appref) (getLine >>= putMVar command)
      putStrLn "Done reading commands"
    return appref
  where
    --
    -- mv = identity !*! rotateAxis (V3 1 0 0) (2*π*mx/700) !*! translate (V3 0 0 (-my/30)) -- !*! translate (V3 0 0 (5 * sin secs)) :: M44 Float
    mv = identity
    -- pm = ortho (-w/2) (w/2) (-h/2) (h/2) (0) (5.0) :: M44 Float
    pm = perspective (60.0 * π/180.0) (w/h) (-1.0) 100.0

    -- TODO: Don't hard-code the root
    debug' t = Debug { _logLevel = WarningLevel, _startTime = t }
    paths' = Paths { _home = homepath, _assets = assetspath, _textures = texturepath }
    homepath    = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"
    assetspath  = homepath </> "assets"
    texturepath = assetspath </> "textures"


------------------------------------------------------------------------------------------------------------------------------------------------------

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

-- | 
testEitherT :: IO (Either String Int)
testEitherT = runEitherT $ do
  first  <- EitherT $ readEither <$> getLine
  second <- EitherT $ readEither <$> getLine
  return $ first + second

------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
-- main = execute $ do
main = eitherT (putStrLn) (return) $ do
  --
  lift $ do
    logStr (Debug { _logLevel=InfoLevel }) InfoLevel 1 "Testing random walks"
    watchFiles
    setupOpenGL --
    -- webview <- Aw.createWebview w h False

  appref <- EitherT $ initialiseApp (V2 720 435)
  -- sz <- GLFW.getFramebufferSize window
  -- uncurry (GLFW.setWindowSize window) sz
  lift $ do
    renderToTexture
    -- TODO: Print exceptions (?)
    finally (mainloop appref) $ do
      start     <- (^.(debug.startTime)) <$> readIORef appref
      Just time <- GLFW.getTime
      printf "The program ran for %.02f seconds.\n" (time - start)
      uninitialise appref
      readIORef appref >>= GLFW.destroyWindow . (^.window)
      GLFW.terminate