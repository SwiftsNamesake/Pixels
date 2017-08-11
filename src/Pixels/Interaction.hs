-- |
-- Module      : Pixels.Render
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : 
-- Portability : 
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



-- GHC pragmas ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

-- API -----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Interaction where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import Prelude hiding (print, putStr, putStrLn)

import qualified Data.ByteString as B
import           Data.Word
import           Data.Function (on)
import           Data.Bits
import           Data.Time
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.IORef
import           Data.Foldable
import qualified Data.Set         as S
import qualified Data.Map         as M
import qualified Data.Traversable as T
import qualified Data.Vector.Unboxed as VU
-- import           Data.Aeson
import           Data.List  (transpose, isInfixOf, sortBy)
import           Data.Ord   (comparing)

-- import           Data.Array.Repa ((:.)(..))
-- import qualified Data.Array.Repa as R

import Text.Printf
import Text.Read (readMaybe, readEither)

import Linear (V2(..), V3(..), V4(..))

import System.FilePath  ((</>), (<.>), takeExtension, dropExtension, takeFileName, normalise, isValid)
import System.Directory (getDirectoryContents)
import System.FSNotify
import System.Console.ANSI as ANSI

import qualified Codec.Picture.Types as Juicy

import           Control.Monad.Trans.Class as St
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as St
import           Control.Monad.Trans.Either
import           Control.Lens hiding (argument, (<.>))
import           Control.Monad
import           Control.Monad.Loops (whileM, whileJust)
import           Control.Applicative ((<$>), (<*>), liftA2)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception (finally, catch, displayException)

import           Graphics.GPipe hiding (texture, vector)
import qualified Graphics.GPipe.Context.GLFW       as GLFW
import           Graphics.GPipe.Context.GLFW       (WindowConfig(..), setWindowShouldClose)
import           Graphics.GPipe.Context.GLFW.Input (Key(..), KeyState(..), MouseButton(..), MouseButtonState(..))

import Leibniz.Constants (π)
import Cartesian.Core    (x,y,z)

import           Pixels.Types
import           Pixels.Lenses
import           Pixels.Persistence
import           Pixels.Trinkets
import           Pixels.Render.Surface as Surface
import           Pixels.Render         as Render
import qualified Pixels.Load           as Load

-- Definitions ---------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Move
--        - Don't hard-code
root :: FilePath
root = "C:/Users/Jonatan/Desktop/Haskell/projects/Pixels"


-- |
initialInput :: InputChannel -> V2 Int -> Input
initialInput ch sz = Input {
                       fSize   = sz,
                       fScroll = V2 0 0,
                       fMouse  = Mouse (V2 0 0) S.empty,
                       fKeyboard = S.empty,
                       fInputChannel = ch }

-- App actions ---------------------------------------------------------------------------------------------------------------------------------------

-- These should eventually be moved to some other module

-- | 
clearCanvas :: App os -> AppT os (App os)
clearCanvas app = return (app & easel.canvas.surface.pixels.vector %~ VU.map (const fill))
  where
    fill = app^.easel.canvas.colour


-- |
quitApplication :: App os -> AppT os (App os)
quitApplication app = do
  setWindowShouldClose (app^.window) True
  return app


-- |
-- TODO | - Choose path properly
--        -
saveCanvas :: App os -> AppT os (App os)
saveCanvas app = do
  now <- normalise . formatTime defaultTimeLocale "%s" <$> liftIO getCurrentTime
  if isValid now
    then Surface.saveGPUTexture (root </> "assets" </> "saves" </> now <.> "png") (app^.easel.canvas.surface.texture) id
    else liftIO . putStrLn $ "Invalid file name: " ++ (root </> "assets" </> now <.> "png")
  return app


-- | Writes a pixel on the canvas if `p` is inside it
-- drawPixelOnCanvas :: App os -> V2 Float -> Pixel -> AppT os (Either String ())
-- drawPixelOnCanvas app p colour = Render.writePixel (floor <$> toCanvasCoords app p) colour (app^.easel.canvas.texture)
drawPixelOnCanvas :: App os -> V2 Float -> Pixel -> AppT os (App os)
drawPixelOnCanvas app p colour = return (app & easel.canvas.surface.pixels %~ Surface.writeCPUPixel (floor <$> toCanvasCoords app p) colour)


-- |
drawWithBrush :: App os -> V2 Float -> AppT os (App os) -- (Either String ())
drawWithBrush app pos = drawPixelOnCanvas app pos (app^.easel.brush.colour)


-- |
erasePixel :: App os -> V2 Float -> AppT os (App os) -- (Either String ())
erasePixel app pos = drawPixelOnCanvas app pos (app^.easel.canvas.colour)


-- |
-- TODO | - Rename
--        -
sampleColour :: App os -> V2 Float -> AppT os (App os)
sampleColour app pos = do
  -- liftIO $ do
  --   cursorDown 3
  --   print coords
  --   print $ texture2DSizes (app^.easel.canvas.texture)
  --   cursorUp 5
  -- liftIO $ putStrLn "\n\n\n\n\n"
  -- mcolour <- Render.pixelAt (coords) (app^.easel.canvas.texture)
  -- liftIO $ print mcolour
  -- Render.readPixels (V2 5 5) (V2 8 8) (app^.easel.canvas.texture)
  -- liftIO $ print mcolour
  -- return $ maybe (app) (\c -> app & easel.brush.colour .~ toHexColour c) mcolour
  return $ maybe app (\new -> app & easel.brush.colour .~ new) (Surface.readCPUPixel coords (app^.easel.canvas.surface.pixels))
  -- return app
  where
    coords = floor <$> toCanvasCoords app pos

-- Event plumbing ------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Add modifiers, scancode (?)
--        - KeyDown
--        - KeyUp
--        - FileChange
setupEvents :: Window os RGBAFloat Depth -> AppT os (TChan AppEvent)
setupEvents win = do
  ch <- liftIO newTChanIO

  GLFW.setDropCallback        win (Just $ \paths            -> putEvent ch $ FileDrop paths)
  GLFW.setScrollCallback      win (Just $ \sx sy            -> putEvent ch $ MouseScroll (V2 sx sy))
  GLFW.setCursorPosCallback   win (Just $ \mx my            -> putEvent ch $ MouseMotion (V2 mx my))
  GLFW.setMouseButtonCallback win (Just $ \b bstate mods    -> putEvent ch $ makeMouseEvent b bstate mods)
  GLFW.setKeyCallback         win (Just $ \k sc kstate mods -> putEvent ch $ makeKeyEvent k sc kstate mods)
  GLFW.setWindowCloseCallback win (Just $ putEvent ch $ WindowClosing) -- TODO: Invoke directly (?)
  -- GLFW.setCursorEnterCallback win (Just $ \cstate -> _)
  -- (CursorState'InWindow, CursorState'NotInWindow)
  -- setClipboardString, getClipboardString
  return ch
  where
    makeMouseEvent b MouseButtonState'Pressed  _ = MouseDown b
    makeMouseEvent b MouseButtonState'Released _ = MouseUp b

    makeKeyEvent k _ (KeyState'Pressed)   mods = KeyDown k
    makeKeyEvent k _ (KeyState'Released)  mods = KeyUp k
    makeKeyEvent k _ (KeyState'Repeating) mods = KeyRepeat k
    
    putEvent :: TChan AppEvent -> AppEvent -> IO ()
    putEvent ch = liftIO . atomically . writeTChan ch


-- |
-- TODO | - How do we deal with repeat events (does it even matter)?
processEvents :: App os -> AppT os (App os)
processEvents app = do
  events <- liftIO $ whileJust (atomically . tryReadTChan $ app^.input.inputChannel) return
  sz     <- getFrameBufferSize (app^.window)
  flip St.execStateT app $ do
    input.size .= sz
    viewport   .= ViewPort (V2 0 0) sz
    mapM set events
  where
    -- TODO: Clean up on aisle 7
    set :: AppEvent -> St.StateT (App os) (ContextT GLFW.Handle os IO) ()
    set e = do
      s  <- St.get
      s' <- lift $ onevent s e
      St.put s'

-- Events --------------------------------------------------------------------------------------------------------------------------------------------

-- |
onclosing :: App os -> AppT os (App os)
onclosing app = liftIO $ do
  putStrLn "\n\n\nI'm leaving"
  return app


-- |
onkeydown :: App os -> Key -> AppT os (App os)
onkeydown app k = case k of
  Key'Space  -> clearCanvas app
  Key'Escape -> quitApplication app
  Key'S      -> saveCanvas app
  _          -> return app


-- |
onmotion :: App os -> V2 Float -> AppT os (App os)
onmotion app pos
  | app^.hasMousebutton MouseButton'1 = drawWithBrush app pos
  | app^.hasMousebutton MouseButton'2 = erasePixel app pos
  | app^.hasMousebutton MouseButton'3 = sampleColour app pos
  | otherwise                                       = return app


-- |
onmousedown :: App os -> MouseButton -> AppT os (App os)
onmousedown app b = case b of
  MouseButton'1 -> drawWithBrush app (app^.input.mouse.cursor)
  MouseButton'2 -> erasePixel    app (app^.input.mouse.cursor)
  MouseButton'3 -> sampleColour  app (app^.input.mouse.cursor)
  _             -> return app


-- |
onscroll :: App os -> V2 Double -> AppT os (App os)
onscroll app (V2 dx dy) = flip St.execStateT app $ do
  -- TODO: Merge brush and palette
  easel.palette %= stepBy (floor dy)
  new <- St.get -- TODO: Refactor
  easel.brush.colour .= (current $ new^.easel.palette)


-- |
onevent :: App os -> AppEvent -> AppT os (App os)
onevent app e = case e of
  MouseMotion pos' -> let pos = realToFrac <$> pos' in onmotion (app & input.mouse.cursor .~ pos) pos
  KeyDown k        -> onkeydown   (app & hasKey k .~ True) k
  KeyUp k          -> return      (app & hasKey k .~ False)
  MouseDown b      -> onmousedown (app & hasMousebutton b .~ True) b
  MouseUp b        -> return      (app & hasMousebutton b .~ False)
  MouseScroll δ    -> onscroll    (app & input.scroll %~ (+ δ)) δ
  WindowClosing    -> onclosing   (app)
  _ -> return app

-- Linear algebra (coordinate systems) ---------------------------------------------------------------------------------------------------------------

-- TODO | - Factor out, refactor
--        - Type safety (use types to distinguish different coordinate systems)

-- |
transformInverse :: App os -> M44 Float
transformInverse = inv44 . modelProj


-- |
modelProj app = (app^.uniforms.singular (matrices.values.ix 0)) !*! (app^.uniforms.singular (matrices.values.ix 1))


-- | Convert screen coordinates to canvas coordinates
-- TODO | - This function has to be a lot cleverer
--        - Take viewport into account
--        - Factor out
--        - All 2D (?)
--        - Should W be 0 or 1
toCanvasCoords :: App os -> V2 Float -> V2 Float
toCanvasCoords app (V2 px py) = dropZW $ screenToNormalised app !*! fmap (fmap realToFrac) (transformInverse app) !* V4 px py 0 1
-- toCanvasCoords app p = dropZW $ (fmap realToFrac <$> transformInverse app) !* to4D 0 (to3D 0 p)
--fmap fromIntegral . (\(V2 mx my) -> V2 (mx) (app^.canvas.size.y-my) + origin app) . fmap floor


-- |
-- TODO | - Make sure this is correct
--        -
screenToNormalised :: App os -> M44 Float
screenToNormalised app = translated !*! scaled (V4 (sx) (-sy) 1 1)
  where
    (V2 sx sy) = (2*) . recip . fromIntegral <$> app^.input.size
    translated = identity & translation .~ V3 0 (fromIntegral $ app^.easel.canvas.surface.size.y) 0


-- toTextureCoords :: App os -> V2 Double -> V2 Int


-- |
-- origin :: App os -> V2 Int
-- origin app = (`div` 2) <$> (app^.easel.canvas.size - app^.easel.canvas.size)

-- Colour theory -------------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Factor out, refactor
--        - Type safety (use types to distinguish different colour systems)

-- |
-- TODO | - Rename
--        - Polymorphic
toFloatColour :: V3 Word8 -> V3 Float
toFloatColour = fmap ((/0xFF) . fromIntegral)


-- |
-- TODO | - Rename
--        - Polymorphic
toHexColour :: V3 Float -> V3 Word8
toHexColour = fmap (floor . (*0xFF))

-- Debugging and logging ----------------------------------------------------------------------------------------------------------------------------

-- |
alignedCoords :: String -> V2 Float -> IO ()
alignedCoords label coord = let (V2 x y) = round <$> coord in printf "%-10s% 5d % 5d\n" (label ++ ":") (x :: Int) y


-- |
debugScreen :: App os -> AppT os ()
debugScreen app = liftIO $ do
  let sm = app^.input.mouse.cursor
      cm = toCanvasCoords app sm
  alignedCoords "Screen" sm
  alignedCoords "Canvas" cm
  printf "%s|%s|%s\n" (btn MouseButton'1) (btn MouseButton'3) (btn MouseButton'2)
  cursorUp 3
  where
    btn b = if app^.hasMousebutton b then "x" else " "

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Rename
run :: IO (Either String ())
run = runEitherT $ do
  lift $ putStrLn "Running"
  config <- EitherT . loadConfig $ root </> "assets/settings/config.json"
  lift $ putStrLn "Done loading config"
  EitherT $ Right <$> runApplication config


-- |
-- TODO | - Rename
--        - Separate initialise and execute functions (?)
--        - Communicate with the 'outside', return some message or accept a queue for messages (?)
runApplication :: AppConfig -> IO ()
runApplication config = do
  putStrLn "Running application"  
  runContextT GLFW.defaultHandleConfig $ do
    liftIO $ putStrLn "Creating window"
    win <- newWindow (WindowFormatColorDepth RGBA8 Depth32) $ WindowConfig {
                                                                  configWidth   = config^.windowSize.x
                                                                , configHeight  = config^.windowSize.y
                                                                , configTitle   = "Pixels"
                                                                , configMonitor = Nothing
                                                                , configHints   = []
                                                                , configSwapInterval = Nothing }
    liftIO $ putStrLn "Creating canvas"
    cvs <- Render.newCanvas (config^.canvasSize) (config^.canvasColour)
    liftIO $ putStrLn "Creating brush"
    br  <- Render.newBrush  (V2 0 0)             (config^.brushColour)
    liftIO $ putStrLn "Creating uniforms"
    us  <- Render.newUniforms config

    -- TODO: How do you use the same shader for different topologies?
    liftIO $ putStrLn "Compiling shader"
    shade <- compileShader $ do
      canvasFragments <- Render.texturedShader
      drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBAFloat)) canvasFragments
      pointFragments  <- Render.colorShader
      drawWindowColor (\_ -> (win, ContextColorOption NoBlending (pure True) :: ContextColorOption RGBAFloat)) pointFragments
      -- ContextColorOption NoBlending (pure True :: ColorMask RGBFloat)
    
    --forall ctx a b c h w os f m. (ContextHandler ctx, MonadAsyncException m, MonadIO m, BufferFormat b, ColorSampleable c, BufferColor (Color c (ColorElement c)) h ~ b, h ~ HostFormat b) =>
    -- Texture2D os (Format c) -> Level -> StartPos2 -> Size2 -> (a -> h -> ContextT ctx os m a) -> a -> ContextT ctx os m a
    -- instance HasTexture (Canvas os0) (Texture2D os0 (Format RGBFloat)
    --          HasTexture (Canvas os) (Texture2D os1 (Format RGBFloat)))

    -- mcolour <- readTexture2D (cvs^.texture) 0 (V2 4 8) (V2 4 4) (\ps c -> return (c:ps :: [V3 Float])) []
    -- liftIO $ print mcolour

    -- Events
    liftIO $ putStrLn "Setting up channel"
    channel <- setupEvents win

    -- Off we go
    liftIO $ putStrLn "Entering mainloop"
    mainloop $ App {
      fRasterOptions = (FrontAndBack, ViewPort (V2 0 0) (config^.windowSize), DepthRange 0 1),
      fShader        = shade,
      fUniforms      = us,
      fEasel         = Easel {
                         fCanvas  = cvs,
                         fBrush   = br,
                         fPalette = let (Just xs) = newCircleList (config^.easelPalette) in xs }, -- TODO: Don't assume
      fWindow        = win,
      fInput         = initialInput channel (config^.windowSize) -- (InputChannel channel)
    }


-- |
tick :: App os -> AppT os (App os)
tick app' = do
  -- Read input
  app <- processEvents app'

  -- Write uniforms
  writeBuffer (app^.uniforms.matrices.buffer) 0 (app^.uniforms.matrices.values)
  writeBuffer (app^.uniforms.vectors.buffer)  0 (app^.uniforms.vectors.values)
  writeBuffer (app^.uniforms.scalars.buffer)  0 (app^.uniforms.scalars.values)

  -- Write attributes
  -- liftIO $ putStrLn "Writing position buffer"
  -- writeBuffer (app^.easel.brush.positionBuffer) 0 [let (V2 x y) = app^.input.mouse.cursor
  --                                                      pos      = screenToNormalised app !* (V4 x y 0 1)
  --                                                      pixel    = app^.easel.brush.colour --fromIntegral <$> head (app^.easel.palette)
  --                                                  in (pos, V3 0 0 0)]

  --
  Surface.writeCPUTextureToGPU (app^.easel.canvas.surface.pixels) (app^.easel.canvas.surface.texture)

  --
  debugScreen app
  return app


-- |
mainloop :: App os -> AppT os ()
mainloop app' = do
  Render.render app'
  swapWindowBuffers $ app'^.window
  app <- tick app'
  closeRequested <- fromMaybe True <$> GLFW.windowShouldClose (app^.window)
  unless closeRequested $ mainloop app