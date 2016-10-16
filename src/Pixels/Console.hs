
--
-- Pixels.Console
-- Windowless console without all the legacy cruft
--
-- Jonatan H Sundqvist
-- September 14 2016
--

-- TODO | - Input
--        - Functional reactive programming integration
--        - Separate the 'console' (eg. pure text rendering) from the 'command prompt'
--        - [Lens-based] interface that hides implementation (eg. setting the 'dirty' flag)

-- SPEC | - For simplicity - and to avoid bikeshedding - I have decided to use reasonable defaults for the types of Fonts, Indices, Buffers
--          and encodings. I may reconsider that choice once I have a working implementation of this module.
--          


------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE OverloadedRecordFields #-} -- Some day...
{-# LANGUAGE DuplicateRecordFields  #-} -- PRAISE BE THE LORD FOR THIS BLESSING
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Console where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll neeed these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStr, putStrLn, putChar)
import qualified Prelude as P

import Linear.V2

import Control.Lens (makeFields, makeLensesWith, abbreviatedFields, Getter, Setter, Lens, (.~), (^.))
import Control.Applicative ((<$>), (<*>))

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Word

import Data.Colour
import Data.Colour.SRGB

import Graphics.Rendering.OpenGL as GL hiding (projection, perspective, Line, position, ortho, viewport, RGB)

-- import Interpolate
import Cartesian.Core (BoundingBox(..), left, right, top, corner, size, bottom)

import Pixels.Render.Utils



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- Types ---------------------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Proper anti-aliased fonts with full Unicode support (mono-spaced)
--          cf. https://medium.com/@evanwallace/easy-scalable-text-rendering-on-the-gpu-c3f4d782c5ac#.ct9d4tfvp
--
--        - [Viewport] Use Natural type, Cartesian.BoundingBox (?)
--        - [Settings] I'll add more options eventually, I'm sure
--        - [Console]  I'll keep this structure for now, but we'll probably have to change the buffer type for efficiency
--
--        - Should I include metadata with the fonts (?)

-- |
-- TODO: Graphics primitives
type Texture = GL.TextureObject -- TODO: Use frame buffer object (optional?) (?)
type RGBA w  = (w, w, w, w)


-- |
type Viewport = BoundingBox (V2 Int)


-- |
data FontMap = FontMap {
  fFindGlyph :: (Char -> Maybe Viewport),
  fGlyphSize :: V2 Int,
  fTexture   :: Texture
}


-- |
data Font = Font {
  fFamily :: (),
  fSize   :: V2 Int
}


-- |
data Canvas = Canvas {
  fTexture :: Texture,
  fDirty   :: Bool
}


-- |
data Settings = Settings {
  fFont       :: Font,
  fBackground :: RGBA Word8,
  fResizable  :: Bool,
  fAutoscroll :: Bool
}


-- |
data Cursor = Cursor {
  fRow    :: Int,
  fColumn :: Int
}


-- |
data Console  = Console {
  fBuffer   :: Text,
  fCursor   :: Cursor,
  fViewport :: Viewport,
  fSettings :: Settings
}


-- |
-- TODO: Lenses (✓)
-- makeLensesWith (defaultFieldRules & lensField .~ yourCustomFieldNamingConvention)
makeLensesWith abbreviatedFields ''Colour
makeLensesWith abbreviatedFields ''Settings
makeLensesWith abbreviatedFields ''Cursor
makeLensesWith abbreviatedFields ''Console


-- TODO | - Font introspection, adding new ones
--        - Saving state, resuming sessions
--        - Formatting (cf. interpolation)
--        - Events, hooks, FRP, plugins
--        - Custom backends (?)
--        - Tabs
--        - Multiple viewports (for the same buffer)

-- Initialisation ------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Maybe add some settings (?)
new :: IO Console
new = return $ Console { fBuffer   = "",
                         fCursor   = Cursor 0 0,
                         fViewport = BoundingBox { cornerOf = V2 0 0, sizeOf = V2 120 40 },
                         fSettings = defaultSettings }


-- |
defaultSettings :: Settings
defaultSettings = Settings { fBackground = (0, 0, 0, 0),
                             fResizable  = False,
                             fAutoscroll = False,
                             fFont       = Font { fSize = 13, fFamily = () } }

-- Rendering -----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Reconsider name (eg. renderBuffer[Of])
render :: Console -> IO Texture
render self = do
  tex <- createRepaTexture (self^.viewport.size) (\_ -> self^.settings.background)
  -- TODO: Render the actual text (:/)
  return tex


-- | 
-- refresh :: Console -> IO ()
-- refresh self = _

-- Text rendering ------------------------------------------------------------------------------------------------------------------------------------

-- |
-- createFontMap :: Font -> IO FontMap
-- createFontMap = ()

-- Output --------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- putChar  :: Console -> Char -> IO ()
-- putStr   :: Console -> Text -> IO ()
-- putStrLn :: Console -> Text -> IO ()

-- print :: Show a => Console -> a -> IO ()
-- markupLn, markup, formatLn

-- Input ---------------------------------------------------------------------------------------------------------------------------------------------
