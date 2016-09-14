
--
-- Pixels.Console
-- Windowless console without all the legacy cruft
--
-- Jonatan H Sundqvist
-- September 14 2016
--

-- TODO | - 
--        -

-- SPEC | - For simplicity - and to avoid bikeshedding - I have decided to use reasonable defaults for the types of Fonts, Indices, Buffers
--          and encodings. I may reconsider that choice once I have a working implementation of this module.
--          
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Console where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll neeed these
------------------------------------------------------------------------------------------------------------------------------------------------------

import           Prelude (Int, Bool)
import qualified Prelude

import           Control.Lens
import qualified Data.Text as T
import           Data.Word

-- import Interpolate

------------------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- Types ---------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Lenses
type Font = () -- Shimmy

data Colour   = Colour   { red :: Word8, green :: Word8, blue :: Word8, alpha :: Word8 }
data Viewport = Viewport { left :: Int, right :: Int, top :: Int, bottom :: Int }                      -- TODO: Use Natural type (?)
data Settings = Settings { font :: Font, background :: Colour, resizable :: Bool, autoscroll :: Bool } -- TODO: I'll add more options eventually, I'm sure
data Cursor   = Cursor   { row :: Int, column :: Int }

data Console = Console { buffer   :: T.Text,
                         cursor   :: Cursor,
                         viewport :: Viewport,
                         settings :: Settings }


makeLenses ''Colour
makeLenses ''Viewport
makeLenses ''Settings
makeLenses ''Cursor
makeLenses ''Console

-- TODO: Saving state, resuming sessions
-- TODO: Formatting (cf. interpolation)
-- TODO: Events, hooks, plugins
-- TODO: Custom backends (?)

-- Functions -----------------------------------------------------------------------------------------------------------------------------------------
