-- |
-- Module      : Pixels.Colour
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : 
-- Portability : 
-- 

-- TODO | - 
--        - 

-- SPEC | -
--        -

-- GHC Pragmas -----------------------------------------------------------------------------------------------------------------------------

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Colour where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

-- *
import Data.Word (Word8)

-- *
import Linear

-- *
import Pixels.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- Colour theory ---------------------------------------------------------------------------------------------------------------------------

-- TODO | - Factor out, refactor
--        - Type safety (use types to distinguish different colour systems)

-- |
-- TODO | - Rename
--        - Polymorphic
toFloatColour :: Functor f => f Word8 -> f Float
toFloatColour = fmap ((/0xFF) . fromIntegral)


-- |
-- TODO | - Rename
--        - Polymorphic
toHexColour :: Functor f => f Float -> f Word8
toHexColour = fmap (floor . (*0xFF))