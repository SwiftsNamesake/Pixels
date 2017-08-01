
--
-- Pixels.Render.Utils
-- Rendering utilities...
--
-- Jonatan H Sundqvist
-- September 18 2016
--

-- TODO | - 
--        - 

-- SPEC | -
--        -



-- GHC Pragmas ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}



-- API -----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Render.Utils where



-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import Data.Word

import Linear (V2(..))

import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Foreign.Ptr        as Ptr

import           Data.Array.Repa ((:.)(..)) -- Weirdest syntax ever
import qualified Data.Array.Repa as R

import Leibniz.Constants (π)

import Pixels.Types

-- Functions -----------------------------------------------------------------------------------------------------------------------------------------

