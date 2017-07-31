-- |
-- Module      : Pixels.Lenses
-- Description : Auto-generated lenses
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



-- GHC Pragmas ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

--- API ----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Lenses where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import Control.Lens
import Pixels.Types

-- Definitions ---------------------------------------------------------------------------------------------------------------------------------------

makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''Canvas
makeLensesWith abbreviatedFields ''UniformData
makeLensesWith abbreviatedFields ''UniformBlock
makeLensesWith abbreviatedFields ''ShaderEnvironment
makeLensesWith abbreviatedFields ''TextureEnvironment