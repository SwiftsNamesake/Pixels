--
-- Pixels.Lenses
-- Lenses...
--
-- Jonatan H Sundqvist
-- August 20 2016
--

-- TODO | -
--        -

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC directives
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Lenses where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Control.Lens

import Pixels.Types



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Lenses
------------------------------------------------------------------------------------------------------------------------------------------------------

-- type Mesh  = (GL.BufferObject, GL.BufferObject, Int)
-- type World = ()

makeLenses ''AppState 
makeLenses ''Input 
makeLenses ''Mouse 
makeLenses ''Settings
-- makeLenses ''Camera