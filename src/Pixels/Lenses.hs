
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

-- TODO: Prevent clashes (use a more sophisticated lens generator)
-- runQ . sequence $ map makeLenses [''AppState, ''Paths , ''Mesh , ''Debug , ''Input , ''Mouse , ''Settings , ''Graphics , ''Camera]
makeLenses ''AppState
makeLenses ''Paths
makeLenses ''Mesh
makeLenses ''Debug
makeLenses ''Input
makeLenses ''Mouse
makeLenses ''Settings
makeLenses ''Graphics
makeLenses ''Camera
makeLenses ''CPUResources