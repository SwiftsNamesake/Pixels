
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
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}



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
makeLensesWith abbreviatedFields ''AppState
makeLensesWith abbreviatedFields ''Paths
makeLensesWith abbreviatedFields ''Mesh
makeLensesWith abbreviatedFields ''Debug
makeLensesWith abbreviatedFields ''Input
makeLensesWith abbreviatedFields ''Mouse
makeLensesWith abbreviatedFields ''Settings
makeLensesWith abbreviatedFields ''Graphics
makeLensesWith abbreviatedFields ''Camera
makeLensesWith abbreviatedFields ''CPUResources