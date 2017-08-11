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
{-# LANGUAGE RankNTypes #-}

--- API ----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Lenses where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import Data.Set (Set)

-- import Control.Monad (forM_, forM, mapM_, mapM)
import Control.Lens (makeLensesWith, abbreviatedFields, ix, _2, contains, Simple, Lens, Traversal, Identity, IxValue)
import Linear       (M44)
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW.Input (Key(..), MouseButton(..))

import Pixels.Types

-- Definitions ---------------------------------------------------------------------------------------------------------------------------------------

-- mapM_ (makeLensesWith abbreviatedFields) [''App,
--                                          ''Canvas,
--                                          ''Brush,
--                                          ''UniformData,
--                                          ''UniformBlock,
--                                          ''Input,
--                                          ''Mouse,
--                                          ''ShaderEnvironment,
--                                          ''TextureEnvironment,
--                                          ''Easel,
--                                          ''AppConfig]

makeLensesWith abbreviatedFields ''AppConfig

makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''Canvas
makeLensesWith abbreviatedFields ''Surface
makeLensesWith abbreviatedFields ''Brush

makeLensesWith abbreviatedFields ''ShaderEnvironment
makeLensesWith abbreviatedFields ''AttributeData
makeLensesWith abbreviatedFields ''UniformData
makeLensesWith abbreviatedFields ''UniformBlock
makeLensesWith abbreviatedFields ''TextureEnvironment

makeLensesWith abbreviatedFields ''Input
makeLensesWith abbreviatedFields ''Mouse
makeLensesWith abbreviatedFields ''Easel

makeLensesWith abbreviatedFields ''CPUTexture

-- Input ---------------------------------------------------------------------------------------------------------------------------------------------

mousebuttons :: Simple Lens (App os) (Set MouseButton)
mousebuttons = input.mouse.buttons

hasMousebutton :: MouseButton -> Simple Lens (App os) Bool
hasMousebutton b = input.mouse.buttons.contains b

keys :: Simple Lens (App os) (Set Key)
keys = input.keyboard

hasKey :: Key -> Simple Lens (App os) Bool
hasKey k = input.keyboard.contains k

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
viewport :: Simple Lens (App os) ViewPort
viewport = rasterOptions . _2


-- |
projection :: (IxValue [M44 Float] -> Identity (IxValue [M44 Float])) -> UniformData os -> Identity (UniformData os)
projection = matrices.values.ix 0


-- |
modelview :: (IxValue [M44 Float] -> Identity (IxValue [M44 Float])) -> UniformData os -> Identity (UniformData os)
modelview = matrices.values.ix 1

-- |
-- projection :: Simple Traversal (UniformBlockMatrix os) (M44 Float) -- Simple Traversal (UniformBlockMatrix os) (M44 Float)
-- projection = (matrices :: Simple Lens (UniformBlockMatrix os)).(values).ix 0


-- |
--modelview :: Simple Traversal (UniformBlockMatrix os) (M44 Float) -- Simple Traversal (UniformBlockMatrix os) (M44 Float)
-- modelview = matrices.values.ix 1