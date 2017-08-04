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

-- import Control.Monad (forM_, forM, mapM_, mapM)
import Control.Lens  (makeLensesWith, abbreviatedFields, ix, Simple, Lens, Traversal)
import Linear        (M44)

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

makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''Canvas
makeLensesWith abbreviatedFields ''Brush
makeLensesWith abbreviatedFields ''AttributeData
makeLensesWith abbreviatedFields ''UniformData
makeLensesWith abbreviatedFields ''UniformBlock
makeLensesWith abbreviatedFields ''Input
makeLensesWith abbreviatedFields ''Mouse
makeLensesWith abbreviatedFields ''ShaderEnvironment
makeLensesWith abbreviatedFields ''TextureEnvironment
makeLensesWith abbreviatedFields ''Easel
makeLensesWith abbreviatedFields ''AppConfig

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- projection :: Simple Traversal (UniformBlockMatrix os) (M44 Float) -- Simple Traversal (UniformBlockMatrix os) (M44 Float)
-- projection = (matrices :: Simple Lens (UniformBlockMatrix os)).(values).ix 0


-- |
--modelview :: Simple Traversal (UniformBlockMatrix os) (M44 Float) -- Simple Traversal (UniformBlockMatrix os) (M44 Float)
-- modelview = matrices.values.ix 1