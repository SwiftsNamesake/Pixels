-- |
-- Module      : Pixels.Render.Core
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

module Pixels.Render.Core where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

-- *
import Graphics.GPipe

-- * Mine
import Geometry.Sculptor.Shapes hiding (Face)

-- * Internal
import Pixels.Trinkets
import Pixels.Algebra
import Pixels.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- Geometry --------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Move
--        -

-- |
newAttributeBuffer :: BufferFormat b => [HostFormat b] -> AppT os (Buffer os b)
newAttributeBuffer d = do
  buffer <- newBuffer (length d)
  writeBuffer buffer 0 d
  return buffer


-- |
newQuadXY :: V2 Float -> (V3 Float -> V2 Float) -> AppT os (Buffer os VertexAttributes)
newQuadXY (V2 dx dy) texcoord = newAttributeBuffer vertices
  where
    makeVertex v = (to4D 1 v, texcoord v)
    vertices = map makeVertex (concat . triangles $ planeXY V3 dx dy)