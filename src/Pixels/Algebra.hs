-- |
-- Module      : Pixels.Algebra
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

module Pixels.Algebra where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

-- *
import Control.Applicative

-- *
import Linear

-- *
import Pixels.Types

-- Definitions -----------------------------------------------------------------------------------------------------------------------------


-- | Converts a 2D point to an index, given a size
toLinearIndex :: V2 Int -> V2 Int -> Int
toLinearIndex (V2 px py) (V2 dx _) = py*dx + px


-- | Converts an index to a 2D point, given a size
fromLinearIndex :: Int -> V2 Int -> V2 Int
fromLinearIndex i (V2 dx _) = let (py, px) = divMod i dx in V2 px py


-- | Is `p` (point) inside the rectangle defined by `sz` (size)? The upper bounds are exclusive.
-- TODO | - Factor out
--        - Rename (?)
fits :: (Num n, Ord n, Applicative v, Foldable v) => v n -> v n -> Bool
fits p sz = all id $ (\v hi -> 0 <= v && v < hi) <$> p <*> sz -- (0 <= x && x < dx) && (0 <= y && y < dy)


-- |
guardPoint :: (Num n, Ord n, Applicative v, Foldable v) => v n -> v n -> (v n -> a) -> Maybe a
guardPoint p sz f = if fits p sz then Just (f p) else Nothing