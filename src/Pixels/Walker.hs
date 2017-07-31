-- |
-- Module      : Pixels.Walker
-- Description : Little toy module to experiment with random paths
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created August 27 2016

-- TODO | -
--        -y

-- SPEC | -
--        -



-- API -------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Walker where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import System.Random
import Linear.V2
import Linear.V3

-- Functions -------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Relax the type restrictions (?)
path :: (Random f, RealFloat f) => StdGen -> V2 f -> [V2 f]
path g _ = let (x, g')  = random g
               (y, g'') = random g'
           in V2 x y : path g'' (V2 x y)
