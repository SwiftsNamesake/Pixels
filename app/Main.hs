-- |
-- Module      : Main
-- Description : This is where the pixels happen
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

module Main where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import qualified Pixels.Interaction as Interaction

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do
  putStrLn "Oh ffs"
  print =<< Interaction.run
  return ()