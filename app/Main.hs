
--
-- Main.hs
-- This is where the pixels happen
--
-- Jonatan H Sundqvist
-- date
--

-- TODO | -
--        -

-- SPEC | -
--        -


module Main where

import qualified Pixels.Render as Render
import qualified Pixels.Walker as Walker

main :: IO ()
main = do
  --
  putStrLn "Testing random walks"

  --
  Render.main