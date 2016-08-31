-- |
-- Module      : Pixels.Trinkets
-- Description : Assorted utilities
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created August 27 2016

-- TODO | - 
--        - 

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC directives
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Trinkets where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStrLn, print, putStr, putChar)
import qualified Prelude as P

import Control.Monad (when)
import Control.Lens

import System.Random
import System.IO (stdout, hFlush) --

import Linear.V2
import Linear.V3

import Cartesian.Types (Vector(..))

import Pixels.Types
import Pixels.Lenses



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Data
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- IO helpers ----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (?)
flushed :: IO a -> IO a
flushed action = do
  v <- action
  hFlush stdout
  return v


-- TODO: Unicode

putStrLn :: String -> IO ()
putStrLn = flushed . P.putStrLn

print :: Show s => s -> IO ()
print    = flushed . P.print

putStr :: String -> IO ()
putStr   = flushed . P.putStr

putChar :: Char -> IO ()
putChar  = flushed . P.putChar

-- Logging -------------------------------------------------------------------------------------------------------------------------------------------

-- |
indentedStrLn :: Int -> String -> IO ()
indentedStrLn level s = putStrLn $ replicate (level*2) ' ' ++ s


-- |
-- TODO: Rendering log messages
logStr :: Debug -> LogLevel -> Int -> String -> IO ()
logStr db level indent s = when (level >= db^.logLevel) (indentedStrLn indent s)

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
pass :: Monad m => m ()
pass = return ()

-- Math ----------------------------------------------------------------------------------------------------------------------------------------------

-- Vectors -------------------------------------------------------------------------------------------------------------------------------------------

flatten :: (Vector v, Num f) => [v f] -> [f]
flatten = concatMap asList

asList :: (Vector v, Num f) => v f -> [f]
asList = reverse . vfold (flip (:)) []

-- Map operations ------------------------------------------------------------------------------------------------------------------------------------

-- | Perform some action with the indexed item if it is found, otherwise use the default value
-- TODO: Find out if the Lens or Containers packages provide a similar function
