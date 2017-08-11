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

--- GHC directives -----------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}

-- API -----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Trinkets (
  -- * Console IO
  flushed, putStr, putStrLn, putChar, print,

  -- * Stuff
  pass, execute, sequencePairs,
  hasExtension,
  -- flatten, asList,
  indentedStrLn,

  -- * Linear algebra
  to2D, to3D, to4D, dropY, dropZ, dropZW,
  axisRotation, texcoord
) where

--- We'll need these ---------------------------------------------------------------------------------------------------------------------------------

import           Prelude hiding (putStrLn, print, putStr, putChar)
import qualified Prelude as P

import Data.Foldable as F
import Data.Aeson    as JSON

import Linear (V0(..), V1(..), V2(..), V3(..), V4(..), M33, Epsilon, axisAngle, fromQuaternion)
-- import Text.Printf

import Control.Monad.Trans.Either
import Control.Monad (when, void)
import Control.Lens
import Control.Applicative (liftA2)

import System.FilePath     (takeExtension) --
import System.IO           (stdout, hFlush) --
import System.Console.ANSI (setSGR, SGR(..), Color(..), ConsoleLayer(..), ColorIntensity(Vivid))

import Pixels.Types

-- Definitions ---------------------------------------------------------------------------------------------------------------------------------------

-- Experiments ---------------------------------------------------------------------------------------------------------------------------------------

-- IO helpers ----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (?)
flushed :: IO a -> IO a
flushed action = do
  v <- action
  hFlush stdout
  return v


-- TODO | - Unicode
--        -

putStrLn :: String -> IO ()
putStrLn = flushed . P.putStrLn

print :: Show s => s -> IO ()
print    = flushed . P.print

putStr :: String -> IO ()
putStr   = flushed . P.putStr

putChar :: Char -> IO ()
putChar  = flushed . P.putChar

-- Paths ---------------------------------------------------------------------------------------------------------------------------------------------

-- | Does the filepath have the given extension?
hasExtension :: String -> FilePath -> Bool
hasExtension ext fn = takeExtension fn == ext -- This should really be defined in the filepaths package....

------------------------------------------------------------------------------------------------------------------------------------------------------

-- Logging -------------------------------------------------------------------------------------------------------------------------------------------

-- | TODO: LoggerT


-- |
indent :: Int -> String -> String
indent i s = replicate (i*2) ' ' ++ s


-- |
indentedStrLn :: Int -> String -> IO ()
indentedStrLn level s = putStrLn $ indent level s


-- |
putWithSGR :: [Either [SGR] String] -> IO ()
putWithSGR = void . mapM (either setSGR putStr)


-- |
-- shouldLog :: Debug -> LogLevel -> Bool
-- shouldLog db logLvl = (logLvl) >= (db^.logLevel)


-- |
-- TODO | - Colouring breaks when using the Git-bash console
--        - Rendering log messages
--        - Indent multiple lines (?)
--        - Break up filtering (when) and formatting (putWithSGR)
-- logStr :: Debug -> LogLevel -> Int -> String -> IO ()
-- logStr db logLvl indentLvl s = when (shouldLog db logLvl) $ putWithSGR [Right $ indent indentLvl "[",
--                                                                         Left  $ [SetColor Foreground Vivid fg],
--                                                                         Right $ label,
--                                                                         Left  $ [SetColor Foreground Vivid White],
--                                                                         Right $ "] " ++ message]
--   where
--     message = let (l:ines) = lines s in unlines $ l : map (indent indentLvl) ines -- The first line has already been indented
--     (fg, label) = case logLvl of
--       InfoLevel     -> (Green,  "Info")
--       WarningLevel  -> (Yellow, "Warning")
--       CriticalLevel -> (Red,    "Critical")

--- Control ------------------------------------------------------------------------------------------------------------------------------------------

-- |
pass :: Monad m => m ()
pass = return ()


-- |
execute :: Monad m => EitherT a m b -> m ()
execute = void . runEitherT


-- | Like sequence, but for tuples where the first item is pure
-- TODO | - Rename (?)
sequencePairs :: Monad m => [(a, m b)] -> m [(a,b)]
sequencePairs ps = let invert (a, b) = b >>= return . (a,) in sequence . map invert $ ps

-- Coordinate systems --------------------------------------------------------------------------------------------------------------------------------

-- |
snapToNearest :: (Num (v f), RealFrac f, Applicative v) => v f -> v f -> v f -> v f
snapToNearest resolution point origin = liftA2 snap' resolution (point - origin) + origin
  where
    snap' res v = res * (fromIntegral . round $ v/res) -- Snaps in one dimensions


-- TODO | - Clear up the naming confusion (some drop, some add)
--        -


-- | Drops the Z coordinate
to2D :: f -> V1 f -> V2 f
to2D y (V1 x) = V2 x y


-- | Adds the Z coordinate
to3D :: f -> V2 f -> V3 f
to3D z (V2 x y) = V3 x y z


-- | Adds the W coordinate
to4D :: f -> V3 f -> V4 f
to4D w (V3 x y z) = V4 x y z w


-- |
dropY :: V2 f -> V1 f
dropY (V2 x _) = V1 x


-- |
dropZ :: V3 f -> V2 f
dropZ (V3 x y _) = V2 x y


-- |
dropW :: V4 f -> V3 f
dropW (V4 x y z _) = V3 x y z


-- |
dropZW :: V4 f -> V2 f
dropZW (V4 x y _ _) = V2 x y


-- | Creates a rotation matrix from an axis and an angle (in radians)
axisRotation :: (Floating f, Epsilon f) => V3 f -> f -> M33 f
axisRotation axis θ = fromQuaternion $ axisAngle axis θ


-- | Finds the right texture coordinate for a vertex (in a rectangle)
-- TODO | - Move
--        - Rename
texcoord :: Fractional f => V3 f -> V2 f
texcoord = fmap ((*0.5) . (+1) . signum) . dropZ

-- Math ----------------------------------------------------------------------------------------------------------------------------------------------

-- Vectors -------------------------------------------------------------------------------------------------------------------------------------------

-- Map operations ------------------------------------------------------------------------------------------------------------------------------------

-- | Perform some action with the indexed item if it is found, otherwise use the default value
-- TODO: Find out if the Lens or Containers packages provide a similar function
