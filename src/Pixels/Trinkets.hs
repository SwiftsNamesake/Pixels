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
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Trinkets (flushed,
                        pass, execute, sequencePairs,
                        hasExtension,
                        putStr, putStrLn, putChar, print,
                        flatten, asList,
                        logStr, indentedStrLn) where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStrLn, print, putStr, putChar)
import qualified Prelude as P

import Data.Foldable as F

import Text.Printf

import Control.Monad.Trans.Either
import Control.Monad (when, void)
import Control.Lens
import Control.Applicative (liftA2)

import System.FilePath  ((</>), takeExtension, dropExtension, takeFileName) --
import System.Random
import System.IO (stdout, hFlush) --
import System.Console.ANSI

import Linear.V2
import Linear.V3

-- import Cartesian.Space.Types (Vector(..))

import Pixels.Types
import Pixels.Lenses



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Data
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- Experiments ---------------------------------------------------------------------------------------------------------------------------------------

-- How do you manipulate, componse, varargs functions...

-- class VarArg a where
--   apply :: VarArg c => (a -> b) -> [b] -> a -> c


-- instance VarArg String where
--   apply f bs a = f a : bs


-- instance VarArg b => VarArg (a -> b) where
--   apply f bs a = apply f (f a : bs)


-- reduce :: (VarArg a, VarArg c) => (a -> b) -> [b] -> c
-- reduce f bs a = apply f bs a

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

-- Paths ---------------------------------------------------------------------------------------------------------------------------------------------

-- | Does the filepath have the given extension?
hasExtension :: String -> FilePath -> Bool
hasExtension ext fn = takeExtension fn == ext -- This should really be defined in the filepaths package....

------------------------------------------------------------------------------------------------------------------------------------------------------



-- Logging -------------------------------------------------------------------------------------------------------------------------------------------

-- | TODO: LoggerT


-- |
indent :: Int -> String -> String
indent i s = (++ s) . flip replicate ' ' . (*2) $ i


-- |
indentedStrLn :: Int -> String -> IO ()
indentedStrLn level s = putStrLn $ indent level s


-- |
putWithSGR :: [Either [SGR] String] -> IO ()
putWithSGR = void . mapM (either setSGR putStr)


-- |
shouldLog :: Debug -> LogLevel -> Bool
shouldLog db logLvl = (logLvl) >= (db^.logLevel)


-- |
-- TODO: Colouring breaks when using the Git-bash console
-- TODO: Rendering log messages
-- TODO: Indent multiple lines (?)
-- TODO: Break up filtering (when) and formatting (putWithSGR)
logStr :: Debug -> LogLevel -> Int -> String -> IO ()
logStr db logLvl indentLvl s = when (shouldLog db logLvl) $ putWithSGR [Right $ indent indentLvl "[",
                                                                        Left  $ [SetColor Foreground Vivid fg],
                                                                        Right $ label,
                                                                        Left  $ [SetColor Foreground Vivid White],
                                                                        Right $ "] " ++ message ++ "\n"]
  where
    message = let (l:ines) = lines s in unlines $ l : map (indent indentLvl) ines -- The first line has already been indented
    (fg, label) = case logLvl of
      InfoLevel     -> (Green,  "Info")
      WarningLevel  -> (Yellow, "Warning")
      CriticalLevel -> (Red,    "Critical")

--- Control ------------------------------------------------------------------------------------------------------------------------------------------

-- |
pass :: Monad m => m ()
pass = return ()


-- |
execute :: Monad m => EitherT a m b -> m ()
execute = void . runEitherT


-- | Like sequence, but for tuples where the first item is pure
-- TODO: Rename (?)
sequencePairs :: Monad m => [(a, m b)] -> m [(a,b)]
sequencePairs ps = let invert (a, b) = b >>= return . (a,) in sequence . map invert $ ps
    

-- Coordinate systems --------------------------------------------------------------------------------------------------------------------------------

-- |    
snapToNearest :: (Num (v f), RealFrac f, Applicative v) => v f -> v f -> v f -> v f
snapToNearest resolution point origin = liftA2 snap' resolution (point - origin) + origin
  where
    snap' res v = res * (fromIntegral . round $ v/res) -- Snaps in one dimensions

-- Math ----------------------------------------------------------------------------------------------------------------------------------------------

-- Vectors -------------------------------------------------------------------------------------------------------------------------------------------

flatten :: Foldable v => [v f] -> [f]
flatten = concatMap asList


-- foldl :: (b -> a -> b) -> b -> t a -> b
asList :: Foldable v => v f -> [f]
asList = F.foldr (:) []

-- Map operations ------------------------------------------------------------------------------------------------------------------------------------

-- | Perform some action with the indexed item if it is found, otherwise use the default value
-- TODO: Find out if the Lens or Containers packages provide a similar function
