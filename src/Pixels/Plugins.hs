-- |
-- Module      : Pixels.Plugins
-- Description : Loading program extensions
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
-- 

-- Created September 3 2016
--
-- Based on:
-- https://github.com/SwiftsNamesake/Jigsaw/blob/master/src/Plugins.hs

-- TODO | - 
--        - 

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Plugins where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Data.Maybe (maybe)
import Data.Dynamic
import Data.Functor ((<$>))
import qualified Data.Map.Strict as M

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce



-- |
load :: String -> String -> [String] -> IO (Either PluginError (M.Map String Dynamic))
load fn modname symbols = defaultErrorHandler putStrLn (FlushOut $ putStrLn "Something went awry. Flushing out.") $ do
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    target <- guessTarget fn Nothing
    addTarget target
    r <- GHC.load LoadAllTargets
    case r of
      Failed    -> return $ Left PluginError
      Succeeded -> do
        setContext [IIDecl $ simpleImportDecl (mkModuleName modname)]
        (Right . M.fromList . zip symbols) <$> mapM (\s -> dynCompileExpr $ modname ++ "." ++ s) symbols -- A list of 'dynamic imports'



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------------------------------------------------------------------
-- |
data Plugin a = Plugin { run :: a } deriving Show


-- |
data PluginError = PluginError deriving Show



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------



---- |
--rawLoad = do
--  target <- GHC.guessTarget "mods/Test.hs" Nothing
--  GHC.addTarget target
--  r <- GHC.load GHC.LoadAllTargets
--  case r of
--    GHC.Failed    -> error "Compilation failed"
--    GHC.Succeeded -> do
--      m <- GHC.findModule (GHC.mkModuleName "Test") Nothing
--      GHC.setContext [] [m]
--      value <- GHC.compileExpr "Test.print"
--      return value--
