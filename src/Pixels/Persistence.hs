-- |
-- Module      : Pixels.Persistence
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



-- GHC Pragmas ---------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings      #-}

-- API -----------------------------------------------------------------------------------------------------------------------------------------------

module Pixels.Persistence where

-- We'll need these ----------------------------------------------------------------------------------------------------------------------------------

import Linear (V2(..), V3(..), V4(..))

import Data.Aeson as JSON (FromJSON(..), (.:), withObject, eitherDecode)

import Data.ByteString.Lazy as B

import Pixels.Types
import Pixels.Lenses
import Pixels.Trinkets

-- Definitions ---------------------------------------------------------------------------------------------------------------------------------------

instance FromJSON a => FromJSON (V2 a) where
  parseJSON = withObject "V2" $ \o -> V2 <$> o .: "x"
                                         <*> o .: "y"


instance FromJSON a => FromJSON (V3 a) where
  parseJSON = withObject "V3" $ \o -> V3 <$> o .: "x"
                                         <*> o .: "y"
                                         <*> o .: "z"


instance FromJSON a => FromJSON (V4 a) where
  parseJSON = withObject "V4" $ \o -> V4 <$> o .: "x"
                                         <*> o .: "y"
                                         <*> o .: "z"
                                         <*> o .: "w"


instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \o -> AppConfig <$> (o .: "canvas" >>= (.: "size"))   -- TODO: Refactor
                                                       <*> (o .: "canvas" >>= (.: "colour")) -- TODO: Refactor
                                                       <*> (o .: "window" >>= (.: "size"))
                                                       <*> (o .: "brush")

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Catch exceptions
--        -
loadConfig :: String -> IO (Either String AppConfig)
loadConfig fn = eitherDecode <$> B.readFile fn