--
-- Pixels.Render.Typography
-- ...
--
--

-- TODO | - 
--        - 

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Render.Typography where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import Graphics.GPipe hiding (texture)

import Pixels.Types



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- createTextAtlas :: Font -> Int -> AppT os (Texture2D os (Format RGBFloat))
-- createTextAtlas font pt = do
--   _