
--
-- Pixels.
-- Loading geometry...
--
-- Jonatan H Sundqvist
-- September 2 2016
--

-- TODO | - 
--        - 

-- SPEC | -
--        -



------------------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
------------------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
--{-# LANGUAGE OverlappingInstances #-}



------------------------------------------------------------------------------------------------------------------------------------------------------
-- API
------------------------------------------------------------------------------------------------------------------------------------------------------
module Pixels.Load where



------------------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
------------------------------------------------------------------------------------------------------------------------------------------------------
import           Prelude hiding (putStrLn, putStr, print, putChar)
-- import qualified Prelude as P

import Text.Printf

import System.FilePath  ((</>), dropExtension, takeFileName) --
import System.Directory (getDirectoryContents)

import Linear.V2
import Linear.V3
import Linear.V4

import           Data.Bits
import qualified Data.Map            as M
import qualified Data.Vector.Unboxed as V
import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Ord      (comparing)

import           Control.Applicative
import           Control.Monad.Trans.Class as St
import           Control.Monad.Trans.Either
import           Control.Monad
import           Control.Lens hiding (argument)

import qualified Graphics.UI.GLFW as GLFW

import Graphics.Rendering.OpenGL as GL hiding (projection, perspective, Line, position, ortho, tessellate)
import Graphics.GLUtil           as GL

import Foreign.C.Types (CUChar)

import           Leibniz.Constants (π)

import qualified Graphics.Text.TrueType        as TT

import qualified Graphics.Michelangelo.Shaders as Shaders
import qualified Graphics.Michelangelo.Shapes  as Shapes

import           Pixels.Types
import qualified Pixels.Lenses as L
import           Pixels.Trinkets



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Error handling
-- TODO: Refactor buffer creation (arbitrary attributes)
-- TODO: Throw error on length mismatch (?)
mesh :: Debug -> GL.PrimitiveMode -> [V3 Float] -> [V4 Float] -> [V2 Float] -> [GL.TextureObject] -> IO Mesh
mesh db prim vertices colours texcoords textures' = do
  logStr db InfoLevel 1 "Loading mesh..."
  unless (allEqual) $ do
    -- TODO: bail
    logStr db WarningLevel 2 $ unlines [printf "Loading mesh with attribute buffers of unequal length!",
                                        printf "Vertex count: %d" (length vertices),
                                        printf "Colour count: %d" (length colours),
                                        printf "Tex    count: %d" (length texcoords)]
  [vs, cs, ts] <- forM [flatten vertices, flatten colours, flatten texcoords] $ GL.makeBuffer GL.ArrayBuffer
  return $ Mesh { fPrimitive        = prim,
                  fAttributeBuffers = M.fromList [("aVertexPosition", (vs, 3)), ("aVertexColor", (cs, 4)), ("aTexCoord", (ts, 2))],
                  fNumVertices      = length vertices,
                  fTextures         = textures' }
  where
    allEqual = all (== length vertices) [length colours, length texcoords]

-- Fundamental shapes --------------------------------------------------------------------------------------------------------------------------------

-- |
type MakeVertex f = (f -> f -> f -> V3 f) -> Float -> Float -> [V3 Float]


-- |
rectangleXY = rectangle Shapes.planeXY
rectangleXZ = rectangle Shapes.planeXZ
rectangleYZ = rectangle Shapes.planeYZ
-- rectangleXZ = rectangle Shapes.planeXZ


-- |
rectangle :: MakeVertex Float -> Debug -> V2 Float -> V4 Float -> [GL.TextureObject] -> IO Mesh
rectangle plane db (V2 dx dy) colour texes = mesh db
                                               (GL.Triangles)
                                               (tessellate id $ plane V3 dx dy)
                                               (replicate 6 colour)
                                               (tessellate id $ [V2 0 1, V2 1 1, V2 1 0, V2 0 0])
                                               (texes)


tessellate :: ([a] -> [b]) -> [a] -> [b]
tessellate f as = concatMap f . Shapes.triangles $ as

-- Shaders -------------------------------------------------------------------------------------------------------------------------------------------

-- |
shaders :: Debug -> FilePath -> IO (Either String GL.Program)
shaders db root = Shaders.loadProgram (root </> "shader-vertex.glsl") (root </> "shader-pixel.glsl")


-- |
-- TODO: Refactor
-- TODO: Error handling (cf. EitherT)
-- TODO: We really need better path handling (...)
meshes :: Debug -> V2 Float -> Paths -> IO (Either String Meshes)
meshes db (V2 w h) paths = runEitherT $ do
  -- Load textures
  [tex, white, mickey] <- textures (paths^.L.textures) ["normal.jpg", "white.png", "sorcerermickey.png"]
  foxsprites           <- sprites  (paths^.L.textures </> "run_frames")
  flamesprites         <- sprites  (paths^.L.textures </> "Flames/flame_1")

  -- Fonts
  fontmap <- fonts (paths^.L.assets </> "fonts")
  thefont <- maybe (left "Couldn't find font") (right) $ M.lookup "digital-7" fontmap

  -- Create meshes
  -- TODO: Refactor
  lift $ M.fromList <$> sequencePairs [("grid",      grid db w h tilesize tilesize (0.0) white),
                                       ("square",    rectangleXY db (V2 30 30) (V4 1 1 1 1) [tex]),
                                       ("interface", interface db white),
                                       ("fox",       rectangleXY db (V2 190 159) (V4 1 1 1 1) (foxsprites)),
                                       ("flame",     rectangleXY db (V2 128 128) (V4 1 1 1 1) (flamesprites)),
                                       ("flame2",    rectangleYZ db (V2 128 128) (V4 1 1 1 1) (flamesprites)), -- TODO: WHY DOESNT THIS WORK
                                       ("message",   fontMesh    db thefont "7+3=0=0=" white),
                                       ("mickey",    rectangleXY db (V2 190 150) (V4 1 1 1 1) ([mickey]))]
  where
    tilesize :: Float
    tilesize = fromIntegral $ (gcd `on` floor) w h -- The 'floor' kinda ruins it, but I happen to know that w and h do not have a fractional part

-- Textures ------------------------------------------------------------------------------------------------------------------------------------------
   
-- |
textures :: FilePath -> [FilePath] -> EitherT String IO [GL.TextureObject]
textures root fns = mapM (EitherT . readTexture . (root </>)) fns


-- |
sprites :: FilePath -> EitherT String IO [GL.TextureObject]
sprites dir = do
  fns <- lift $ sortNumerically . filter isImageFile <$> getDirectoryContents dir
  textures dir fns
  where
    isImageFile :: FilePath -> Bool
    isImageFile fn = any (flip hasExtension fn) allowed

    sortNumerically :: [FilePath] -> [FilePath]
    sortNumerically = sortBy (comparing number)

    allowed :: [String]
    allowed = [".png", ".PNG", ".jpeg", ".JPEG", ".jpg", ".JPG"]

    number :: String -> Int
    number = read . dropWhile (not . (`elem` "0123456789")) . dropExtension . takeFileName -- 

-- Fonts ---------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- fonts :: EitherT String IO [GL.TextureObject]
fonts :: FilePath -> EitherT String IO (M.Map String TT.Font)
fonts root = M.fromList <$> (sequencePairs $ map (load <$>) [("Elixia",    "Elixia.ttf"),
                                                             ("rothenbg",  "rothenbg.ttf"),
                                                             ("digital-7", "digital-7 (mono).ttf")])
  where
    load :: FilePath -> EitherT String IO TT.Font
    load = EitherT . TT.loadFontFile . (root </>)


-- |
-- TODO: Tessellate (on the GPU?)
--       cf. https://hackage.haskell.org/package/OpenGL-2.1/docs/Graphics-Rendering-OpenGL-GLU-Tessellation.html#t:AnnotatedVertex
-- TODO: Options
-- TODO: Metrics
fontMesh :: Debug -> TT.Font -> String -> GL.TextureObject -> IO Mesh
fontMesh db font' s tex = mesh db GL.Lines (concat vertices') colours' texcoords' [tex]
  where
    vertices' :: [[V3 Float]]
    vertices' = map makeGlyphVertices glyphs
    
    -- I'm assuming each V.Vector holds the coordinates for a connected line strip
    -- and that each glyph is represented by a list of such vectors (some glyphs are of course disjoint).
    --
    -- The docs do not make this point explicitly so I'll just have to test it. 
    makeGlyphVertices :: [V.Vector (Float, Float)] -> [V3 Float]
    makeGlyphVertices glyph = concatMap stitch glyph

    -- | I also considered the names 'pairs', 'pairwise' and 'connect'
    -- TODO: Be strict (remind me why foldl is bad?)
    -- TODO: Account for empty Vector (?)
    stitch :: V.Vector (Float, Float) -> [V3 Float]
    stitch = drop 1 . reverse . drop 1 . V.foldl (\xs p -> let v = fromTuple p in v:v:xs) []

    fromTuple :: Num a => (a, a) -> V3 a
    fromTuple (x, y) = V3 x (-y) 0

    texcoords' = map (\(V3 x y z) -> V2 (x/1) (y/1)) (concat vertices') -- TODO: Do this properly?
    colours'   = concat $ zipWith (\c g -> replicate (length g) c) (cycle [V4 0.2 0.2 0.9 1, V4 0.9 0.35 0.42 1.0]) vertices' --
    glyphs = TT.getStringCurveAtPoint 508 (0,0) [(font', TT.PointSize 48, s)] -- TODO: Figure out exactly what each argument means


-- Interface -----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Use applicative (?)
interface :: Debug -> GL.TextureObject -> IO Mesh
interface db white = do
  [vs, cs, ts] <- forM [flatten vertices', flatten colors', flatten texcoords'] $ GL.makeBuffer GL.ArrayBuffer
  return $ Mesh { fPrimitive        = GL.Triangles,
                  fAttributeBuffers = M.fromList [("aVertexPosition", (vs, 3)), ("aVertexColor", (cs, 4)), ("aTexCoord", (ts, 2))],
                  fNumVertices      = length vertices',
                  fTextures         = textures' }
  where
    textures' :: [GL.TextureObject]
    textures' = [white]

    palette :: [V4 Float]
    palette = [V4 1.00 0.00 0.00 1.00, V4 1.00 1.00 1.00 1.00, V4 0.22 0.22 0.22 1.00, V4 0.05 0.15 0.95 1.00, V4 0.95 0.08 0.92 1.00,
               V4 0.20 0.30 0.40 1.00, V4 1.00 0.95 0.23 1.00, V4 0.00 0.00 0.00 1.00, V4 0.72 0.00 1.00 1.00, V4 0.22 0.35 0.55 1.00]
    
    vertices' :: [V3 Float]
    vertices' = concat $ [concat $ Shapes.triangles $ Shapes.planeXY (makeVertex n) dx dy | (n,_) <- zip [0..] palette]

    colors' :: [V4 Float]
    colors' = concat $ map (replicate 6) palette -- Two triangels (2*3 vertices) per quad

    texcoords' :: [V2 Float]
    texcoords' = map (const $ V2 0 0) vertices'
    
    --
    -- makeVertex :: (RealFloat r) => Int -> r -> r -> r -> V3 r
    makeVertex :: Int -> Float -> Float -> Float -> V3 Float
    makeVertex n x y _ = let row = fromIntegral (n `div` cols)
                             col = fromIntegral (n `mod` cols)
                         in V3 (x + col * (dx+padx)) (y + row * (dy+pady)) (0)

    -- Palette layout parameters
    (V2   dx   dy) = V2 30 30 :: V2 Float
    (V2 padx pady) = V2  6  6 :: V2 Float
    (V2 rows cols) = V2  2  5 :: V2 Int


-- | Minimap experiment
-- minimap :: IO

-- IO helpers ----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Loading from file
cursor :: FilePath -> IO (Either String GLFW.Cursor)
cursor fn = Right <$> GLFW.createCursor (makeImage pixels) 30 30
  where
    -- TODO: Refactor this mess
    unpack :: (Integral n, Bits n) => n -> [CUChar]
    unpack c = map (fromIntegral . (\m -> (c .&. (0xFF `shiftL` m)) `shiftR` m)) [24, 16, 8, 0] -- Split up the channels
   
    pixels :: [[Int]]
    pixels = []

    flatten :: (Integral n, Bits n) => [[n]] -> [CUChar]
    flatten = concat . map (unpack) . concat

    makeImage :: (Integral n, Bits n) => [[n]] -> GLFW.Image
    makeImage px = GLFW.Image { GLFW.imagePixels = flatten px,
                                GLFW.imageWidth  = (length $ px!!0),
                                GLFW.imageHeight = (length $ px) }

-- Rendering -----------------------------------------------------------------------------------------------------------------------------------------

-- |
-- grid :: Integral n => V2 Float -> n -> n -> Float -> IO Mesh
-- TODO: More options
-- TODO: 3D grid
-- TODO: Factor out (to Cartesian)
grid :: Debug -> Float -> Float -> Float -> Float -> Float -> GL.TextureObject -> IO Mesh
grid db w h dx dy z white = do
  mesh db GL.Lines vertices colours texcoords [white]
  where
    -- dx n = fromIntegral n * w/fromIntegral cols - w/2
    -- dy n = fromIntegral n * h/fromIntegral rows - h/2
    (V2 cols rows) = liftA2 (/) (V2 w h) (V2 dx dy)
    colours = replicate (length vertices) (V4 0.58 0.58 0.58 1.00)
    
    vertices  = verticals ++ horizontals
    texcoords = map (\(V3 x y _) -> V2 (x/w) (y/h)) vertices

    verticals   = concat [ let x = c*dx - w/2 in [V3    (x) (-h/2) z, V3   (x) (h/2) z] | c <- [0..cols]]
    horizontals = concat [ let y = r*dy - h/2 in [V3 (-w/2)    (y) z, V3 (w/2)   (y) z] | r <- [0..rows]]

