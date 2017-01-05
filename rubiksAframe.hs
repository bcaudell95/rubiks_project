{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module RubiksAFrame where
import Rubiks

import Data.String (IsString)
import Data.Text (Text, pack)
import Text.AFrame
import Text.AFrame.WebPage
import Text.AFrame.DSL 
import Data.Maybe (fromJust)

import System.Environment

-- Mapping from (face, x, y) cube space to (x,y,z) global locations
-- Output coordinates are the centers of the sticker planes
posForSticker :: (Fractional a) => CubeSize -> FaceId -> XPos -> YPos -> (a,a,a)
posForSticker size f x y = toFrac $ 
    case f of 0 -> (dx , d  , dy' ) 
              1 -> (dx , dy , d   ) 
              2 -> (d' , dx , dy' ) 
              3 -> (dx , dy', d'  ) 
              4 -> (d  , dx', dy' ) 
              5 -> (dx', d' , dy' )
    where toFrac = mapOverUniform3Tuple fromRational
          d = (toRational size) / 2
          d' = (-1) * d
          dx = if odd size then toRational x else absSub (toRational x) 0.5
          dx' = (-1) * dx
          dy = if odd size then toRational y else absSub (toRational y) 0.5
          dy' = (-1) * dy

-- Utility function to map over a uniformly-typed 3-tuple
mapOverUniform3Tuple :: (a -> b) -> (a,a,a) -> (b,b,b)
mapOverUniform3Tuple f (a,b,c) = (f a, f b, f c)

-- Utility function for always ``subtracting'' towards zero
absSub :: (Num a, Ord a) => a -> a -> a
absSub a b = if a >= 0 then a - b else a + b

-- Function to translate the above sticker position to the position for the plastic cubie underneath
translatePosForCubie :: (Fractional a) => CubeSize -> (a,a,a) -> FaceId -> (a,a,a)
translatePosForCubie size (x,y,z) f = 
    case f of 0 -> (x, y - 0.5, z)
              1 -> (x, y, z - 0.5)
              2 -> (x + 0.5, y, z)
              3 -> (x, y, z + 0.5)
              4 -> (x - 0.5, y, z)
              5 -> (x, y + 0.5, z)

-- Determine the rotation in 3-space of all the sticker planes for a cube, which is solely determined by their faces
rotationForSticker :: FaceId -> XPos -> YPos -> (Number, Number, Number)
rotationForSticker 0 _ _ = (-90.0 ,0.0     ,0.0)
rotationForSticker 1 _ _ = (0.0   ,0.0     ,0.0)
rotationForSticker 2 _ _ = (0.0   ,-90.0   ,0.0)
rotationForSticker 3 _ _ = (0.0   ,180.0   ,0.0)
rotationForSticker 4 _ _ = (0.0   ,90.0    ,0.0)
rotationForSticker 5 _ _ = (90.0  ,0.0     ,0.0)

hexForColor :: (Data.String.IsString s) => Rubiks.Color -> s
hexForColor 0 = "#FFFFFF" -- White/Black
hexForColor 1 = "#FF0000" -- Red
hexForColor 2 = "#00FF00" -- Green
hexForColor 3 = "#FF8C00" -- Orange
hexForColor 4 = "#0000FF" -- Blue
hexForColor 5 = "#FFFF00" -- Yellow

-- Using the above function to create an appropriate DSL for a sticker on the cube
stickerDSL :: CubeSize -> FaceId -> XPos -> YPos -> Rubiks.Color -> DSL () 
stickerDSL size f x y c = do
    plane $ do
        rotation $ rotationForSticker f x y
        width 0.95
        height 0.95
        color $ hexForColor c
        position pos

    box $ do
        position $ translatePosForCubie size pos f
        width 1
        height 1
        scale (0.99, 0.99, 0.99)
        color "#000000"
    where pos = posForSticker size f x y

-- Build an abstract Cube with the above function partially-applied as the data component
dslCubeOfSize :: CubeSize -> Cube (Rubiks.Color -> DSL ())
dslCubeOfSize size = Cube size $ stickerDSL size

-- Pseudo-Applicative apply that with a given cube to create a cube with all our DSL's as data
getDSLsForCube :: IndexedCube -> DSL [()]
getDSLsForCube c@(Cube size _) = sequence . orderedElements . fromJust $ absoluteDSLCube 
    where colorCube = fmap (idToColor size) c
          relativeDSLCube = dslCubeOfSize size
          absoluteDSLCube = applyCube relativeDSLCube colorCube

-- Takes a Cube, gets its construction DSLs, and wraps them in an entity with a position that can be moved
positionCube :: IndexedCube -> (Number, Number, Number) -> DSL [()]
positionCube c@(Cube size _) pos = entity $ do
    position pos
    getDSLsForCube c

-- Build a scene frome that
cubeScene :: [(IndexedCube, (Number, Number, Number))] -> AFrame
cubeScene cubesAndPositions = scene $ do
    sequence $ map (uncurry positionCube) cubesAndPositions
     
    entity $ do
        position (0,-5,5)
        camera $ return ()

--  Main function to output an HTML file with an aframe scene of some cubes
outputScene :: String -> [(IndexedCube, (Number, Number, Number))] -> IO ()
outputScene fn cubes = do
    webPage [fn] $ cubeScene cubes

main :: IO ()
main = do
    let cube1 = solvedCubeOfSize 3
    let cube2 = applyPerm cube1 $ xRotation 3
    let cube3 = applyPerm cube1 $ yRotation 3
    let cube4 = applyPerm cube1 $ zRotation 3
    args <- getArgs
    outputScene (head args) [(cube1, (-5,0,-5)), (cube2, (0,0,-5)), (cube3, (5,0,-5)), (cube4, (10,0,-5))]