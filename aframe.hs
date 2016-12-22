{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
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
posForSticker size | (odd size) = oddPosForSticker
              | otherwise = error "Implement even size case for posForSticker" 

mapOverUniform3Tuple :: (a -> b) -> (a,a,a) -> (b,b,b) -- Small utility function
mapOverUniform3Tuple f (a,b,c) = (f a, f b, f c)

oddPosForSticker :: (Fractional a) => FaceId -> XPos -> YPos -> (a,a,a)
oddPosForSticker 0 x y = mapOverUniform3Tuple fromRational (toRational x,1.5,toRational $ (-1)*y) 
oddPosForSticker 1 x y = mapOverUniform3Tuple fromRational (toRational x,toRational y,1.5) 
oddPosForSticker 2 x y = mapOverUniform3Tuple fromRational ((-1.5),toRational x,toRational $ (-1)*y) 
oddPosForSticker 3 x y = mapOverUniform3Tuple fromRational (toRational x,toRational $ (-1)*y,(-1.5)) 
oddPosForSticker 4 x y = mapOverUniform3Tuple fromRational (1.5,toRational $ (-1)*x,toRational $ (-1)*y) 
oddPosForSticker 5 x y = mapOverUniform3Tuple fromRational (toRational $ (-1)*x,-1.5,toRational $ (-1)*y)

-- Determine the rotation in 3-space of all the sticker planes for a cube, which is solely determined by their faces
rotationForSticker :: FaceId -> XPos -> YPos -> (Number, Number, Number)
rotationForSticker 0 = const (const (-90.0 ,0.0     ,0.0))
rotationForSticker 1 = const (const (0.0   ,0.0     ,0.0))
rotationForSticker 2 = const (const (0.0   ,-90.0   ,0.0))
rotationForSticker 3 = const (const (0.0   ,180.0   ,0.0))
rotationForSticker 4 = const (const (0.0   ,90.0   ,0.0))
rotationForSticker 5 = const (const (90.0 ,0.0     ,0.0))

hexForColor :: (Data.String.IsString s) => Rubiks.Color -> s
hexForColor 0 = "#000000" -- White/Black
hexForColor 1 = "#FF0000" -- Red
hexForColor 2 = "#00FF00" -- Green
hexForColor 3 = "#FF8C00" -- Orange
hexForColor 4 = "#0000FF" -- Blue
hexForColor 5 = "#FFFF00" -- Yellow

-- Using the above function to create an appropriate DSL for a sticker on the cube
stickerDSL :: CubeSize -> FaceId -> XPos -> YPos -> Rubiks.Color -> DSL () 
stickerDSL size f x y c = plane $ do
    rotation $ rotationForSticker f x y
    width 1
    height 1
    color $ hexForColor c
    position $ posForSticker size f x y

-- Build an abstract Cube with the above function partially-applied as the data component
dslCubeOfSize :: CubeSize -> Cube (Rubiks.Color -> DSL ())
dslCubeOfSize size = Cube size $ stickerDSL size

-- Pseudo-Applicative apply that with a given cube to create a cube with all our DSL's as data
getDSLsForCube :: IndexedCube -> DSL [()]
getDSLsForCube c@(Cube size _) = sequence . orderedElements . fromJust $ absoluteDSLCube 
    where colorCube = fmap (flip div (size*size)) c
          absoluteDSLCube = applyCube (dslCubeOfSize size) $ colorCube

-- Build a scene frome that
cubeScene :: AFrame
cubeScene = scene $ do
    getDSLsForCube $ flip applyPerms [rightMove 3 0, upMove 3 0, rightMove 3 0, rightMove 3 0, backMove 3 0] $ solvedCubeOfSize 3
     
    entity $ do
        position (0,-5,5)
        camera $ return ()

--  Main function to create a 
main :: IO ()
main = do
    args <- getArgs
    webPage args cubeScene 
