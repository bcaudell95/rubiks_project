{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module RubiksAFrame where
import Rubiks
import Cubie

import Data.String (IsString)
import Data.Text (Text, pack)
import Text.AFrame
import Text.AFrame.WebPage
import Text.AFrame.DSL 
import Data.Maybe (fromJust, isJust)

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

stickerRelPosition :: (Fractional a) => ThreeDimensionalDir -> (a, a, a)
stickerRelPosition DirUp    = (0.0 ,0.5   ,0.0 )
stickerRelPosition DirDown  = (0.0 ,-0.5  ,0.0 )
stickerRelPosition DirLeft  = (-0.5,0.0   ,0.0 )
stickerRelPosition DirRight = (0.5 ,0.0   ,0.0 )
stickerRelPosition DirFront = (0.0 ,0.0   ,0.5 )
stickerRelPosition DirBack  = (0.0 ,0.0   ,-0.5)

rotationForSticker' :: (Fractional a) => ThreeDimensionalDir -> (a, a, a)
rotationForSticker' DirUp    = (-90.0 ,0.0     ,0.0)
rotationForSticker' DirFront = (0.0   ,0.0     ,0.0)
rotationForSticker' DirLeft  = (0.0   ,-90.0   ,0.0)
rotationForSticker' DirBack  = (0.0   ,180.0   ,0.0)
rotationForSticker' DirRight = (0.0   ,90.0    ,0.0)
rotationForSticker' DirDown  = (90.0  ,0.0     ,0.0)

stickerDSL' :: Rubiks.Color -> ThreeDimensionalDir -> DSL ()
stickerDSL' c dir = plane $ do
        position $ stickerRelPosition dir
        rotation $ rotationForSticker' dir
        width 0.95
        height 0.95
        color $ hexForColor c

cubieDSL :: CubieStickerFunc Rubiks.Color -> DSL ()
cubieDSL func = do
    box $ do
        position $ (0,0,0)
        width 1
        height 1
        scale (0.99, 0.99, 0.99)
        color "#000000"

    let maybeColorsAndDirs = fmap ((,) <$> func <*> id) dirs
    let colorsAndDirs = fmap (\(a,b) -> (fromJust a, b)) $ filter (isJust . fst) maybeColorsAndDirs
    sequence $ fmap (uncurry stickerDSL') colorsAndDirs

    return ()

    where dirs = [DirUp, DirDown, DirLeft, DirRight, DirFront, DirBack]

-- To facilitate animation, we will have a "control entity" for each cubie, which extends it out from the origin
dslControlForCubie :: CubeSize -> (CubieX, CubieY, CubieZ) -> CubieStickerFunc Rubiks.Color -> DSL ()
dslControlForCubie size (cx, cy, cz) func = entity $ do
    -- This is where cubie-specific animations will go once that is implemented
    
    entity $ do
        -- We need to invert the Z component, because aframe uses the convention that positive z is towards the camera
        let x' = if odd size then toRational cx else (toRational cx) `absSub` 0.5
        let y' = if odd size then toRational cy else (toRational cy) `absSub` 0.5
        let z' = (*(-1.0)) $ if odd size then toRational cz else (toRational $ cz) `absSub` 0.5
    
        position . (mapOverUniform3Tuple fromRational) $ (x', y', z')
        cubieDSL func

dslForCubieMap :: CubieMap c Rubiks.Color -> DSL ()
dslForCubieMap c@(CubieMap size _) = entity $ do
    let positionsAndCubies = listCubies c
    sequence $ fmap (uncurry (dslControlForCubie size) . (\(a,_,c) -> (a,c))) positionsAndCubies
    return ()

-- Build an abstract Cube with the above function partially-applied as the data component
dslCubeOfSize :: CubeSize -> Cube (Rubiks.Color -> DSL ())
dslCubeOfSize size = Cube size $ stickerDSL size

-- Pseudo-Applicative apply that with a given cube to create a cube with all our DSL's as data
getDSLsForCube :: IndexedCube -> DSL ()
getDSLsForCube c@(Cube size _) = dslForCubieMap cubieMap 
    where colorCube = fmap (idToColor size) c
          cubieMap = buildStdCubieMap colorCube

spinningAnimation :: DSL ()
spinningAnimation = animation $ do
    attribute_ "rotation"
    dur 3000
    fill "forwards"
    to "0 360 0"
    repeat_ "indefinite"

-- Takes a Cube, gets its construction DSLs, and wraps them in an entity with a position that can be moved and animated (with a flag for the animation)
positionCube :: IndexedCube -> (Number, Number, Number) -> Bool -> DSL ()
positionCube c@(Cube size _) pos animFlag = entity $ do
    position pos
    if animFlag then spinningAnimation else return () 
    getDSLsForCube c

-- Build a scene from that, with a flag for solved animation
cubeScene :: [(IndexedCube, (Number, Number, Number))] -> Bool -> AFrame
cubeScene cubesAndPositions anim = scene $ do
    sequence $ zipWith3 positionCube cubes positions (repeat anim)
     
    entity $ do
        position (0,-5,5)
        camera $ return ()
    where (cubes, positions) = unzip cubesAndPositions

--  Main function to output an HTML file with an aframe scene of some cubes
outputScene :: String -> AFrame -> IO ()
outputScene fn scene = do
    webPage [fn] scene

main :: IO ()
main = do
    let cube1 = solvedCubeOfSize 3
    let cube2 = applyPerm cube1 $ xRotation 3
    let cube3 = applyPerm cube1 $ yRotation 3
    let cube4 = applyPerm cube1 $ zRotation 3
    let cubes = [(cube1, (-5,0,-5)), (cube2, (0,0,-5)), (cube3, (5,0,-5)), (cube4, (10,0,-5))]
    args <- getArgs
    outputScene (head args) $ cubeScene cubes True 
