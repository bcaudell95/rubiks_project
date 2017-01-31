{-# LANGUAGE  RankNTypes, FlexibleInstances, UndecidableInstances, OverloadedStrings #-}
module RubiksAFrame where
import Rubiks
import Cubie

import Data.String (IsString)
import Data.Text (Text, pack)
import Data.List (intersperse)
import Text.AFrame
import Text.AFrame.WebPage hiding (webPage)
import Text.AFrame.DSL 
import Data.Maybe (fromJust, isJust)

import System.Environment

type AnimDelay = Int
-- A Vector of mod-360 rotation values (currently Ints, could be changed)
newtype RotationVec = RotationVec (Int, Int, Int)

instance Show RotationVec where
    show (RotationVec (delay, start, end)) = concat $ intersperse " " [show delay, show start, show end]

instance Monoid RotationVec where
    mempty = RotationVec (0,0,0)
    (RotationVec (a1, a2, a3)) `mappend` (RotationVec (b1, b2, b3)) = RotationVec  (a1 + b1, a2 + b2, a3 + b3)

type AnimData = (AnimDelay, RotationVec, RotationVec)

-- Each cubie contains a processed list of animations that it needs to perform
type AnimationCubieMap a = CubieMap [AnimData] a

-- This type is the unprocessed version of the above data.  All cubies have equivalent-length lists of Maybe (AnimAxis, AnimDir) which will be turned
--      into an AnimationCubieMap
data AnimAxis = AxisX | AxisY | AxisZ
data AnimDir = Forwards | Backwards

type RawAnimationCubieMap a = CubieMap [Maybe (AnimAxis, AnimDir)] a

animationDuration :: Int
animationDuration = 2000

reverseAnimDir :: AnimDir -> AnimDir
reverseAnimDir Forwards = Backwards
reverseAnimDir Backwards = Forwards

applyAnimationForRotation :: RotationVec -> (AnimAxis, AnimDir) -> RotationVec
applyAnimationForRotation start (AxisX, Forwards)  = start `mappend` (RotationVec ( 90,   0,   0))
applyAnimationForRotation start (AxisX, Backwards) = start `mappend` (RotationVec (-90,   0,   0))
applyAnimationForRotation start (AxisY, Forwards)  = start `mappend` (RotationVec (  0,  90,   0))
applyAnimationForRotation start (AxisY, Backwards) = start `mappend` (RotationVec (  0, -90,   0))
applyAnimationForRotation start (AxisZ, Forwards)  = start `mappend` (RotationVec (  0,   0,  90))
applyAnimationForRotation start (AxisZ, Backwards) = start `mappend` (RotationVec (  0,   0, -90))

processAnimationChain :: RotationVec -> [Maybe (AnimAxis, AnimDir)] -> [AnimData]
processAnimationChain _ [] = []
processAnimationChain start (Nothing:as) = ((animationDuration, start, start): processAnimationChain start as)
processAnimationChain start ((Just rot):as) = ((animationDuration, start, end): processAnimationChain end as) 
    where end = applyAnimationForRotation start rot

processRawAnimationCubieMap :: RawAnimationCubieMap a -> AnimationCubieMap a
processRawAnimationCubieMap = mapOverCubies (processAnimationChain mempty) 

-- Utility function to map over a uniformly-typed 3-tuple
mapOverUniform3Tuple :: (a -> b) -> (a,a,a) -> (b,b,b)
mapOverUniform3Tuple f (a,b,c) = (f a, f b, f c)

-- Utility function for always ``subtracting'' towards zero
absSub :: (Num a, Ord a) => a -> a -> a
absSub a b = if a >= 0 then a - b else a + b

hexForColor :: (Data.String.IsString s) => Rubiks.Color -> s
hexForColor 0 = "#FFFFFF" -- White/Black
hexForColor 1 = "#FF0000" -- Red
hexForColor 2 = "#00FF00" -- Green
hexForColor 3 = "#FF8C00" -- Orange
hexForColor 4 = "#0000FF" -- Blue
hexForColor 5 = "#FFFF00" -- Yellow

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

animationFromAnimData :: (Attributes f ) => Int -> AnimData -> f ()
animationFromAnimData index (delay, start, end) = attribute (Label . pack $  "animation__" ++ show index) $ pack $
    "property: rotation; " ++
    (if index > 0 then "startEvents: animation__" ++ show (index -1)  ++ "-complete; " else "") ++
    ("from: " ++ show start ++ "; ") ++
    ("to: " ++ show end ++ "; ") ++
    ("dur: " ++ show animationDuration ++ ";")

-- To facilitate animation, we will have a "control entity" for each cubie, which extends it out from the origin
dslControlForCubie :: CubeSize -> (CubieX, CubieY, CubieZ) -> [AnimData] -> CubieStickerFunc Rubiks.Color -> DSL ()
dslControlForCubie size (cx, cy, cz) anims func = entity $ do
    sequence $ zipWith animationFromAnimData  [0..] anims
    
    entity $ do
        -- We need to invert the Z component, because aframe uses the convention that positive z is towards the camera
        let x' = if odd size then toRational cx else (toRational cx) `absSub` 0.5
        let y' = if odd size then toRational cy else (toRational cy) `absSub` 0.5
        let z' = (*(-1.0)) $ if odd size then toRational cz else (toRational $ cz) `absSub` 0.5
    
        position . (mapOverUniform3Tuple fromRational) $ (x', y', z')
        cubieDSL func

dslForCubieMap :: RawAnimationCubieMap Rubiks.Color -> DSL ()
dslForCubieMap cube@(CubieMap size _) = entity $ do
    let positionsAndCubies = listCubies . processRawAnimationCubieMap $ cube
    sequence $ (\(a,b,c) -> zipWith3 (dslControlForCubie size) a b c) $ unzip3 $ positionsAndCubies
    return ()

-- Pseudo-Applicative apply that with a given cube to create a cube with all our DSL's as data
getDSLsForCube :: RawAnimationCubieMap StickerId -> DSL ()
getDSLsForCube c@(CubieMap size _) = dslForCubieMap colorCube 
    where colorCube = mapOverStickers (idToColor size) c

spinningAnimation :: DSL ()
spinningAnimation = animation $ do
    attribute_ "rotation"
    dur 3000
    fill "forwards"
    to "0 360 0"
    repeat_ "indefinite"

-- Takes a Cube, gets its construction DSLs, and wraps them in an entity with a position that can be moved and animated (with a flag for the animation)
positionCube :: RawAnimationCubieMap StickerId -> (Number, Number, Number) -> Bool -> DSL ()
positionCube c@(CubieMap size _) pos animFlag = entity $ do
    position pos
    --if animFlag then spinningAnimation else return () 
    getDSLsForCube c

-- Build a scene from that, with a flag for solved animation
cubeScene :: [(RawAnimationCubieMap StickerId, (Number, Number, Number))] -> Bool -> AFrame
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

webPage :: [String] -> AFrame -> IO ()        
webPage [fileOut] af = do
    file <- readFile "animated.html"
    writeFile fileOut $ injectAFrame af $ file

main :: IO ()
main = do
    let cube1 = solvedCubeOfSize 3
    let cube2 = applyPerm cube1 $ xRotation 3
    let cube3 = applyPerm cube1 $ yRotation 3
    let cube4 = applyPerm cube1 $ zRotation 3
    let cubes = [(cube1, (-5,0,-5)), (cube2, (0,0,-5)), (cube3, (5,0,-5)), (cube4, (10,0,-5))]
    let cubes' = fmap (\(c,p) -> ((flip buildCubieMap (\x y z -> if z == 0 then [Just (AxisZ, Forwards), Nothing, Just (AxisZ, Backwards)] else [])) $ c, p)) cubes
    args <- getArgs
    outputScene (head args) $ cubeScene cubes' True 
