module Cubie where
import Rubiks
import Data.Maybe (fromJust, isJust)
import Control.Applicative (liftA2)

-- A Cubie is a small cube-shaped plastic piece on a Rubik's Cube.  On the standard 3x3 Cube, there
--      are 26 total cubies.  They can be center cubies (with one sticker), edges (with two
--      stickers), or corners (with three).
-- This module consists of functionality to translate the sticker representation we developed
--      before into a cubie-centric representation.  The idea is that we will represent a cube
--      as a mapping from x,y,z coordinates to a cubie containing some number of stickers.

data ThreeDimensionalDir = DirUp | DirDown | DirLeft | DirRight | DirFront | DirBack
    deriving (Eq,Show)

-- This structure should be able to be extended to higher-dimensional hypercubies in the future
type CubieStickerFunc s = ThreeDimensionalDir -> Maybe s

type CubieX = Int
type CubieY = Int
type CubieZ = Int

-- These function types are used to map from X,Y,Z coords to a cubie data value and CubieStickerFunc carrying sticker data
type CubieFunc c         = CubieX -> CubieY -> CubieZ -> c
type CubieComboFunc c s  = CubieFunc (Maybe (c, CubieStickerFunc s))

-- This structure wraps up all of the above into a representation of 
data CubieMap c s        = CubieMap CubeSize (CubieComboFunc c s)

isOuterCoordinate :: CubeSize -> Int -> Bool
isOuterCoordinate size c = (abs c) == (size `div` 2)

-- These functions can be used to map the Cube representation defined in Rubiks to this representation
getStickersFor :: Cube a -> CubieX -> CubieY -> CubieZ -> ThreeDimensionalDir -> Maybe a
getStickersFor cube@(Cube size func) x y z d
    | (x == k') && (d == DirLeft)   = Just $ func 2 y      z 
    | (x == k ) && (d == DirRight)  = Just $ func 4 ((-1)*y) z
    | (y == k ) && (d == DirUp)     = Just $ func 0 x z
    | (y == k') && (d == DirDown)   = Just $ func 5 ((-1)*x) z
    | (z == k') && (d == DirFront)  = Just $ func 1 x      y
    | (z == k ) && (d == DirBack)   = Just $ func 3 x      ((-1)*y)
    | otherwise                 = Nothing
    where k = size `div` 2
          k' = (-1)*k
 
getCubie :: Cube s -> CubieFunc c -> CubieComboFunc c s
getCubie cube@(Cube size func) cf x y z 
    | or $ (map (isOuterCoordinate size)) [x,y,z] = Just $ (cVal, sFunc)
    | otherwise = Nothing 
        where cVal = cf x y z
              sFunc = getStickersFor cube x y z 

-- Uses the above functions to build a CubieMap structure with both cubie data and sticker data
buildCubieMap :: Cube a -> CubieFunc c -> CubieMap c a
buildCubieMap c@(Cube size _) cf = CubieMap size $ getCubie c cf

-- A simple use case of the above function where the cubie data is a null unit value
buildStdCubieMap :: Cube a -> CubieMap () a
buildStdCubieMap = flip buildCubieMap $ const . const . const ()

-- Creates an arbitrarily-ordered list of the cubies and their positions
listCubies :: CubieMap c s -> [((CubieX, CubieY, CubieZ), c, CubieStickerFunc s)]
listCubies c@(CubieMap size func) = unwrapped 
    where range = xyRangeForSize size
          maybeList = [((x,y,z), func x y z) | x <- range, y <- range, z <- range] 
          filtered = filter (isJust . snd) maybeList
          unwrapped = map (\(a,Just (b,c)) -> (a,b,c)) filtered

-- And here we have the inverse of the above transformation, going from a CubieMap to a Cube
cubiesToCube :: CubieMap c s -> Cube s
cubiesToCube m@(CubieMap size _) = Cube size $ stickerAt m 

stickerAt :: CubieMap c s -> FaceId -> XPos -> YPos -> s 
stickerAt m@(CubieMap size func) f x y = case f of
    0 -> fromJust $ snd (fromJust $ func x  k  y ) $ DirUp
    1 -> fromJust $ snd (fromJust $ func x  y  k ) $ DirFront
    2 -> fromJust $ snd (fromJust $ func k' x  y ) $ DirLeft
    3 -> fromJust $ snd (fromJust $ func x  y' k ) $ DirBack
    4 -> fromJust $ snd (fromJust $ func k  x' y ) $ DirRight
    5 -> fromJust $ snd (fromJust $ func x' k' y ) $ DirDown
    where k = size `div` 2
          x':(y':(k':[])) = map (*(-1)) [x,y,k] -- Defined for convencience in referencing negatives
