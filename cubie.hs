module Rubiks.Cubie where
import Rubiks
import Data.Maybe (fromJust)

-- A Cubie is a small cube-shaped plastic piece on a Rubik's Cube.  On the standard 3x3 Cube, there
--      are 26 total cubies.  They can be center cubies (with one sticker), edges (with two
--      stickers), or corners (with three).
-- This module consists of functionality to translate the sticker representation we developed
--      before into a cubie-centric representation.  The idea is that we will represent a cube
--      as a mapping from x,y,z coordinates to a cubie containing some number of stickers.

data ThreeDimensionalDir = DirUp | DirDown | DirLeft | DirRight | DirFront | DirBack
    deriving (Eq,Show)

-- This structure should be able to be extended to higher-dimensional hypercubies in the future
type Cubie a = (ThreeDimensionalDir -> Maybe a)

type CubieX = Int
type CubieY = Int
type CubieZ = Int

-- Because cubies at "interior" coordinates don't really exist, we stick a Maybe here in this definition
type CubieFunc a = CubieX -> CubieY -> CubieZ -> Maybe (Cubie a)
data CubieMap a = CubieMap CubeSize (CubieFunc a)

-- Utility function to determine if a sticker coordinate lies on the outside of a face
isOuterCoordinate :: CubeSize -> Int -> Bool
isOuterCoordinate size coord = (abs coord) == (size `div` 2)

-- These functions can be used to map the Cube representation defined in Rubiks to this representation
getStickersFor :: Cube a -> CubieX -> CubieY -> CubieZ -> ThreeDimensionalDir -> Maybe a
getStickersFor cube@(Cube size func) x y z d
    | xEdge && (d == DirLeft)   = Just $ func 2 y      z 
    | xEdge && (d == DirRight)  = Just $ func 4 ((-1)*y) z
    | yEdge && (d == DirUp)     = Just $ func 0 x z
    | yEdge && (d == DirDown)   = Just $ func 5 ((-1)*x) z
    | zEdge && (d == DirFront)  = Just $ func 1 x      y
    | zEdge && (d == DirBack)   = Just $ func 3 x      ((-1)*y)
    | otherwise                 = Nothing
    where xEdge:(yEdge:(zEdge:[])) = map (isOuterCoordinate size) [x,y,z] 

getCubie :: Cube a -> CubieX -> CubieY -> CubieZ -> Maybe (Cubie a)
getCubie cube@(Cube size func) x y z 
    | or $ (map (isOuterCoordinate size)) [x,y,z] = Just $ getStickersFor cube x y z
    | otherwise = Nothing 

buildCubeMap :: Cube a -> CubieMap a
buildCubeMap c@(Cube size _) = CubieMap size $ getCubie c

-- And here we have the inverse of the above transformation, going from a CubieMap to a Cube
cubiesToCube :: CubieMap a -> Cube a
cubiesToCube m@(CubieMap size func) = Cube size $ stickerAt m 

stickerAt :: CubieMap a -> FaceId -> XPos -> YPos -> a 
stickerAt m@(CubieMap size func) f x y = case f of
    0 -> fromJust $ (fromJust $ func x  k  y ) DirUp
    1 -> fromJust $ (fromJust $ func x  y  k ) DirFront
    2 -> fromJust $ (fromJust $ func k' x  y ) DirLeft
    3 -> fromJust $ (fromJust $ func x  y' k ) DirBack
    4 -> fromJust $ (fromJust $ func k  x' y ) DirRight
    5 -> fromJust $ (fromJust $ func x' k' y ) DirDown
    where k = size `div` 2
          x':(y':(k':[])) = map (*(-1)) [x,y,k] -- Defined for convencience in referencing negatives
