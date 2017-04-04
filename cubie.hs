{-# LANGUAGE GADTs #-}

module Cubie where
import Rubiks
import Data.Maybe (fromJust, isJust)
import Control.Applicative (liftA2)
import Data.List (sort)

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
data CubieMap c s where
    CubieMap :: CubeSize -> (CubieComboFunc c s) -> CubieMap c s

instance (Show c, Show s) => Show (CubieMap c s) where
    show = show . (fmap (\(_,_,f) -> stickersOnCubie f)) . listCubies

lookupStickerValue :: CubieMap c s -> (CubieX, CubieY, CubieZ, ThreeDimensionalDir) -> Maybe s
lookupStickerValue (CubieMap size func) (x,y,z,d) = do
    (_, stickerFunc) <- func x y z
    stickerFunc d

lookupCubieValue :: CubieMap c s -> (CubieX, CubieY, CubieZ) -> Maybe c
lookupCubieValue (CubieMap size func) (x,y,z) = do
    (val, _) <- func x y z
    return val

stickersOnCubie :: CubieStickerFunc s -> [Maybe s]
stickersOnCubie func = [func] <*> [DirUp, DirFront, DirLeft, DirBack, DirRight, DirDown]

--Pseudo-Functor abilities that map over the cubie values and the sticker values
mapOverCubies :: (c1 -> c2) -> CubieMap c1 s -> CubieMap c2 s
mapOverCubies f (CubieMap size f1) = CubieMap size (\x y z -> fmap (\(a,b) -> (f a, b)) $ f1 x y z)

mapOverStickers :: (s1 -> s2) -> CubieMap c s1 -> CubieMap c s2
mapOverStickers f (CubieMap size f1) = CubieMap size (\x y z -> fmap (\(a,b) -> (a, (\d -> fmap f $ b d))) $ f1 x y z)

isOuterCoordinate :: CubeSize -> Int -> Bool
isOuterCoordinate size c = (abs c) == (size `div` 2)

-- These functions can be used to map the Cube representation defined in Rubiks to this representation
getStickersFor :: Cube a -> CubieX -> CubieY -> CubieZ -> ThreeDimensionalDir -> Maybe a
getStickersFor cube@(Cube size func) x y z d
    | (x == k') && (d == DirLeft)   = Just $ func 2 y           z     
    | (x == k ) && (d == DirRight)  = Just $ func 4 ((-1)*y)    z
    | (y == k ) && (d == DirUp)     = Just $ func 0 x           z
    | (y == k') && (d == DirDown)   = Just $ func 5 ((-1)*x)    z
    | (z == k') && (d == DirFront)  = Just $ func 1 x           y    
    | (z == k ) && (d == DirBack)   = Just $ func 3 x           ((-1)*y)
    | otherwise                     = Nothing
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
    1 -> fromJust $ snd (fromJust $ func x  y  k') $ DirFront
    2 -> fromJust $ snd (fromJust $ func k' x  y ) $ DirLeft
    3 -> fromJust $ snd (fromJust $ func x  y' k ) $ DirBack
    4 -> fromJust $ snd (fromJust $ func k  x' y ) $ DirRight
    5 -> fromJust $ snd (fromJust $ func x' k' y ) $ DirDown
    where k = size `div` 2
          x':(y':(k':[])) = map (*(-1)) [x,y,k] -- Defined for convencience in referencing negatives

getCornersFor :: CubieMap c s -> [((CubieX, CubieY, CubieZ), [Maybe s])] 
getCornersFor c@(CubieMap size func) = (fmap (\(a,_,f) -> (a, stickersOnCubie f))) unwrapped
    where k = size `div` 2
          range = [k, ((-1)*k)]
          maybeList = [((x,y,z), func x y z) | x <- range, y <- range, z <- range] 
          filtered = filter (isJust . snd) maybeList
          unwrapped = map (\(a,Just (b,c)) -> (a,b,c)) filtered

-- Now we lift the generators (legal moves) from the sticker-based data-type to the cube-based one
type CubiePermutationFunc s = CubieMap () s -> CubieMap () s

cubieMove :: (CubeSize -> Int -> PermutationFunc) -> CubieMap () s -> Int -> CubieMap () s
cubieMove func cube@(CubieMap size _) slice = buildStdCubieMap . (flip applyPerm (func size slice)) . cubiesToCube $ cube

cubieMoveUp :: CubieMap () s -> Int -> CubieMap () s
cubieMoveUp = cubieMove upMove 

cubieMoveFront :: CubieMap () s -> Int -> CubieMap () s
cubieMoveFront = cubieMove frontMove 

cubieMoveLeft :: CubieMap () s -> Int -> CubieMap () s
cubieMoveLeft = cubieMove leftMove 

cubieMoveBack :: CubieMap () s -> Int -> CubieMap () s
cubieMoveBack = cubieMove backMove 

cubieMoveRight :: CubieMap () s -> Int -> CubieMap () s
cubieMoveRight = cubieMove rightMove 

cubieMoveDown :: CubieMap () s -> Int -> CubieMap () s
cubieMoveDown = cubieMove downMove 

cubieMoveX :: CubiePermutationFunc s
cubieMoveX = (flip $ cubieMove (flip $ const xRotation)) 0

cubieMoveY :: CubiePermutationFunc s
cubieMoveY = (flip $ cubieMove (flip $ const yRotation)) 0

cubieMoveZ :: CubiePermutationFunc s
cubieMoveZ = (flip $ cubieMove (flip $ const zRotation)) 0

reverseCubieMove :: CubiePermutationFunc s -> CubiePermutationFunc s
reverseCubieMove func = func . func . func

removeCubieData :: CubieMap c s -> CubieMap () s
removeCubieData = mapOverCubies $ const ()

-- In order to figure out where different cubies move during permutation without rewriting my sticker
--  permutation code, we will track the stickers on a cubie before and after permutation
idCubiesByStickers :: (Ord s) => CubieMap c s -> CubieMap [s] s
idCubiesByStickers cube@(CubieMap size func) = CubieMap size $ cubieLabelingFunction cube

cubieLabelingFunction :: (Ord s) => CubieMap c s -> CubieComboFunc [s] s
cubieLabelingFunction cube@(CubieMap size func) x y z = oldCubieVal >>= (return . extractStickerData . filterMapSort . stickersOnCubie . snd)
    where oldCubieVal = func x y z 
          filterMapSort = sort . (map fromJust) . (filter isJust)
          extractStickerData = flip (,) $ snd . fromJust $ oldCubieVal

findCubie :: (Eq c) => CubieMap c s -> c -> (CubieX, CubieY, CubieZ)
findCubie cube@(CubieMap size func) target = fst $ head $ filter (\(pos, val) -> target == val) $ list 
    where list = map (\(pos, val, _) -> (pos, val)) $ listCubies cube
    
labelCubiesWithNextPositions :: (Ord s) => CubieMap c s -> CubiePermutationFunc s -> CubieMap (CubieX, CubieY, CubieZ) s
labelCubiesWithNextPositions cube@(CubieMap size func) perm = mapOverCubies (findCubie $ idCubiesByStickers $ perm $ removeCubieData cube) $ idCubiesByStickers cube

-- Utility function to merge two CubieMaps with the same sticker data and different cubie data
zipCubieMaps :: CubieMap c1 s -> CubieMap c2 s -> CubieMap (c1, c2) s
zipCubieMaps c1@(CubieMap size1 func1) c2@(CubieMap size2 func2)
    | size1 == size2    = CubieMap size1 newFunc
    | otherwise         = error "mismatched cube sizes"
        where newFunc   = (\x -> (\y -> (\z -> zipTuples <$> func1 x y z <*> func2 x y z)))

-- Makes no assumptions about the equality of the second components, just takes the first
zipTuples :: (c1, s) -> (c2, s) -> ((c1, c2), s)
zipTuples (a,b) (c,_) = ((a,c),b)
