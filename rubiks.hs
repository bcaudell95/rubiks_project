{-# LANGUAGE KindSignatures, ScopedTypeVariables, PolyKinds, TypeInType, GADTs, DataKinds, ExistentialQuantification #-}
module Rubiks where
import Data.List
import Data.Permute
import Data.Kind
import Data.Sized.Fin
import Data.Singletons.TypeLits
import qualified GHC.TypeLits as TL

-- Define our basic type with some useful synonyms
type CubeSize = Int
type FaceId = Int
-- X,Y range from -k to k, where k = CubeSize // 2
-- The origin is the center sticker on odd-order cubes, and in between the sticker 4 stickers of even-order cubes
type XPos = Int
type YPos = Int
type Color = Int
-- StickerId carries knowledge about what color a sticker is, and can be used to track where a sticker moves
type StickerId = Int

-- Generalized mapping from the FXY space to some data type on the cube
type CubeFunc a = FaceId -> XPos -> YPos -> a

data Cube a = (Show a, Eq a) => Cube CubeSize (CubeFunc a)

type IndexedCube = Cube StickerId


-- Range of x and y over a cube of a size
xyRangeForSize :: CubeSize -> [Int]
xyRangeForSize size = range 
        where k = size `div` 2
              range
                | odd size = [(-1)*k..k]
                | even size = [(-1)*k..(-1)] ++ [1..k]

-- Compute the ordered stickers of a cube
orderedElements :: Cube a -> [a]
orderedElements (Cube size stickerFunc) =  map (\(f,x,y) -> stickerFunc f x y) [(f,x,y) | f <- [0..5], x <- range, y <- range]
    where range = xyRangeForSize size

-- Instances for the Cube type
instance Show (Cube a) where
    show cube@(Cube size _) = "Cube of size " ++ (show size) ++ " with ordered elements " ++ (concat . (intersperse ",") . (map show) $ elements)
        where elements = orderedElements cube 

instance Eq (Cube a) where
    (==) c1@(Cube size1 func1) c2@(Cube size2 func2) 
        | (size1 == size2) = and [(func1 f x y) == (func2 f x y) | f <- [0..5], x <- range, y <- range]
        | otherwise = False
        where range = xyRangeForSize size1

-- Define functions to move between the spaces of (FaceId, X, Y) and StickerId
faceXYtoId :: CubeSize -> FaceId -> XPos -> YPos -> StickerId
faceXYtoId size
    | odd size = faceXYtoIdOdd size
    | otherwise = faceXYtoIdEven size

faceXYtoIdOdd :: CubeSize -> FaceId -> XPos -> YPos -> StickerId
faceXYtoIdOdd size f x y = (f*size*size) + ((y+k)*size) + (x+k)
    where k = size `div` 2

faceXYtoIdEven :: CubeSize -> FaceId -> XPos -> YPos -> StickerId
faceXYtoIdEven size f x y = (f*size*size) + ((y'+k)*size) + (x'+k)
    where k = (size `div` 2) - 1 
          x' = if x < 0 then x+1 else x
          y' = if y < 0 then y+1 else y

faceXYTupletoId :: CubeSize -> (FaceId, XPos, YPos) -> StickerId
faceXYTupletoId size (f,x,y) = faceXYtoId size f x y

idtoFaceXY :: CubeSize -> StickerId -> (FaceId, XPos, YPos)
idtoFaceXY size
    | odd size = idtoFaceXYOdd size
    | otherwise = idtoFaceXYEven size

idtoFaceXYOdd :: CubeSize -> StickerId -> (FaceId, XPos, YPos)
idtoFaceXYOdd size id = (f, x, y)
    where k = size `div` 2
          f = id `div` (size*size)
          x = (id `mod` size) - k
          y = ((id `mod` (size*size)) `div` size) - k 

idtoFaceXYEven :: CubeSize -> StickerId -> (FaceId, XPos, YPos)
idtoFaceXYEven size id = (f, x', y')
    where k = (size `div` 2) - 1
          f = id `div` (size*size)
          x = (id `mod` size) - k
          x' = if x > 0 then x else x-1
          y = ((id `mod` (size*size)) `div` size) - k 
          y' = if y > 0 then y else y-1

-- Takes a sticker id to the color of that sticker
idToColor :: CubeSize -> StickerId -> Color
idToColor size id = id `div` (size*size)

-- Construct a solved cube of an appropriate size
solvedCubeOfSize :: CubeSize -> IndexedCube
solvedCubeOfSize size = Cube size $ faceXYtoId size

-- Functions of this type take a new (face, x, y) to the old (face, x, y) that sticker was before the permutation
-- Note: PermutationFunc's can be composed using (,).  For best results, have permutations operate on disjoint stickers to ensure commutativity
type PermutationFunc = (FaceId, XPos, YPos) -> (FaceId, XPos, YPos)

-- Takes a (FaceId, XPos, YPos) tuple to a tuple of (FaceIndex, XIndex, YIndex) which accounts for the list indices of the CubeState
type FaceIndex = Int
type XIndex = Int
type YIndex = Int
fxyToIndices :: CubeSize -> (FaceId, XPos, YPos) -> (FaceIndex, XIndex, YIndex)
fxyToIndices size (f,x,y) = (f, x'+k', y'+k')
    where k = size `div` 2
          k' = if odd size then k else k-1
          x' = if x > 0 || odd size then x else x+1
          y' = if y > 0 || odd size then y else y+1 

-- Functions to apply a permutation to a cube
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

applyPerm :: IndexedCube -> PermutationFunc -> IndexedCube
applyPerm (Cube size oldFunc) perm = Cube size (\f -> (\x -> (\y -> uncurry3 oldFunc $ perm (f,x,y))))

applyPerms :: IndexedCube -> [PermutationFunc] -> IndexedCube
applyPerms = foldl applyPerm

-- Functions to generate the permutations (i.e. turns) for a cube
faceRotationPerm :: FaceId -> PermutationFunc
faceRotationPerm face = (\(f,x,y) -> (f,if f /= face then x else (-1)*y,if f /= face then y else x))

permsForCubeSize :: CubeSize -> [PermutationFunc]
permsForCubeSize size = concat $ [rightPermsForCubeSize, upPermsForCubeSize, backPermsForCubeSize] <*> pure size

rightPermsForCubeSize :: CubeSize -> [PermutationFunc]
rightPermsForCubeSize size = rightFaceTurn : map (rightSliceMove size) sliceIndexRange
    where rightFaceTurn = ((rightSliceMove size 0) . (faceRotationPerm 4))
          sliceIndexRange = [1..size-2]
          
type SliceIndex = Int
rightSliceMove :: CubeSize -> SliceIndex -> PermutationFunc 
rightSliceMove size slice = (\pos@(f,x,y) -> if notInSlice pos then pos else (f' pos, x' pos, y' pos)) 
    where notInSlice = (\pos -> not $ inRSlice size slice pos)
          f' = (\pos@(f,x,y) -> [1,5,2,0,4,3] !! f)
          x' = (\pos@(f,x,y) -> if f `notElem` [1,5] then x else (-1)*x)
          y' = (\pos@(f,x,y) -> if f `notElem` [1,5] then y else (-1)*y)

inRSlice :: CubeSize -> SliceIndex -> (FaceIndex, XPos, YPos) -> Bool
inRSlice size slice (f,x,y) = (f `elem` [0,1,3] && x == sliceX) || (f == 5 && x == (-1)*sliceX)
    where k = size `div` 2
          range = if odd size then [k,k-1..(-1)*(k-1)] else [k,k-1..1] ++ [-1,-2..(-1)*(k-1)]
          sliceX = range !! slice

upPermsForCubeSize :: CubeSize -> [PermutationFunc]
upPermsForCubeSize size = upFaceTurn : map (upSliceMove size) sliceIndexRange
    where upFaceTurn = ((upSliceMove size 0) . (faceRotationPerm 0))
          sliceIndexRange = [1..size-2]

upSliceMove :: CubeSize -> SliceIndex -> PermutationFunc
upSliceMove size slice = (\pos@(f,x,y) -> if notInSlice pos then pos else (f' pos, (-1)*y, x)) 
    where notInSlice = not . (inUSlice size slice)
          f' = (\pos@(f,x,y) -> [0,4,1,2,3,5] !! f)

inUSlice :: CubeSize -> SliceIndex -> (FaceIndex, XPos, YPos) -> Bool
inUSlice size slice = (\pos@(f,x,y) -> (f == 1 && y == z) || (f == 2 && x == z) || (f == 3 && y == (-1)*z) || (f == 4 && x == (-1)*z))
    where k = size `div` 2
          range = if odd size then [k,k-1..(-1)*(k-1)] else [k,k-1..1] ++ [-1,-2..(-1)*(k-1)]
          z = range !! slice -- Will be either an x or y coord depending on the face
          

backPermsForCubeSize :: CubeSize -> [PermutationFunc]
backPermsForCubeSize size = backFaceTurn : map (backSliceMove size) sliceIndexRange
    where backFaceTurn = ((backSliceMove size 0) . (faceRotationPerm 3))
          sliceIndexRange = [1..size-2]

backSliceMove :: CubeSize -> SliceIndex -> PermutationFunc
backSliceMove size slice = (\pos@(f,x,y) -> if notInSlice pos then pos else (f' pos, x, y)) 
    where notInSlice = (\pos -> not $ inBSlice size slice pos) 
          f' = (\(f,_,_) -> [4,1,0,3,5,2] !! f)

inBSlice :: CubeSize -> SliceIndex -> (FaceIndex, XPos, YPos) -> Bool
inBSlice size slice (f,x,y) = f `elem` [0,2,4,5] && y == sliceY
    where k = size `div` 2
          range = if odd size then [k,k-1..(-1)*(k-1)] else [k,k-1..1] ++ [-1,-2..(-1)*(k-1)]
          sliceY = range !! slice

-- Reverses a move by applying it three times
reverseMove :: PermutationFunc -> PermutationFunc
reverseMove move = move . move . move

-- Convenience functions to make writing these permuation moves easier
rightMove :: CubeSize -> Int -> PermutationFunc
rightMove size i = (rightPermsForCubeSize size) !! i
upMove :: CubeSize -> Int -> PermutationFunc
upMove size i = (upPermsForCubeSize size) !! i
backMove :: CubeSize -> Int -> PermutationFunc
backMove size i = (backPermsForCubeSize size) !! i

-- Utility function to take every other item from a list, starting with the first
everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:[]) = [x]
everyOther (x:y:xs) = [x] ++ (everyOther xs)

-- And a variant that does the same thing starting with the second
everyOther' :: [a] -> [a]
everyOther' = everyOther . tail

-- Checkerboard pattern moves just for fun and testing
checkerboardPerms :: CubeSize -> [PermutationFunc]
checkerboardPerms size = concat $ zipWith (\a b -> [a,b]) moves moves 
    where rMoves = rightPermsForCubeSize size
          uMoves = upPermsForCubeSize size
          bMoves = backPermsForCubeSize size
          moves = concat $ map everyOther' [rMoves, uMoves, bMoves]


