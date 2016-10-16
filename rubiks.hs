{-# LANGUAGE KindSignatures, ScopedTypeVariables, PolyKinds, TypeInType, GADTs, DataKinds, ExistentialQuantification #-}
import Data.List
import Data.Permute
import Data.Kind
import Data.Sized.Fin
import Data.Singletons.TypeLits
import qualified GHC.TypeLits as TL

-- Define our basic type with some useful synonyms
type CubeSize = Int
type Permutation = [Int]
type StickerGroups = [Permutation]
data Cube = Cube CubeSize StickerGroups
    deriving Show

-- Functions to build a solved cube of arbitrary size
solvedCubeOfSize :: CubeSize -> Cube
solvedCubeOfSize size = Cube size (stickerGroupsForSize size) 

stickerGroupsForSize :: CubeSize -> StickerGroups
stickerGroupsForSize size = concat $ map (stickerGroupsForSizeAtRow size) [0..x]
    where x = (stickerGroupCountForSize size) - 1

stickerGroupCountForSize :: CubeSize -> Int
stickerGroupCountForSize size
    | odd size = (size `div` 2) + 1
    | even size = size `div` 2

stickerGroupsForSizeAtRow :: CubeSize -> Int -> StickerGroups
stickerGroupsForSizeAtRow size x
    | (odd size && x == 0) = [[0..5]]
    | odd size = [[0..23]] ++ [[0..47] | _ <- [0..x-2]] ++ [[0..23]]
    | even size = [[0..47] | _ <- [0..x-1]] ++ [[0..23]] 

-- Validates that the number of stickers is correct
expectedStickerCountForSize :: CubeSize -> Int
expectedStickerCountForSize size = 6*(size^2)

isValidStickerCount :: Cube -> Bool
isValidStickerCount (Cube size stickers) = (expectedStickerCountForSize size) == (sum $ map length stickers)

-- Function that generates a sequence of turns for an arbitrarily-sized cube
-- These turns are expressed as permutations of the sticker groups
createTurnPermutations :: CubeSize -> [[Permutations]]
