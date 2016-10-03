{-# LANGUAGE KindSignatures, ScopedTypeVariables, PolyKinds, TypeInType, GADTs, DataKinds #-}
import Data.List
import Data.Permute
import Data.Kind
import Data.Sized.Fin

-- Define our basic type with some useful synonyms
type CubeSize = Int
type Permutation = [Int]
data Cube :: Nat -> * where 
    Cube :: Permutation -> Cube n
        deriving (Show, Eq)

--cubeLength :: (Cube n) -> Int
--cubeLength (CubeCon _) = n

--lengthOfCube :: Cube n -> Int
--lengthOfCube (CubeCon n p) = n

--instance Monoid (Cube n) where
--    mempty = CubeCon n [0..n]
--    mappend p1 p2 = p1

-- Define a smart constructor for the Cube type that validates the perm length and values
expectedPermLength :: CubeSize -> Int
expectedPermLength s = 6*s^2

--isValidPerm :: Permutation -> Bool
--isValidPerm p = (sort p) == [0..((length p)-1)]
--
--isValidCubeDesc :: CubeSize -> Permutation -> Bool
--isValidCubeDesc s p = ((length p) == (expectedPermLength s)) && (isValidPerm p)

--makeCube :: CubeSize -> Permutation -> Cube
--makeCube s p
--    | isValidCubeDesc s p = Cube s p
--    | otherwise = error "Invalid cube description"

-- Because we have characterized a cube by its permutation of stickers, we can write
--  a Monoid instance which combines two permutations/cubes into one
--instance Monoid Cube where
--    mempty

-- Define functions to create the "generators" for the cube group
-- These are essentially the possible turns of the cube, but we modify them such that
--      they "fix" one corner in space to ensure there is a unique representation for each
--      cube permutation.

