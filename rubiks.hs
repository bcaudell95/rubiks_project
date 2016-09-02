import Data.List
import Control.Monad.Writer
import qualified Data.Set as Set
import Data.Permute

--- Counting reachable states of a 2x2

-- the 2x2 has 24 stickers, so one approach is to look at permutations of [0..23]

cubeSize :: Int
cubeSize = 2

stickerCount :: Int
stickerCount = cubeSize^2 * 6

instance Ord Permute where
    compare p1 p2
        | (size p1) == (size p2) = (elems p1) `compare` (elems p2) 
        | otherwise = (size p1) `compare` (size p2)

instance Monoid Permute where
    mempty = permute stickerCount
    mappend p1 p2 = listPermute stickerCount $ map (\i -> p1 `at` i) $ elems p2
    mconcat = foldr mappend mempty . reverse

invert :: Permute -> Permute
invert p = p `mappend` p `mappend` p

-- Now we define the permutations that represent the six rotations of the 2x2 cube
-- When I originally wrote this, the permutations were 1-based, but the Permute library expects 0-based arrays, hence the map (-1)
top :: Permute
top =  listPermute stickerCount $ map (\i -> i-1) [4,1,2,3, 17,18,7,8, 5,6,11,12, 9,10,15,16, 13,14,19,20, 21,22,23,24]

back :: Permute
back = listPermute stickerCount $ map (\i -> i-1) [10,11,3,4, 8,5,6,7, 9,23,24,12, 13,14,15,16, 2,18,19,1, 21,22,20,17]

right :: Permute
right = listPermute stickerCount $ map (\i -> i-1) [1,14,15,4, 3,6,7,2, 12,9,10,11, 13,22,23,16, 17,18,19,20, 21,8,5,24]

front :: Permute
front = listPermute stickerCount $ map (\i -> i-1) [1,2,18,19, 5,6,7,8, 4,10,11,3, 16,13,14,15, 17,21,22,20, 12,9,23,24]

left :: Permute
left = listPermute stickerCount $ map (\i -> i-1) [7,2,3,6, 5,24,21,8, 9,10,11,12, 1,14,15,4, 20,17,18,19, 13,22,23,16]

bottom :: Permute
bottom = listPermute stickerCount $ map (\i -> i-1) [1,2,3,4, 5,6,11,12, 9,10,15,16, 13,14,19,20, 17,18,7,8, 24,21,22,23]

-- we only include three of the six moves here because, for instance, a left rotation is the same as 3 right rotations and a whole-cube turn.  Thus, including both would be redundant.
-- This effectively "locks" the 1-6-17 (in 1-based numbering) corner piece in place, letting the other corners move freely
moves :: [Permute]
moves = [right, back, bottom]

-- Now that we have defined the monoidal permutations, we set up a Writer monad to keep track of the rotations that get applied
-- This function allows us to quickly turn the above 6 permutations into monadic functions
rotate :: Permute -> String -> Permute -> Writer String Permute
rotate r s p = writer (p `mappend` r, s) 

rotateTop = rotate top "U"
rotateBack = rotate back "B"
rotateRight = rotate right "R"
rotateFront = rotate front "F"
rotateLeft = rotate left "L"
rotateBottom = rotate bottom "D"

-- Use a BFS to try to list all the reachable permutations of the cube.
-- Note that this will overcount by a factor of 24 because for each permutation, we will get a copy corresponding to every perspective view of the cube as a whole, of which there are 24
allPerms :: [Permute]
allPerms = search [mempty :: Permute] (Set.fromList [mempty :: Permute])

search :: [Permute] -> Set.Set Permute -> [Permute]
search [] foundPerms = Set.elems foundPerms
-- Note that the Set.foldl could be replaced with a Set.union for more clarity, but I was curious if this would be faster.  
search (x:xs) foundPerms = search (xs ++ (Set.elems newPerms)) (Set.foldl (flip Set.insert) foundPerms newPerms)
    where newPerms = Set.filter (flip Set.notMember $ foundPerms) $ Set.fromList $ map (\f -> x `mappend` f) moves
