import Data.List
import Control.Monad.Writer
import qualified Data.Set as Set
import Data.Permute
import Codec.Picture
import Control.Applicative
import Debug.Trace

--- Counting reachable states of a 2x2

-- the 2x2 has 24 stickers, so one approach is to look at permutations of [0..23]

cubeSize :: Int
cubeSize = 2

stickersPerSide = cubeSize^2

stickerCount :: Int
stickerCount = stickersPerSide * 6

instance Ord Permute where
    compare p1 p2
        | (size p1) == (size p2) = (elems p1) `compare` (elems p2) 
        | otherwise = (size p1) `compare` (size p2)

instance Monoid Permute where
    mempty = idCube
    mappend p1 p2 = listPermute stickerCount $ map (\i -> p1 `at` i) $ elems p2
    mconcat = foldr mappend mempty . reverse

invert :: Permute -> Permute
invert p = p `mappend` p `mappend` p

idCube :: Permute
idCube = permute stickerCount

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
search (x:xs) foundPerms = traceShow ("Queue size = " ++ (show $ (length xs) + 1) ++ ", found set size = " ++ (show $ Set.size foundPerms)) $ search (newPerms ++ xs) (foldl (flip Set.insert) foundPerms newPerms)
    where newPerms = filter (flip Set.notMember $ foundPerms) $  map (\f -> x `mappend` f) moves


-- Functions for drawing a cube representation
cubeImageWidth = 600
cubeImageHeight = cubeImageWidth * (3 :: Int) `div` 4

-- Define colors
red = PixelRGBA8 255 0 0 255
black = PixelRGBA8 0 0 0 255
blue = PixelRGBA8  0 0 255 255
yellow = PixelRGBA8 255 255 0 255
green = PixelRGBA8 0 255 0 255
orange = PixelRGBA8 255 165 0 255

colors = [red, black, blue, yellow, green, orange]

--Function to get the color of a sticker number
stickerToColor :: Int -> PixelRGBA8
stickerToColor s | s > 0 = colors !! ((s - (s `mod` stickersPerSide)) `div` stickersPerSide)
    | otherwise = colors !! 0

-- Define the width/height of a face in the image
faceImageSize :: Int
faceImageSize = cubeImageWidth `div` 4

-- Define the lower-left corner of the image region which corresponds to each face
faceIdToBounds :: Int -> (Int, Int)
faceIdToBounds f = (\(fx, fy) -> (faceImageSize*fx, faceImageSize*fy)) $ [(1,1),(1,2),(2,1),(1,0),(0,1),(3,1)] !! f

-- For each face, give the order that each sticker is drawn.  Inner lists are each one face, with elements being stickers
stickerDrawOrder :: [[Int]]
stickerDrawOrder = map (\face -> map (\s -> s-1) face) [[1,2,4,3],[7,8,6,5],[10,11,9,12],[13,14,16,15],[20,17,19,18],[23,24,22,21]]

-- Define width and height of each sticker
stickerDrawWidth = faceImageSize `div` cubeSize
stickerDrawHeight = stickerDrawWidth

-- Since we ordered the stickers differently that we draw them, we need to define a permutation from the former to the latter
reorderForDrawing :: Permute -> Permute
reorderForDrawing cube = cube `mappend` (listPermute stickerCount $ join stickerDrawOrder)

-- take an x,y coord to a "face index" which is in [0..11], only six of which will be tiled with stickers
getFaceCellX :: Int -> Int
getFaceCellX = flip div $ faceImageSize

getFaceCellY :: Int -> Int
getFaceCellY = getFaceCellX

getFaceCell :: Int -> Int -> Int
getFaceCell x y = (getFaceCellY y)*4 + (getFaceCellX x)

-- take an x,y coord to a sticker index WITHIN one cell, i.e. in the range [0..3] for a 2x2
getStickerCellX :: Int -> Int
getStickerCellX = (flip div $ stickerDrawWidth) . (flip mod $ faceImageSize)

getStickerCellY :: Int -> Int
getStickerCellY = getStickerCellX

getStickerCell :: Int -> Int -> Int
getStickerCell x y = (getStickerCellY y)*cubeSize + (getStickerCellX x)

-- Need to translate a face index and sticker index into an index in our cube permutation (after it is reordered above)
-- Because not all "face cells" in the grid are actually tiled, this needs to be a maybe
faceAndStickerToListIndex :: Int -> Int -> Maybe Int
faceAndStickerToListIndex f s
    | f == 5 = Just s
    | f == 1 = Just $ s + 4
    | f == 6 = Just $ s + 8
    | f == 9 = Just $ s + 12
    | f == 4 = Just $ s + 16
    | f == 7 = Just $ s + 20
    | otherwise = Nothing 

-- Small helper function to ensure Nothings from the above function get transparent pixels
maybeStickerIndexToColor :: Maybe Int -> Permute -> PixelRGBA8
maybeStickerIndexToColor (Just i) = stickerToColor . ((flip at) i) . reorderForDrawing
maybeStickerIndexToColor Nothing = (\_ -> PixelRGBA8 0 0 0 0)

-- Finally, combine the above functions to get a function from x,y to coloo
coordsToMaybeStickerIndex :: Int -> Int -> Maybe Int
coordsToMaybeStickerIndex = liftA2 faceAndStickerToListIndex <$> getFaceCell <*> getStickerCell

coordsToColor :: Permute -> Int -> Int -> PixelRGBA8
coordsToColor cube x y = ((flip maybeStickerIndexToColor) cube) $ coordsToMaybeStickerIndex x y  

-- Use this function to draw an image of the cube
generateCubeImage :: Permute -> Image PixelRGBA8
generateCubeImage cube = generateImage (coordsToColor cube) cubeImageWidth cubeImageHeight

saveImageToProjectFolder :: String -> Permute -> IO ()
saveImageToProjectFolder fn = writePng fn . generateCubeImage
