import Rubiks
import Codec.Picture

type ImageHeight = Int
type ImageWidth = Int
type ImageFunc = XPos -> YPos -> PixelRGBA8
data CubeImage = CubeImage ImageWidth ImageHeight ImageFunc

type PixelX = Int
type PixelY = Int

-- Width and Height in pixels per sticker square
stickerSquareDim :: Int
stickerSquareDim = 25

type Filename = String

drawCube :: Filename -> Cube -> IO ()
drawCube fn = (writePng ("images/" ++ fn)) . buildImageForCube 

-- Define a way to go from a Cube object to a CubeImage representation
buildImageForCube :: Cube -> Image PixelRGBA8
buildImageForCube cube@(Cube size _) = generateImage imageFunc width height
    where width = imageWidthForSize size
          height = imageHeightForSize size
          imageFunc = pixelCoordsToColor cube
          
imageWidthForSize :: CubeSize -> ImageWidth
imageWidthForSize size = stickerSquareDim*size*4 -- In our image, there will be four sides layed out horizontally

imageHeightForSize :: CubeSize -> ImageWidth
imageHeightForSize size = stickerSquareDim*size*3 -- and 3 sides vertically
 
-- Functions to determine the FXY coords for a pixel in tbe image
faceForPoint :: CubeSize -> PixelX -> PixelY -> Maybe FaceId
faceForPoint size px py
    | (fx,fy) == (1,1) = Just 0
    | (fx,fy) == (1,2) = Just 1
    | (fx,fy) == (0,1) = Just 2
    | (fx,fy) == (1,0) = Just 3
    | (fx,fy) == (2,1) = Just 4
    | (fx,fy) == (3,1) = Just 5
    | otherwise = Nothing
        where fx = px `div` (stickerSquareDim*size) -- If we imagine a grid of 12 "face sections", this is the x coord we're in on that grid
              fy = py `div` (stickerSquareDim*size) --   and this is the y coord on that grid.  Some face sections go to face values, others transparent

xForPoint :: CubeSize -> PixelX -> XPos
xForPoint size px = range !! ((px `mod` faceWidth) `div` stickerSquareDim)
    where range = xyRangeForSize size -- This function accounts for our strange XY coords on cube faces
          faceWidth = stickerSquareDim*size

-- The complementation by size-1 here accounts for the face that in the image space, +y is down, but in the cube space +y is up
yForPoint :: CubeSize -> PixelY -> YPos
yForPoint size py = range !! ((size-1) - ((py `mod` faceHeight) `div` stickerSquareDim))
    where range = xyRangeForSize size -- This function accounts for our strange XY coords on cube faces
          faceHeight = stickerSquareDim*size

-- Table of conversions from Color as an integer to an RGB pixel
colorIndexToPixel :: Color -> PixelRGBA8 
colorIndexToPixel c = [black, red, green, orange, blue, yellow] !! c

red = PixelRGBA8 255 0 0 255
black = PixelRGBA8 0 0 0 255
blue = PixelRGBA8  0 0 255 255
yellow = PixelRGBA8 255 255 0 255
green = PixelRGBA8 0 255 0 255
orange = PixelRGBA8 255 165 0 255
transparent = PixelRGBA8 0 0 0 0

-- Combine all of this to go from PixelX, PixelY to a color
pixelCoordsToColor :: Cube -> PixelX -> PixelY -> PixelRGBA8
pixelCoordsToColor (Cube size stickerFunc) px py
    | isJust face = (\(Just f) -> colorIndexToPixel . (idToColor size) $ stickerFunc f cx cy) $ face 
    | otherwise = transparent
        where face = faceForPoint size px py
              cx = xForPoint size px
              cy = yForPoint size py

-- Small helper function to see if a Maybe is a Just
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- Functions for testing, draw images for all basic moves of a sized cube
drawMovesOfSize :: CubeSize -> IO [()]
drawMovesOfSize size = sequence $ [drawCube (concat ["solved", show size, ".png"]) start] ++ [drawCube fn (applyPerm start perm) | (fn, perm) <- allNamesAndMoves] 
    where start = solvedCubeOfSize size
          rightNamesAndMoves = [("R" ++ show i ++ "_" ++ show size ++ ".png", (rightPermsForCubeSize size) !! i) | i <- [0..size-2]]
          upNamesAndMoves = [("U" ++ show i ++ "_" ++ show size ++ ".png", (upPermsForCubeSize size) !! i) | i <- [0..size-2]]
          backNamesAndMoves = [("B" ++ show i ++ "_" ++ show size ++ ".png", (backPermsForCubeSize size) !! i) | i <- [0..size-2]]
          allNamesAndMoves = concat [rightNamesAndMoves, upNamesAndMoves, backNamesAndMoves]
          
