module Display where
import Rubiks
import Codec.Picture
import Data.Maybe (isJust)


type ImageHeight = Int
type ImageWidth = Int
type ImageFunc = XPos -> YPos -> PixelRGBA8
data CubeImage = CubeImage ImageWidth ImageHeight ImageFunc

type PixelX = Int
type PixelY = Int

-- Typeclass for pixels that we can use in our rendering -- requies specification of border and background "colors"
class (Pixel p, PngSavable p) => CubeRenderable p where
    transparent     :: p
    border          :: p

-- Define one instance for now
instance CubeRenderable PixelRGBA8 where
    transparent = PixelRGBA8 0 0 0 0
    border = PixelRGBA8 0 0 0 255

-- Width and Height in pixels per sticker square
stickerSquareDim :: Int
stickerSquareDim = 10

type Filename = String

drawIndexedCube :: Filename -> IndexedCube -> IO ()
drawIndexedCube fn cube@(Cube size _) = drawCubeWith fn cube (colorIndexToPixel . (idToColor size))

drawPixelCube :: (CubeRenderable p) => Filename -> (Cube p) -> IO()
drawPixelCube fn = (writePng ("images/" ++ fn)) . buildImageForCube

drawCubeWith :: (CubeRenderable p) => Filename -> (Cube a) -> (a -> p) -> IO ()
drawCubeWith fn cube func = drawPixelCube fn $ fmap func cube

-- One function that can be used with drawCubeWith to show the sticker labels
-- Encodes the sticker label to a base-256 three-digit number, then uses those digits as the pixel colors
-- Note that this will only work properly if there are fewer that 256^3 stickers, but that isn't a problem with most cubes
labelToRgbColor :: StickerId -> PixelRGBA8
labelToRgbColor x = PixelRGBA8 r g b 255
    where r = fromIntegral $ x `div` (256*256)
          g = fromIntegral $ (x `mod` (256*256)) `div` 256
          b = fromIntegral $ x `mod` 256

-- Generalization of above function to any CubeRenderable cube
buildImageForCube :: (CubeRenderable p) => (Cube p) -> (Image p)
buildImageForCube cube@(Cube size _) = generateImage borderFunc width height
    where width = imageWidthForSize size
          height = imageHeightForSize size
          imageFunc = pixelCoordsToColor cube
          borderFunc = drawStickerBordersOverImage imageFunc 
         
drawStickerBordersOverImage :: (CubeRenderable p) => (PixelX -> PixelY -> p) -> PixelX -> PixelY -> p
drawStickerBordersOverImage imageFunc x y
    | (onXBorder || onYBorder) = if isTransparent then transparent else border
    | otherwise = imagePixel
        where onXBorder = (x `mod` stickerSquareDim `elem` [0,stickerSquareDim-1])
              onYBorder = (y `mod` stickerSquareDim `elem` [0,stickerSquareDim-1])
              imagePixel = imageFunc x y
              isTransparent = (imagePixel == transparent)
 
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
colorIndexToPixel c = [white, red, green, orange, blue, yellow] !! c

red = PixelRGBA8 255 0 0 255
black = PixelRGBA8 0 0 0 255
blue = PixelRGBA8  0 0 255 255
yellow = PixelRGBA8 255 255 0 255
green = PixelRGBA8 0 255 0 255
orange = PixelRGBA8 255 165 0 255
white = PixelRGBA8 255 255 255 255

-- Combine all of this to go from PixelX, PixelY to a color
pixelCoordsToColor :: (CubeRenderable p) => (Cube p) -> PixelX -> PixelY -> p
pixelCoordsToColor (Cube size cubeFunc) px py
    | isJust face = (\(Just f) -> cubeFunc f cx cy) $ face 
    | otherwise = transparent
        where face = faceForPoint size px py
              cx = xForPoint size px
              cy = yForPoint size py

-- Functions for testing, draw images for all basic moves of a sized cube
drawMovesOfSize :: CubeSize -> IO [()]
drawMovesOfSize size = sequence $ [drawIndexedCube (concat ["solved", show size, ".png"]) start] ++ [drawIndexedCube fn (applyPerm start perm) | (fn, perm) <- allNamesAndMoves] 
    where start = solvedCubeOfSize size
          rightNamesAndMoves = [("R" ++ show i ++ "_" ++ show size ++ ".png", (rightPermsForCubeSize size) !! i) | i <- [0..size-2]]
          upNamesAndMoves = [("U" ++ show i ++ "_" ++ show size ++ ".png", (upPermsForCubeSize size) !! i) | i <- [0..size-2]]
          backNamesAndMoves = [("B" ++ show i ++ "_" ++ show size ++ ".png", (backPermsForCubeSize size) !! i) | i <- [0..size-2]]
          allNamesAndMoves = concat [rightNamesAndMoves, upNamesAndMoves, backNamesAndMoves]
          
