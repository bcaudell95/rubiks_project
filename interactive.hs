module RubiksInteractive where
import RubiksAFrame hiding (main) 
import Rubiks
import Cubie
import Display

import Data.Maybe
import Data.List
import Data.Char
import System.Process
import System.Console.Readline

-- Translate a user-written description of the move to a PermutationFunc
-- Examples:
--      R / R0      -> clockwise right-face turn
--      R' / R'0    -> counter-clockwise right-face turn
--      R1 / R'1    -> turns in either direction of the slice one-layer in from the right face (direction relative to right face)
parseMove :: String -> Maybe (CubiePermutationFunc s)
parseMove text = do
    category <- moveCategory $ head text 
    let afterCategory = tail text

    let shouldReverse = (not $ null afterCategory) && (head afterCategory) == '\''
    let afterReverse = if shouldReverse then tail afterCategory else afterCategory

    let idx = if null afterReverse then 0 else (read afterReverse) :: Int

    let move = category idx
    return $ if shouldReverse then reverseCubieMove move else move

moveCategory :: Char -> Maybe (Int -> CubiePermutationFunc s)
moveCategory 'R' = Just $ flip cubieMoveRight
moveCategory 'L' = Just $ flip cubieMoveLeft 
moveCategory 'U' = Just $ flip cubieMoveUp
moveCategory 'D' = Just $ flip cubieMoveDown
moveCategory 'F' = Just $ flip cubieMoveFront
moveCategory 'B' = Just $ flip cubieMoveBack
moveCategory 'X' = Just $ const $ cubieMoveX 
moveCategory 'Y' = Just $ const $ cubieMoveY
moveCategory 'Z' = Just $ const $ cubieMoveZ
moveCategory _ = Nothing

-- Uses the above function to apply a user-written sequence of moves to a cube
-- This fold trick taken from Michael Kohl at http://stackoverflow.com/questions/4342013/the-composition-of-functions-in-a-list-of-functions
parseAndApply :: IndexedCube -> String -> IndexedCube
parseAndApply cube@(Cube size _) moves = cubiesToCube newCubieMap 
    where newCubieMap   = cubiePermFunc oldCubieMap
          cubiePermFunc = foldl (flip (.)) id $ map (fromJust . parseMove) $ words moves 
          oldCubieMap   = buildStdCubieMap cube

-- Takes a starting cube and a sequence of commands and translates it into a RawAnimationCubieMap
buildRawAnimationCubieMap :: IndexedCube -> String -> RawAnimationCubieMap StickerId 
buildRawAnimationCubieMap cube command = buildAnimationSequenceCubeFunc listOfPerms $ buildStdCubieMap cube
    where listOfPerms = map (fromJust . parseMove) $ words command 

-- Parses a sequence of commands into a sequence of raw animations
getRawAnimationSequence :: CubeSize -> [String] -> CubieX -> CubieY -> CubieZ -> [Maybe (AnimAxis, AnimDir)]
getRawAnimationSequence size commands x y z = fmap (\comm -> getSingleRawAnimation size comm x y z) commands

-- Parses a single user input, but this time turns it into a Cubie function denoting a possible raw animation move
getSingleRawAnimation :: CubeSize -> String -> CubieX -> CubieY -> CubieZ -> Maybe (AnimAxis, AnimDir)
getSingleRawAnimation size command x y z = do
    let (firstChar, afterFirst) = (head command, tail command) 
    let shouldReverse = (not $ null afterFirst) && (head afterFirst) == '\''
    let afterReverse = if shouldReverse then tail afterFirst else afterFirst
    let slice = if null afterReverse then 0 else (read afterReverse) :: Int
   
    foo@(axis, dir) <- rawAnimationCategory size firstChar x y z slice
    if shouldReverse then return (axis, reverseAnimDir dir) else return foo
    
    
rawAnimationCategory :: CubeSize -> Char -> CubieX -> CubieY -> CubieZ -> Int -> Maybe (AnimAxis, AnimDir)
rawAnimationCategory size 'R' x y z slice = if (slice < size) && ((xyRangeForSize size) !! ((size - 1) - slice)) == x then Just (AxisX, Backwards) else Nothing
rawAnimationCategory size 'L' x y z slice = if (slice < size) && ((xyRangeForSize size) !! slice) == x                then Just (AxisX, Forwards)  else Nothing
rawAnimationCategory size 'U' x y z slice = if (slice < size) && ((xyRangeForSize size) !! ((size - 1) - slice)) == y then Just (AxisY, Backwards) else Nothing
rawAnimationCategory size 'D' x y z slice = if (slice < size) && ((xyRangeForSize size) !! slice) == y                then Just (AxisY, Forwards)  else Nothing
rawAnimationCategory size 'B' x y z slice = if (slice < size) && ((xyRangeForSize size) !! ((size - 1) - slice)) == z then Just (AxisZ, Forwards)  else Nothing
rawAnimationCategory size 'F' x y z slice = if (slice < size) && ((xyRangeForSize size) !! slice) == z                then Just (AxisZ, Backwards) else Nothing
rawAnimationCategory size 'X' _ _ _ _ = Just (AxisX, Backwards)
rawAnimationCategory size 'Y' _ _ _ _ = Just (AxisY, Backwards)
rawAnimationCategory size 'Z' _ _ _ _ = Just (AxisZ, Backwards)

--
-- Second attempt at proper animation, using cubie movement
--

-- There is a subset of cubies such that at least one must be moved in every possible move
-- We form such a subset and will check each one to figure out which move occurred for the sake of animation
type SignalCubie = (CubieX, CubieY, CubieZ)

listOfSignalCubies :: CubeSize -> [(SignalCubie, SignalCubie, SignalCubie)]
listOfSignalCubies size = [((k,k,i), (k,i,k), (i,k,k)) | i <- xyRangeForSize size]
    where k = size `div` 2

-- We now look at a tuple of three of those, and check if a specific expected move occurred
checkSignalCubieTuple :: (Eq s) => CubieMap c s -> CubieMap c s -> (SignalCubie, SignalCubie, SignalCubie) -> Maybe (AnimAxis, AnimDir, Int)
checkSignalCubieTuple before@(CubieMap size _) after ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3))
    | (fromJust $ lookupStickerValue before (x1, y1, z1, DirUp))    == (fromJust $ lookupStickerValue after ((-1)*x1, y1, z1, DirLeft))    = Just (AxisZ, Forwards,  z1) 
    | (fromJust $ lookupStickerValue before (x1, y1, z1, DirUp))    == (fromJust $ lookupStickerValue after (x1, (-1)*y1, z1, DirRight))   = Just (AxisZ, Backwards, z1) 
    | (fromJust $ lookupStickerValue before (x2, y2, z2, DirBack))  == (fromJust $ lookupStickerValue after (x2, y2, (-1)*z2, DirRight))   = Just (AxisY, Forwards,  y2) 
    | (fromJust $ lookupStickerValue before (x2, y2, z2, DirBack))  == (fromJust $ lookupStickerValue after ((-1)*x2, y2, z2, DirLeft))    = Just (AxisY, Backwards, y2) 
    | (fromJust $ lookupStickerValue before (x3, y3, z3, DirUp))    == (fromJust $ lookupStickerValue after (x3, (-1)*y3, z3, DirBack))    = Just (AxisX, Forwards,  x3) 
    | (fromJust $ lookupStickerValue before (x3, y3, z3, DirUp))    == (fromJust $ lookupStickerValue after (x3, y3, (-1)*z3, DirFront))   = Just (AxisX, Backwards, x3) 
    | otherwise = Nothing
    where k = size `div` 2 

getAnimationForPermutation :: (Eq s) => CubiePermutationFunc s -> CubieMap () s -> Maybe (AnimAxis, AnimDir, Int)
getAnimationForPermutation perm cube@(CubieMap size _) = foldl foldFunc Nothing checkedSignals
    where signals = listOfSignalCubies size
          checkedSignals = map (checkSignalCubieTuple cube (perm cube)) signals
          foldFunc = (\a b -> if isJust a then a else b)

mapOverUniformThreeTuple :: (a -> b) -> (a,a,a) -> (b,b,b)
mapOverUniformThreeTuple func (x,y,z) = (func x, func y, func z)

singleCubieAnimation :: Maybe (AnimAxis, AnimDir, Int) -> CubieX -> CubieY -> CubieZ -> Maybe (AnimAxis, AnimDir)
singleCubieAnimation (Just (AxisX, dir, target)) x _ _  = if x == target then Just (AxisX, dir) else Nothing
singleCubieAnimation (Just (AxisY, dir, target)) _ y _  = if y == target then Just (AxisY, dir) else Nothing
singleCubieAnimation (Just (AxisZ, dir, target)) _ _ z  = if z == target then Just (AxisZ, dir) else Nothing
singleCubieAnimation Nothing _ _ _                      = Nothing 

buildAnimationStepCube :: (Eq s) => CubiePermutationFunc s -> CubieMap () s -> CubieMap (Maybe (AnimAxis, AnimDir)) s
buildAnimationStepCube perm cube@(CubieMap size func) = CubieMap size newFunc
    where newFunc = (\x -> (\y -> (\z -> (func x y z) >>= (\(_, stickers) -> Just $ ((singleCubieAnimation $ getAnimationForPermutation perm cube) x y z, stickers)))))

-- Now we need to take a sequence of permutatino functions and translate them into a sequence of animations on the cubies
buildAnimationSequenceCubeFunc :: (Ord s) => [CubiePermutationFunc s] -> CubieMap () s -> RawAnimationCubieMap s
buildAnimationSequenceCubeFunc perms cube@(CubieMap size origFunc) = CubieMap size outputFunc
    where outputFunc = (\x -> (\y -> (\z -> (origFunc x y z) >>= (\(_, stickers) -> Just (animFunc x y z, stickers)))))
          animFunc = animationSequenceCubieFunc perms cube 

animationSequenceCubieFunc :: (Ord s) => [CubiePermutationFunc s] -> CubieMap () s -> CubieX -> CubieY -> CubieZ -> [Maybe (AnimAxis, AnimDir)]
animationSequenceCubieFunc [] _ _ _ _           = []
animationSequenceCubieFunc (p:rest) cube x y z  = ((animFunc x y z) >>= (fst)) : (animationSequenceCubieFunc rest (p cube) x' y' z') 
    where (CubieMap _ animFunc)             = buildAnimationStepCube p cube
          (x', y', z')                      = nextCubieLocation cube p (x,y,z)

buildSequenceOfCubes :: (Eq s) => [CubiePermutationFunc s] -> CubieMap () s -> [CubieMap () s]
buildSequenceOfCubes [] cube = [cube]
buildSequenceOfCubes (p:rest) cube = (cube:(buildSequenceOfCubes rest (p cube)))

-- Writes the AFrame output to a file and displays in a web browser pop-up
showCubeAFrame :: RawAnimationCubieMap StickerId -> IO ()
showCubeAFrame cube@(CubieMap size _) = do
    let solved = (cubiesToCube cube) == (solvedCubeOfSize size)

    let fn = "output.html"
    outputScene fn $ cubeScene [(cube, (0,0,-5)), (cube, (0,-7,-5))] solved
    createProcess $ proc "open" [fn]
    return ()

mainLoop :: IndexedCube -> IO ()
mainLoop cube@(Cube size _) = do
    if equalBy (idToColor size) (solvedCubeOfSize size) cube then do
        putStrLn "SOLVED!"
        return ()
    else do
        Just comm <- readline ">>> "
        if comm == "Q" || comm == "q" then return () else do
            if comm == "show" then do
                ph <- showCubeAFrame $ buildCubieMap cube (const . const . const [])
                mainLoop cube
            else do
                let cube' = parseAndApply cube comm
                let animCube = buildRawAnimationCubieMap cube comm
                showCubeAFrame animCube
                mainLoop cube'

main :: IO ()
main = do
    start <- randomCube 4
    mainLoop start


