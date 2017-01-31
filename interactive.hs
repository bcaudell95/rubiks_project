module RubiksInteractive where
import RubiksAFrame hiding (main) 
import Rubiks
import Cubie

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
parseMove :: CubeSize -> String -> Maybe PermutationFunc
parseMove size text = do
    category <- moveCategory $ head text 
    let afterCategory = tail text

    let shouldReverse = (not $ null afterCategory) && (head afterCategory) == '\''
    let afterReverse = if shouldReverse then tail afterCategory else afterCategory

    let idx = if null afterReverse then 0 else (read afterReverse) :: Int

    let move = category size idx
    return $ if shouldReverse then reverseMove move else move

moveCategory :: Char -> Maybe (CubeSize -> Int -> PermutationFunc)
moveCategory 'R' = Just rightMove
moveCategory 'L' = Just leftMove
moveCategory 'U' = Just upMove
moveCategory 'D' = Just downMove
moveCategory 'F' = Just frontMove
moveCategory 'B' = Just backMove
moveCategory 'X' = Just $ const . xRotation 
moveCategory 'Y' = Just $ const . yRotation
moveCategory 'Z' = Just $ const . zRotation
moveCategory _ = Nothing

-- Uses the above function to apply a user-written sequence of moves to a cube
parseAndApply :: IndexedCube -> String -> IndexedCube
parseAndApply cube@(Cube size _) moves = applyPerms cube $ map (fromJust . (parseMove size)) $ words moves

-- Takes a starting cube and a sequence of commands and translates it into a RawAnimationCubieMap
buildRawAnimationCubieMap :: IndexedCube -> String -> RawAnimationCubieMap StickerId 
buildRawAnimationCubieMap c@(Cube size func) command = buildCubieMap c animFunc
    where animFunc = getRawAnimationSequence size $ words command

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
    start <- randomCube 3
    mainLoop start


