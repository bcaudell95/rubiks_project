module RubiksInteractive where
import RubiksAFrame
import Rubiks

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
moveCategory 'X' = Just $ (\size _ -> xRotation size)
moveCategory 'Y' = Just $ (\size _ -> yRotation size)
moveCategory 'Z' = Just $ (\size _ -> zRotation size)
moveCategory _ = Nothing

-- Uses the above function to apply a user-written sequence of moves to a cube
parseAndApply :: IndexedCube -> String -> IndexedCube
parseAndApply cube@(Cube size _) moves = applyPerms cube $ map (fromJust . (parseMove size)) $ words moves

-- Writes the AFrame output to a file and displays in a web browser pop-up
showCubeAFrame :: IndexedCube -> IO ()
showCubeAFrame cube = do
    let fn = "output.html"
    outputScene fn [(cube, (0,0,-5)), (cube, (0,-7,-5))]
    createProcess $ proc "open" [fn]
    return ()

mainLoop :: IndexedCube -> IO ()
mainLoop cube@(Cube size _) = do
    Just comm <- readline ">>"
    if comm == "Q" || comm == "q" then return () else do
        if comm == "show" then do
            showCubeAFrame cube
            mainLoop cube
        else do
            mainLoop $ parseAndApply cube comm

main :: IO ()
main = do
    start <- randomCube 3
    mainLoop start


