module RubiksInteractive where
import RubiksAFrame
import Rubiks

import Data.List
import Data.Char
import System.Process

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
moveCategory _ = Nothing

