import Rubiks
import Display

-- Count the number of "positively" oriented edges on the 3-cube
-- For positivity, assign a poset relationship on colors as White = Yellow > Red = Orange > Blue = Green
--   and a positive edge cubie is one such that the higher-weight sticker is on the higher-weight color face

-- implement this as an assignment from color to rank in the poset above
colorRank :: Color -> Int
colorRank 0 = 2 -- White
colorRank 5 = 2 -- Yellow
colorRank 1 = 1 -- Red
colorRank 3 = 1 -- Orange
colorRank 2 = 0 -- Green
colorRank 4 = 0 -- Blue

-- Edges to check are tuples ((f1,x1,y1),(f2,x2,y2)) where the (f1,x1,y1) is the higher-weight face sticker
edgesWithOrientation :: [((FaceId, XPos, YPos),(FaceId, XPos, YPos))]
edgesWithOrientation = [((0,1,0),(4,-1,0)), -- WB
                        ((0,0,1),(3,0,-1)), -- WO
                        ((0,-1,0),(2,1,0)), -- WG
                        ((0,0,-1),(1,0,1)), -- WR
                        ((1,1,0),(4,0,-1)), -- RB
                        ((1,-1,0),(2,0,-1)), -- RG
                        ((3,1,0),(4,0,1)), -- OB
                        ((3,-1,0),(2,0,1)), -- OG
                        ((5,1,0),(2,-1,0)), -- YG
                        ((5,0,1),(3,0,1)), -- YO
                        ((5,-1,0),(4,1,0)), -- YB
                        ((5,0,-1),(1,0,-1))]

compareColorIndices :: StickerId -> StickerId -> Ordering
compareColorIndices s1 s2 = compare (stickerToRank s1) (stickerToRank s2) 
    where stickerToRank = colorRank . (idToColor 3)

-- Note that with our ordering, there is no chance of equality in this comparison as, for instance, there is no red-orange edge
countPositiveEdges :: IndexedCube -> Int
countPositiveEdges (IndexedCube 3 func) = sum $ map (\((f1,x1,y1),(f2,x2,y2)) -> if (compareColorIndices (func f1 x1 y1) (func f2 x2 y2)) == GT then 1 else 0) edgesWithOrientation 


-- Sequence of moves to flip two edges in place on a 3-cube
flipTwoEdges :: [PermutationFunc]
flipTwoEdges = [slice, up, slice, up, slice, up, slice, up, slice, up', slice, up', slice, up', slice, up'] 
    where up = (upMove 3 0)
          slice = (rightMove 3 1)
          up' = up . up . up

-- General function to get all prefixed of a list, including the whole list
prefixes :: [a] -> [[a]]
prefixes items = [take i items| i <- [1..(length items)]]
