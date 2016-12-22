import Rubiks
import Display
import Codec.Picture
import Data.Maybe (fromJust)

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
countPositiveEdges (Cube 3 func) = sum $ map (\((f1,x1,y1),(f2,x2,y2)) -> if (compareColorIndices (func f1 x1 y1) (func f2 x2 y2)) == GT then 1 else 0) edgesWithOrientation 


-- Sequence of moves to flip two edges in place on a 3-cube
flipTwoEdges :: [PermutationFunc]
flipTwoEdges = [slice, up, slice, up, slice, up, slice, up, slice, up', slice, up', slice, up', slice, up'] 
    where up = (upMove 3 0)
          slice = (rightMove 3 1)
          up' = up . up . up

-- General function to get all prefixed of a list, including the whole list
prefixes :: [a] -> [[a]]
prefixes items = [take i items| i <- [1..(length items)]]

-- Generating the graph of corner transitions
data StandardMove = U | U' | R | R' | F | F' | L | L' | B | B' | D | D'
    deriving Show
type FaceDirection = Char
type CornerOrientation = [FaceDirection] -- Actually has length exactly 3, but declaring a tuple makes instantiation more annoying
type GraphVertexAndEdges = (CornerOrientation, [(StandardMove, CornerOrientation)])

movesFrom :: CornerOrientation -> [(StandardMove, CornerOrientation)]
movesFrom "URF" = [(U, "UFL"), (U',"UBR"), (R, "BRU"), (R', "FRD"), (F, "RDF"), (F', "LUF")]
movesFrom "UFL" = [(U, "ULB"), (U',"URF"), (F, "RFU"), (F', "LFD"), (L, "FDL"), (L', "BUL")]
movesFrom "ULB" = [(U, "UBR"), (U',"UFL"), (L, "FLU"), (L', "BLD"), (B, "LDB"), (B', "RUB")]
movesFrom "UBR" = [(U, "URF"), (U',"ULB"), (R, "BDR"), (R', "FUR"), (B, "LBU"), (B', "RBD")]
movesFrom "FRD" = [(R, "URF"), (R',"DRB"), (F, "FDL"), (F', "FUR"), (D, "RBD"), (D', "LFD")]
movesFrom "LFD" = [(F, "UFL"), (F',"DFR"), (L, "LDB"), (L', "LUF"), (D, "FRD"), (D', "BLD")]
movesFrom "BLD" = [(L, "ULB"), (L',"DLF"), (B, "BDR"), (B', "BUL"), (D, "LFD"), (D', "RBD")]
movesFrom "RBD" = [(R, "RDF"), (R',"RUB"), (B, "UBR"), (B', "DBL"), (D, "BLD"), (D', "FRD")]

subgraph :: [GraphVertexAndEdges]
subgraph = [(start, movesFrom start) | start <- ["URF", "UFL", "ULB", "UBR", "FRD", "LFD", "BLD", "RBD"]]

cycleMove :: GraphVertexAndEdges -> Int -> GraphVertexAndEdges
cycleMove x 0 = x
cycleMove (start, moves) 1 = (cycle3 start, map (\(m, r) -> (m, cycle3 r)) moves) 
    where cycle3 = (\s -> take 3 $ tail $ cycle s)
cycleMove (start, moves) 2 = (cycle3' start, map (\(m, r) -> (m, cycle3' r)) moves) 
    where cycle3' = (\s -> take 3 $ tail $ tail $ cycle s)

graph :: [GraphVertexAndEdges]
graph = concat $ [[m, cycleMove m 1, cycleMove m 2] | m <- subgraph] 

type CornerCube = Cube (Maybe Int)

cornerCube :: CornerCube
cornerCube = Cube 3 cornerCubeFunc

cornerCubeFunc :: CubeFunc (Maybe Int)
-- Centers and edges
cornerCubeFunc _ 0 0    = Nothing
cornerCubeFunc _ 1 0    = Nothing
cornerCubeFunc _ (-1) 0   = Nothing
cornerCubeFunc _ 0 1    = Nothing
cornerCubeFunc _ 0 (-1)   = Nothing
-- 0's on the U and D faces
cornerCubeFunc 0 _ _    = Just 0
cornerCubeFunc 5 _ _    = Just 0
-- 1's and 2's arond the U layer
cornerCubeFunc 1 (-1) 1   = Just 1
cornerCubeFunc 2 1 1   = Just 1
cornerCubeFunc 3 1 (-1)   = Just 1
cornerCubeFunc 4 (-1) (-1)   = Just 1
cornerCubeFunc 1 1 1   = Just 2
cornerCubeFunc 2 1 (-1)   = Just 2
cornerCubeFunc 3 (-1) (-1)   = Just 2
cornerCubeFunc 4 (-1) 1   = Just 2
-- 1's and 2's around the D layer
cornerCubeFunc 1 1 (-1)   = Just 1
cornerCubeFunc 2 (-1) (-1)   = Just 1
cornerCubeFunc 3 (-1) 1   = Just 1
cornerCubeFunc 4 1 1   = Just 1
cornerCubeFunc 1 (-1) (-1)   = Just 2
cornerCubeFunc 2 (-1) 1   = Just 2
cornerCubeFunc 3 1 1   = Just 2
cornerCubeFunc 4 1 (-1)   = Just 2

-- Functions used in the drawing of this corner cube
cornerEntryToPixel :: Maybe Int -> PixelRGBA8
cornerEntryToPixel Nothing = transparent
cornerEntryToPixel (Just x) = [red, green, blue] !! x

sumOfOrientations :: CornerCube -> Int
sumOfOrientations (Cube _ func) = sum . (map fromJust) $ func <$> [0,5] <*> [1,-1] <*> [1,-1]
