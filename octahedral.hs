-- Haskell implementation of the Octahedral group
-- This group characterizes symmetries of the cube (or octahedron)
-- The subgroup of it that consists of orientation-preserving symmetries is useful in 3d graphics
module Octahedral where
import Data.Matrix
import qualified Data.Vector as Vec
import Data.Monoid
import Data.Group

data OctGroup = OctSym (Matrix Int)
    deriving (Eq)

instance Show OctGroup where
    show (OctSym m) = show m

instance Monoid OctGroup where
    mempty = OctSym $ identity 3 
    mappend (OctSym a) (OctSym b) = OctSym $ a * b

instance Group OctGroup where
    invert (OctSym m) = OctSym $ transpose m

-- Generators for the Octahedral group
octRotateX :: OctGroup
octRotateX = OctSym $ fromLists [[1, 0, 0], [0, 0, 1], [0, -1, 0]]

octRotateY :: OctGroup
octRotateY = OctSym $ fromLists [[0, 0, -1], [0, 1, 0], [1, 0, 0]]

octRotateZ :: OctGroup
octRotateZ = OctSym $ fromLists [[0, -1, 0], [1, 0, 0], [0, 0, 1]]

flipX :: OctGroup
flipX = OctSym $ fromLists [[-1, 0, 0], [0, 1, 0], [0, 0, 1]]

flipY :: OctGroup
flipY = OctSym $ fromLists [[1, 0, 0], [0, -1, 0], [0, 0, 1]]

flipZ :: OctGroup
flipZ = OctSym $ fromLists [[1, 0, 0], [0, 1, 0], [0, 0, -1]]

-- Data types to look up the axis alignment of an element of the octahedral group
data AnimAxis = AxisX | AxisY | AxisZ
    deriving (Eq, Show)
data AnimDir = Forwards | Backwards
    deriving (Eq, Show)

flipAnimDir :: AnimDir -> AnimDir
flipAnimDir Forwards    = Backwards
flipAnimDir Backwards   = Forwards

vecForAxis :: AnimAxis -> Vec.Vector Int
vecForAxis AxisX = Vec.fromList [1,0,0]
vecForAxis AxisY = Vec.fromList [0,1,0]
vecForAxis AxisZ = Vec.fromList [0,0,1]

vecToAxis :: [Int] -> (AnimAxis, AnimDir)
vecToAxis [1, 0, 0]     = (AxisX, Forwards)
vecToAxis [-1, 0, 0]    = (AxisX, Backwards)
vecToAxis [0, 1, 0]     = (AxisY, Forwards)
vecToAxis [0, -1, 0]    = (AxisY, Backwards)
vecToAxis [0, 0, 1]     = (AxisZ, Forwards)
vecToAxis [0, 0, -1]    = (AxisZ, Backwards)

lookupAxis :: OctGroup -> AnimAxis -> (AnimAxis, AnimDir)
lookupAxis (OctSym m) = vecToAxis . Vec.toList . (getCol 1) . (multStd m) . colVector . vecForAxis  

