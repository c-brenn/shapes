module Transform ( Transform(..) , transform) where

import qualified Data.Matrix as Matrix
  ( Matrix(..)
  , fromLists
  , multStd2
  , identity
  )

-- DSL representation of transformations
data Transform
  = Identity
  | Translate Double Double
  | Scale Double Double
  | Rotate Double
  | Skew Double Double
  | Transform :+: Transform
  deriving (Read, Show)

-- Points are represented by the vector [x, y, 1]
-- transformations can then be represented as 3x3 matrices
-- composition of these transformations is matrix multiplication
transform :: Transform -> Matrix.Matrix Double
transform Identity = Matrix.identity 3
transform (Translate x y)  = Matrix.fromLists [ [1, 0 ,x],
                                                [0, 1, y],
                                                [0, 0, 1] ]
transform (Scale x y)      = Matrix.fromLists [ [x, 0 ,0],
                                                [0, y, 0],
                                                [0, 0, 1] ]
transform (Rotate degrees) = Matrix.fromLists [ [cos rad, -(sin rad), 0],
                                                [sin rad, cos rad,    0],
                                                [0,       0,          0] ]
  where rad = deg2rad degrees
transform (Skew degX degY) = Matrix.multStd2 skewX skewY
  where
    radX = deg2rad degX
    radY = deg2rad degY
    skewX =  Matrix.fromLists [[1, tan radX, 0], [0,        1, 0], [0, 0, 1]]
    skewY =  Matrix.fromLists [[1, 0,        0], [tan radY, 1, 0], [0, 0, 1]]

transform (t1 :+: t2)  = Matrix.multStd2 (transform t1) (transform t2)

deg2rad :: Floating a => a -> a
deg2rad degrees = (degrees * pi) / 180
