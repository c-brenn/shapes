module Transform
  ( Transform(..)
  , identity
  , translate
  , scale
  , rotate
  , (<+>)
  , transform
  ) where

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
  | Transform :+: Transform
  deriving (Read, Show)

-- Functions to build an AST in the transform DSL
identity :: Transform
identity = Identity

translate, scale :: Double -> Double -> Transform
translate = Translate
scale = Scale

rotate :: Double -> Transform
rotate = Rotate

(<+>) :: Transform -> Transform -> Transform
(<+>) t1 t2 = t1 :+: t2

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
  where rad = (degrees * pi) / 180
transform (t1 :+: t2)  = Matrix.multStd2 (transform t1) (transform t2)
