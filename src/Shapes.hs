module Shapes(Shape(..), empty, circle, square) where

data Shape = Empty
           | Circle
           | Square

empty, circle, square :: Shape
empty  = Empty
circle = Circle
square = Square
