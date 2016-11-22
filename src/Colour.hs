module Colour ( Colour(..)) where

import Text.Printf (printf)

data Colour
  = Black
  | Blue
  | Gray
  | Green
  | Navy
  | Purple
  | Red
  | RGB Int Int Int
  | Silver
  | White
  | Yellow
  deriving Read

instance Show Colour where
  show Black       = "black"
  show Blue        = "blue"
  show Gray        = "gray"
  show Green       = "green"
  show Navy        = "navy"
  show Purple      = "purple"
  show Red         = "red"
  show (RGB r g b) = printf "#%2x%2x%2x" r g b
  show Silver      = "silver"
  show Yellow      = "yellow"
  show White       = "white"
