module Colour ( Colour(..)) where

import Text.Printf (printf)

data Colour
  = White
  | Black
  | Blue
  | RGB Int Int Int
  deriving Read

instance Show Colour where
  show White = "white"
  show Black = "black"
  show Blue  = "blue"
  show (RGB r g b) = printf "#%2x%2x%2x" r g b
