module Colour
  ( Colour(..)
  , white
  , black
  , blue
  , rgb
  ) where

import Text.Printf (printf)

data Colour
  = White
  | Black
  | Blue
  | RGB Int Int Int

white,black,blue :: Colour
white = White
black = Black
blue  = Blue
rgb :: Int -> Int -> Int -> Colour
rgb   = RGB


instance Show Colour where
  show White = "white"
  show Black = "black"
  show Blue  = "blue"
  show (RGB r g b) = printf "#%2x%2x%2x" r g b
