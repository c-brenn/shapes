module Style
  ( Style(..)
  , fill
  , stroke
  , strokeWidth
  , module Colour
  ) where

import Colour

data Style = Fill Colour
           | Stroke Colour
           | StrokeWidth Double

fill,stroke :: Colour -> Style
fill   = Fill
stroke = Stroke

strokeWidth :: Double -> Style
strokeWidth = StrokeWidth
