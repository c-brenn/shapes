module Style ( Style(..) , module Colour) where

import Colour

data Style = Fill Colour
           | Stroke Colour
           | StrokeWidth Double
           deriving Read
