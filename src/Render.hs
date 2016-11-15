module Render (render) where

import Shapes    (Shape(..))
import Transform (Transform(..))
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes
import Control.Monad (forM_)

type Object  = (Transform, Shape)
type Drawing = [Object]

render :: Drawing -> Svg
render drawing = svgBoilerplate $ forM_ drawing toSvg

toSvg :: Object -> Svg
toSvg _ = rect ! width "11" ! height "1"

svgBoilerplate :: Svg -> Svg
svgBoilerplate = docTypeSvg ! version "1.1" ! viewbox "-25 -25 50 50"
