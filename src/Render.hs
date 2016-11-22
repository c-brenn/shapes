module Render (render, Drawing(..)) where

import Control.Monad (forM_)
import Data.Matrix   (toLists)
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes

import Shape     (Shape(..))
import Style     (Style(..))
import Transform (Transform(..))
import qualified Transform as T (transform)

type Object  = ([Transform], Shape, [Style])

type Drawing = [Object]

render :: Drawing -> Svg
render drawing = svgBoilerplate $ forM_ drawing toSvg

toSvg :: Object -> Svg
toSvg (t, sh, st)
  = styles unstyledSvg st
  where unstyledSvg = shape sh ! attributes t

shape :: Shape -> Svg
shape Empty  = rect
shape Square = rect   ! width "1" ! height "1"
shape Circle = circle ! r "1"

attributes :: [Transform ]-> Attribute
attributes transforms = transform attributesMatrix
  where transformMatrix = T.transform $ foldl (:+:) Identity transforms
        [[t11,t12,t13], [t21,t22,t23], _] = toLists transformMatrix
        attributesMatrix = matrix t11 t21 t12 t22 t13 t23

styles :: Svg -> [Style] -> Svg
styles svg s = foldr addStyle svg s ! customAttribute "vector-effect" "non-scaling-stroke"

addStyle :: Style -> Svg -> Svg
addStyle (Fill f)   svg = svg ! fill   (attributeFromString f)
addStyle (Stroke k) svg = svg ! stroke (attributeFromString k)
addStyle (StrokeWidth sw) svg = svg ! strokeWidth (attributeFromString sw)

attributeFromString :: Show a => a -> AttributeValue
attributeFromString = stringValue . show

svgBoilerplate :: Svg -> Svg
svgBoilerplate = docTypeSvg ! version "1.1" ! viewbox "-25 -25 50 50"
