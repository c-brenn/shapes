module Main where

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Web.Scotty

import Render
import Shape
import Style
import Transform

svg drawing = do
  setHeader "Content-Type" "image/svg+xml"
  raw $ renderSvg drawing

main :: IO ()
main = scotty 3000 $ do
  get "/" $ text "hello world!"

  get "/test" $ svg $ render [
    (scale 10 10 <+> rotate 60, square, [strokeWidth 1, stroke blue, fill (rgb 255 182 193)])
    ]
