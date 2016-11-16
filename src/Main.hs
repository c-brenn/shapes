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

  get "/test" $ svg $ render (read testDrawing :: Drawing)

testDrawing :: String
testDrawing = "[(Scale 10 10 :+: Rotate 20, Square, [StrokeWidth 1, Fill (RGB 255 182 193), Stroke Blue])]"
