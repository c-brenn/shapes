module Main where

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import Web.Scotty

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Text.Read (readMaybe)

import Render

svg drawing = do
  setHeader "Content-Type" "image/svg+xml"
  raw $ renderSvg drawing

main :: IO ()
main = scotty 3000 $ do
  get "/" $ file "web/index.html"
  get "/static/shapes.js" $ file "web/elm/shapes.js"

  get "/drawing" $ do
    input <- param "input"
    let drawing = (read $ T.unpack $ TL.toStrict input) :: Drawing
    svg $ render drawing

  get "/validate" $ do
    input <- param "input"
    let respStatus =
          case (readMaybe $ T.unpack $ TL.toStrict input) :: Maybe Drawing of
            Nothing -> 422
            _ -> 200
    status $ toEnum respStatus
    addHeader "Access-Control-Allow-Origin" "*"
