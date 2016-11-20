module Shapes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as Http

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Model

type alias Model =
    { input : String
    , iframeSrc : String
    , valid : Bool
    }

init : (Model, Cmd Msg)
init =
    let initialModel =
            { input  = ""
            , iframeSrc = shapeServerPrefix ++ "[]"
            , valid  = True
            }
    in (initialModel, Cmd.none)

shapeServerPrefix : String
shapeServerPrefix = "http://localhost:3000"

-- Update

type Msg
    = Validate (Result Http.Error String)
    | NewInput String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewInput input ->
            ({model| input = input}, validateInput input)

        Validate (Ok _) ->
            let newSrc = shapeServerPrefix ++ "/drawing?input=" ++ model.input
            in ({model| valid = True, iframeSrc = newSrc}, Cmd.none)

        Validate (Err _) ->
            ({model| valid = False} , Cmd.none)


-- View

view : Model -> Html Msg
view model =
    div [ class "container"]
        [ h2 [id "header"] [text "SVG"]
        , iframe [id "drawing", src model.iframeSrc] []
        , textarea
                  [ class ("form-control" ++ valid model)
                  , id "drawing-input"
                  , onInput NewInput
                  ]
                  []
        ]

valid : Model -> String
valid model =
    if model.valid then
        " valid"
    else
        " invalid"


-- Http

validateInput : String -> Cmd Msg
validateInput input =
    let
        url = shapeServerPrefix ++ "/validate?input=" ++ input
        request = Http.getString url
    in
        Http.send Validate request

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
