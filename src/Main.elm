module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (color, cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Circle


type alias Circle =
    { x : Int
    , y : Int
    }



-- MODEL


type alias Model =
    Circle


init : Model
init =
    { x = 10, y = 10 }



-- UPDATE


type Msg
    = Step


update : Msg -> Model -> Model
update msg model =
    case msg of
        Step ->
            { model | x = model.x + 10, y = model.y + 10 }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Step ] [ text "+" ]

        -- , div [] [ text (String.fromInt model) ]
        , svg [ width "300", height "300", viewBox "0 0 300 300" ] (pixels model)
        ]


pixels model =
    [ circle [ cx (String.fromInt model.x), cy (String.fromInt model.y), r "5", fill "rgb(255,0,0)" ] [] ]
