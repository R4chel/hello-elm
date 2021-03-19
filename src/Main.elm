module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (color, cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Direction


type Direction
    = North
    | South
    | East
    | West


random_direction : Random.Generator Direction
random_direction =
    Random.uniform North [ South, East, West ]



-- Circle


type alias Circle =
    { x : Int
    , y : Int
    }


new_circle : Circle
new_circle =
    { x = 10, y = 10 }


position_delta : Int
position_delta =
    50


update_circle : Circle -> Direction -> Circle
update_circle c direction =
    case direction of
        North ->
            { c | y = c.y + position_delta }

        South ->
            { c | y = c.y - position_delta }

        East ->
            { c | x = c.x + position_delta }

        West ->
            { c | x = c.x - position_delta }



-- MODEL


type alias Model =
    { circles : List Circle
    , display_text : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { circles = [], display_text = "" }, Cmd.none )



-- UPDATE


type Msg
    = Choose_direction
    | Step Direction
    | Print_foo


step : Model -> Direction -> Model
step model direction =
    case model.circles of
        [] ->
            { model | circles = [ new_circle ] }

        (hd :: _) as circles ->
            { model | circles = update_circle hd direction :: circles }



-- (Random.generate random_direction)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step direction ->
            ( step model direction
            , Cmd.none
            )

        Choose_direction ->
            ( model, Cmd.batch (List.repeat 20 (Random.generate Step random_direction)) )

        Print_foo ->
            ( { model | display_text = model.display_text ++ " HI! " }, Cmd.none )



-- VIEW


view_circle : Circle -> Svg.Svg msg
view_circle c =
    circle [ cx (String.fromInt c.x), cy (String.fromInt c.y), r "5", fill "rgb(255,0,0)" ] []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Choose_direction ] [ text "+" ]
        , div [] [ text model.display_text ]
        , svg [ width "500", height "500", viewBox "0 0 500 500" ] (pixels model)
        ]


pixels model =
    List.map view_circle model.circles



-- [ circle [ cx (String.fromInt model.x), cy (String.fromInt model.y), r "5", fill "rgb(255,0,0)" ] [] ]
