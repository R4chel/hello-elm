module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Basics exposing (Int,Float, modBy)
import Browser
import Color exposing (Color)
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


type alias ColorUpdate =
    { r_delta : Int, g_delta : Int, b_delta : Int }


type alias CircleUpdate =
    { direction : Direction, color_update : ColorUpdate }


random_direction : Random.Generator Direction
random_direction =
    Random.uniform North [ South, East, West ]

random_color_update : Random.Generator ColorUpdate
random_color_update =
    Random.map3
       ColorUpdate 
        (Random.uniform
            -5
            [ 5 ]
        )
        (Random.uniform
            -5
            [ 5 ]
        )
        (Random.uniform
            -5
            [ 5 ]
        )


random_circle_update : Random.Generator CircleUpdate
random_circle_update =
    Random.map2
        CircleUpdate
        random_direction
        random_color_update



-- Circle



type alias Position =
    { x : Int
    , y : Int
    }

type alias InternalColor =
    { red : Int,
    green : Int,
    blue : Int

    }

internal_color_to_color : InternalColor -> Color
internal_color_to_color color =
                        Color.rgb255 color.red color.green color.blue

internal_color_to_css_color : InternalColor -> String
internal_color_to_css_color color =
                            internal_color_to_color color
                            |> Color.toCssString
type alias Circle =
    { 
    position : Position
    , color : InternalColor
    }


new_circle : Circle
new_circle =
    { position = {x = 10, y = 10}, color = { red = 100, green = 0, blue = 100}}


image_width =
    500


image_height =
    500


position_delta : Int
position_delta =
    10


update_position : Position -> Direction -> Position
update_position c direction =
    case direction of
        North ->
            { c | y = modBy image_height (c.y + position_delta) }

        South ->
            { c | y = modBy image_height (c.y - position_delta) }

        East ->
            { c | x = modBy image_width (c.x + position_delta) }

        West ->
            { c | x = modBy image_width (c.x - position_delta) }




update_color : InternalColor -> ColorUpdate -> InternalColor
update_color color color_update =
    { red = clamp 0 255 ( color.red + color_update.r_delta ),
      green = clamp 0  255 ( color.green + color_update.g_delta ),
      blue = clamp 0 255 ( color.blue + color_update.b_delta )
    }


update_circle : Circle -> CircleUpdate -> Circle
update_circle c circle_update =
              { position = update_position c.position circle_update.direction,
                color = update_color c.color circle_update.color_update
              
              }



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
    | Step CircleUpdate
    | Print_foo


step : Model -> CircleUpdate -> Model
step model circle_update =
    case model.circles of
        [] ->
            { model | circles = [ new_circle ] }

        (hd :: _) as circles ->
            { model | circles = update_circle hd circle_update :: circles }



-- (Random.generate random_direction)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step circle_update ->
            ( step model circle_update
            , Cmd.none
            )

        -- Choose_direction ->
        --     ( model, Cmd.batch (List.repeat 20 (Random.generate Step random_circle_update)) )

        Choose_direction ->
            ( model, (Random.generate Step random_circle_update) )
        Print_foo ->
            ( { model | display_text = model.display_text ++ " HI! " }, Cmd.none )



-- VIEW


view_circle : Circle -> Svg.Svg msg
view_circle c =
    circle [ cx (String.fromInt c.position.x), cy (String.fromInt c.position.y), r "5", fill (internal_color_to_css_color c.color) ] []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Choose_direction ] [ text "+" ]
        , div [] [ text model.display_text ]
        , svg [ width (String.fromInt image_width), height (String.fromInt image_height), viewBox (String.join " " [ "0", "0", String.fromInt image_width, String.fromInt image_height ]) ] (pixels model)
        ]


pixels model =
    List.map view_circle (List.reverse model.circles)



-- [ circle [ cx (String.fromInt model.x), cy (String.fromInt model.y), r "5", fill "rgb(255,0,0)" ] [] ]
