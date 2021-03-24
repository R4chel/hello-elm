port module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Base64 exposing (decode, encode)
import Basics exposing (Float, Int, modBy)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Color exposing (Color)
import Dict exposing (Dict)
import File.Download as Download
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (color, cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS


port getSvg : String -> Cmd msg


port gotSvg : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        gotSvg GotSvg

    else
        onAnimationFrame (\_ -> Choose_direction)



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


type alias ComparablePosition =
    ( Int, Int )


type alias InternalColor =
    { red : Int
    , green : Int
    , blue : Int
    }


internal_color_to_color : InternalColor -> Color
internal_color_to_color color =
    Color.rgb255 color.red color.green color.blue


internal_color_to_css_color : InternalColor -> String
internal_color_to_css_color color =
    internal_color_to_color color
        |> Color.toCssString


type alias Circle =
    { position : Position
    , color : InternalColor
    }


new_circle : Circle
new_circle =
    { position = { x = 10, y = 10 }, color = { red = 100, green = 0, blue = 100 } }


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
            { c | y = clamp 0 image_height (c.y + position_delta) }

        South ->
            { c | y = clamp 0 image_height (c.y - position_delta) }

        East ->
            { c | x = clamp 0 image_width (c.x + position_delta) }

        West ->
            { c | x = clamp 0 image_width (c.x - position_delta) }


update_color : InternalColor -> ColorUpdate -> InternalColor
update_color color color_update =
    { red = clamp 0 255 (color.red + color_update.r_delta)
    , green = clamp 0 255 (color.green + color_update.g_delta)
    , blue = clamp 0 255 (color.blue + color_update.b_delta)
    }


update_circle : Circle -> CircleUpdate -> Circle
update_circle c circle_update =
    { position = update_position c.position circle_update.direction
    , color = update_color c.color circle_update.color_update
    }



-- MODEL


type alias Model =
    { active_circle : Circle
    , display_text : String
    , visible_circles : Dict ComparablePosition InternalColor
    , paused : Bool
    , output : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initial_circle =
            new_circle
    in
    ( { active_circle = initial_circle
      , display_text = ""
      , visible_circles = Dict.singleton ( initial_circle.position.x, initial_circle.position.y ) initial_circle.color
      , paused = False
      , output = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Choose_direction
    | Step CircleUpdate
    | Toggle_paused
    | Print_foo
    | GetSvg
    | GotSvg String


step : Model -> CircleUpdate -> Model
step model circle_update =
    let
        updated_circle =
            update_circle model.active_circle circle_update
    in
    { model | active_circle = updated_circle, visible_circles = Dict.insert ( updated_circle.position.x, updated_circle.position.y ) updated_circle.color model.visible_circles }


download_svg : String -> Cmd Msg
download_svg svgContent =
    Download.string "elm-art.svg" "image/svg+xml" svgContent


withNone model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step circle_update ->
            ( step model circle_update
            , Cmd.none
            )

        Choose_direction ->
            ( model, Random.generate Step random_circle_update )

        Print_foo ->
            ( { model | display_text = model.display_text ++ " HI! " }, Cmd.none )

        Toggle_paused ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )

        GetSvg ->
            ( model, getSvg "output" )

        GotSvg output ->
            ( { model | output = output }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Toggle_paused ]
            [ text
                (if model.paused then
                    "Play"

                 else
                    "Pause"
                )
            ]
        , button [ onClick GetSvg ]
            [ text "Download"
            ]
        , div [] [ text model.display_text ]
        , div [] [ model_to_svg model ]
        , if String.isEmpty model.output then
            text "Nothing to download"

          else
            Html.a
                [ Html.Attributes.download "output.svg"
                , Html.Attributes.href ("data:image/svg+xml;base64," ++ Base64.encode model.output)
                ]
                [ Html.text "Download Svg" ]
        ]


view_circle : ( ComparablePosition, InternalColor ) -> Svg.Svg msg
view_circle ( position, color ) =
    circle [ cx (String.fromInt (Tuple.first position)), cy (String.fromInt (Tuple.second position)), r "5", fill (internal_color_to_css_color color) ] []


pixels model =
    Dict.toList model.visible_circles |> List.map view_circle


model_to_svg model =
    svg [ width (String.fromInt image_width), height (String.fromInt image_height), viewBox (String.join " " [ "0", "0", String.fromInt image_width, String.fromInt image_height ]) ] (pixels model)
