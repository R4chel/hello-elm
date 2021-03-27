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
import Circle exposing (Circle, CircleUpdate, ColorUpdate, ComparablePosition, InternalColor)
import Color exposing (Color)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import File.Download as Download
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import ImageConfig exposing (ImageConfig)
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (color, cx, cy, fill, height, r, stroke, strokeWidth, viewBox, width)



-- TODO: change snake case to camel case
-- MAIN
-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS


port getSvg : () -> Cmd msg


{-| TODO change to be (() -> msg)
-}
port gotSvg : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotSvg GotSvg
        , if model.paused then
            Sub.none

          else
            onAnimationFrame (\_ -> Choose_direction)
        ]


image_width =
    500


image_height =
    500


position_delta : Int
position_delta =
    10



-- MODEL


type alias Model =
    { active_circle : Circle
    , display_text : String
    , visible_circles : Dict ComparablePosition InternalColor
    , paused : Bool
    , output : String
    , imageConfig : ImageConfig
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initial_circle =
            Circle.new_circle
    in
    ( { imageConfig = { height = 500, width = 500, position_delta = 5 }
      , active_circle = initial_circle
      , display_text = ""
      , visible_circles = Dict.singleton ( initial_circle.position.x, initial_circle.position.y ) initial_circle.color
      , paused = False
      , output = ""
      }
    , Cmd.none
    )



-- UPDATE


{-| TODO rename Choose\_direction to something else now that it also includes color update
-}
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
            Circle.update_circle model.imageConfig circle_update model.active_circle
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
            ( model, Random.generate Step Circle.random_circle_update )

        Print_foo ->
            ( { model | display_text = model.display_text ++ " HI! " }, Cmd.none )

        Toggle_paused ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )

        GetSvg ->
            ( { model | display_text = model.display_text ++ " Download? " }, getSvg () )

        GotSvg output ->
            ( { model | output = output, display_text = model.display_text ++ " TADA!!! " }, Cmd.none )



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

        -- , button [ onClick GetSvg ]
        --     [ text "Download"
        --     ]
        , Html.button [ onClick GetSvg ] [ Html.text "Download" ]
        , div [] [ text model.display_text ]
        , Html.br [] []
        , div [ Html.Attributes.id "output" ] [ model_to_svg model ]
        , Html.textarea
            [ Html.Attributes.rows 10
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.value model.output
            ]
            []
        , if String.isEmpty model.output then
            Html.text ""

          else
            Html.a
                [ Html.Attributes.download "output.svg"
                , Html.Attributes.href ("data:image/svg+xml;base64," ++ Base64.encode model.output)
                ]
                [ Html.text "Download Svg" ]

        -- , if String.isEmpty model.output then
        --     text "Nothing to download"
        --   else
        --     Html.a
        --         [ Html.Attributes.download "output.svg"
        --         , Html.Attributes.href ("data:image/svg+xml;base64," ++ Base64.encode model.output)
        --         ]
        --         [ Html.text "Download Svg" ]
        ]


view_circle : ( ComparablePosition, InternalColor ) -> Svg.Svg msg
view_circle ( position, color ) =
    circle [ cx (String.fromInt (Tuple.first position)), cy (String.fromInt (Tuple.second position)), r "5", fill (Circle.internal_color_to_css_color color) ] []


pixels model =
    Dict.toList model.visible_circles |> List.map view_circle


model_to_svg model =
    svg
        [ width (String.fromInt model.imageConfig.width)
        , height (String.fromInt model.imageConfig.height)
        , viewBox (String.join " " [ "0", "0", String.fromInt model.imageConfig.width, String.fromInt model.imageConfig.height ])
        ]
        (pixels model)
