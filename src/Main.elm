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
import Html.Attributes exposing (href, id)
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
port gotSvg : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotSvg GotSvg
        , if model.paused then
            Sub.none

          else
            onAnimationFrame (\_ -> ChooseDirection)
        ]


imageWidth =
    500


imageHeight =
    500


positionDelta : Int
positionDelta =
    10



-- MODEL


type alias Model =
    { imageConfig : ImageConfig
    , activeCircle : Circle
    , displayText : String
    , visibleCircles : Dict ComparablePosition InternalColor
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialCircle =
            Circle.newCircle
    in
    ( { imageConfig = { height = 500, width = 500, positionDelta = 5 }
      , activeCircle = initialCircle
      , displayText = ""
      , visibleCircles = Dict.singleton ( initialCircle.position.x, initialCircle.position.y ) initialCircle.color
      , paused = False
      }
    , Cmd.none
    )



-- UPDATE


{-| TODO rename Choose\\Direction to something else now that it also includes color update
-}
type Msg
    = ChooseDirection
    | Step CircleUpdate
    | TogglePaused
    | PrintFoo
    | GetSvg
    | GotSvg ()


step : Model -> CircleUpdate -> Model
step model circleUpdate =
    let
        updatedCircle =
            Circle.updateCircle model.imageConfig circleUpdate model.activeCircle
    in
    { model | activeCircle = updatedCircle, visibleCircles = Dict.insert ( updatedCircle.position.x, updatedCircle.position.y ) updatedCircle.color model.visibleCircles }


downloadSvg : String -> Cmd Msg
downloadSvg svgContent =
    Download.string "elm-art.svg" "image/svg+xml" svgContent


withNone model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step circleUpdate ->
            ( step model circleUpdate
            , Cmd.none
            )

        ChooseDirection ->
            ( model, Random.generate Step Circle.randomCircleUpdate )

        PrintFoo ->
            ( { model | displayText = model.displayText ++ " HI! " }, Cmd.none )

        TogglePaused ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )

        GetSvg ->
            ( model, getSvg () )

        GotSvg () ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick TogglePaused ]
            [ text
                (if model.paused then
                    "Play"

                 else
                    "Pause"
                )
            ]
        , Html.button [ onClick GetSvg ] [ Html.text "Download" ]
        , div [] [ text model.displayText ]
        , Html.br [] []
        , div [] [ modelToSvg model ]
        ]


viewCircle : ( ComparablePosition, InternalColor ) -> Svg.Svg msg
viewCircle ( position, color ) =
    circle [ cx (String.fromInt (Tuple.first position)), cy (String.fromInt (Tuple.second position)), r "5", fill (Circle.internalColorToCssColor color) ] []


pixels model =
    Dict.toList model.visibleCircles |> List.map viewCircle


modelToSvg model =
    svg
        [ id "output"
        , width (String.fromInt model.imageConfig.width)
        , height (String.fromInt model.imageConfig.height)
        , viewBox (String.join " " [ "0", "0", String.fromInt model.imageConfig.width, String.fromInt model.imageConfig.height ])
        ]
        (pixels model)
