port module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array exposing (Array)
import Array.Extra as Array
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



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- SUBSCRIPTIONS


port getSvg : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.paused then
            Sub.none

          else
            onAnimationFrame (\_ -> ChooseDirection)
        ]



-- MODEL


type alias Model =
    { imageConfig : ImageConfig
    , activeCircle : Circle
    , displayText : String
    , visibleCircles : Array Circle
    , paused : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialCircle =
            Circle.newCircle
    in
    ( { imageConfig = ImageConfig.init ()
      , activeCircle = initialCircle
      , displayText = ""
      , visibleCircles = Array.fromList [ initialCircle ]
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
    | UpdateImageConfig ImageConfig.Msg


step : Model -> CircleUpdate -> Model
step model circleUpdate =
    let
        updatedCircle =
            Circle.updateCircle model.imageConfig circleUpdate model.activeCircle
    in
    { model
        | activeCircle = updatedCircle
        , visibleCircles =
            Array.push
                updatedCircle
                model.visibleCircles
                |> Array.sliceFrom (-1 * model.imageConfig.maxCircles)
    }


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

        UpdateImageConfig imageConfigUpdate ->
            ( { model
                | imageConfig =
                    ImageConfig.update imageConfigUpdate
                        model.imageConfig
              }
            , Cmd.none
            )



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
        , div [] [ ImageConfig.view model.imageConfig |> Html.map (\x -> UpdateImageConfig x) ]
        , div [] [ text model.displayText ]
        , Html.br [] []
        , div [] [ modelToSvg model ]
        ]


viewCircle : Circle -> Svg.Svg msg
viewCircle c =
    circle [ cx (String.fromInt c.position.x), cy (String.fromInt c.position.y), r (String.fromInt c.radius), fill (Circle.fillColor c) ] []


pixels model =
    Array.toList model.visibleCircles |> List.map viewCircle


modelToSvg model =
    svg
        [ id "output"
        , width (String.fromInt model.imageConfig.width)
        , height (String.fromInt model.imageConfig.height)
        , viewBox (String.join " " [ "0", "0", String.fromInt model.imageConfig.width, String.fromInt model.imageConfig.height ])
        ]
        (pixels model)
