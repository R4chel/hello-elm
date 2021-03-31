module ImageConfig exposing (ImageConfig, Msg, elementView, init, update, view)

import Basics exposing (Float, Int)
import Element exposing (Element, text)
import Element.Input as Input
import Framework exposing (layout)
import Framework.Slider as Slider
import Html exposing (Html, button, div)
import SingleSlider exposing (SingleSlider)
import Svg.Attributes exposing (strokeWidth)



-- MODEL


type alias ImageConfig =
    { height : Int
    , width : Int
    , positionDelta : Int
    , maxCircles : Int
    , radius : Int
    , opacity : Float
    , strokeWidth : Int
    }



-- INIT
-- TODO: don't need both slider and values in config, slider contains value


init : () -> ImageConfig
init () =
    { height = 500
    , width = 500
    , positionDelta = 5
    , maxCircles = 1000
    , radius = 5
    , opacity = 1
    , strokeWidth = 1
    }



-- Msg


type Msg
    = UpdatePositionDelta Float
    | UpdateMaxCircles Float
    | UpdateRadius Float
    | UpdateOpacity Float
    | UpdateStrokeWidth Float


update : Msg -> ImageConfig -> ImageConfig
update msg imageConfig =
    case msg of
        UpdatePositionDelta new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | positionDelta = value
            }

        UpdateMaxCircles new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | maxCircles = value
            }

        UpdateRadius new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | radius = value
            }

        UpdateStrokeWidth new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | strokeWidth = value
            }

        UpdateOpacity value ->
            { imageConfig
                | opacity = value
            }



-- VIEW


positionDeltaSlider : ImageConfig -> Element Msg
positionDeltaSlider imageConfig =
    Input.slider
        Slider.simple
        { onChange = UpdatePositionDelta
        , label =
            Input.labelAbove []
                (text "Position Delta")
        , min = 0
        , max = 75
        , step = Just 1
        , value = toFloat imageConfig.positionDelta
        , thumb =
            Input.defaultThumb
        }


view : ImageConfig -> Html Msg
view imageConfig =
    Framework.layout [] <|
        Element.el Framework.container <|
            positionDeltaSlider imageConfig


elementView : ImageConfig -> Element Msg
elementView imageConfig =
    Element.column [ Element.height Element.fill ]
        [ Input.slider Slider.simple
            { onChange = UpdatePositionDelta
            , label = Input.labelAbove [] (text "Position Delta")
            , min = 0
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.positionDelta
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateMaxCircles
            , label = Input.labelAbove [] (text "Max Circles")
            , min = 1
            , max = 10000
            , step = Just 100
            , value = toFloat imageConfig.maxCircles
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateRadius
            , label = Input.labelAbove [] (text "Radius")
            , min = 1
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.radius
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateOpacity
            , label = Input.labelAbove [] (text "Opacity")
            , min = 0
            , max = 1
            , step = Just 0.05
            , value = imageConfig.opacity
            , thumb = Input.defaultThumb
            }
        , Input.slider Slider.simple
            { onChange = UpdateStrokeWidth
            , label = Input.labelAbove [] (text "Stroke Width")
            , min = 0
            , max = 100
            , step = Just 1
            , value = toFloat imageConfig.strokeWidth
            , thumb = Input.defaultThumb
            }
        ]



-- layout [] (positionDeltaSlider imageConfig)
-- [ SingleSlider.view imageConfig.positionDeltaSlider
-- , SingleSlider.view imageConfig.maxCirclesSlider
-- , SingleSlider.view imageConfig.radiusSlider
-- , SingleSlider.view imageConfig.opacitySlider
-- , SingleSlider.view imageConfig.strokeWidthSlider
-- ]
