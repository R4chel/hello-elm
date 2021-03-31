module ImageConfig exposing (ImageConfig, Msg(..), init, update, view)

import Basics exposing (Float, Int)
import Html exposing (Html, br, button, div, text)
import SingleSlider exposing (SingleSlider)
import Svg.Attributes exposing (strokeWidth)



-- MODEL


type alias ImageConfig =
    { height : Int
    , width : Int
    , positionDelta : Int
    , maxCircles : Int
    , radius : Int
    , colorDelta : Int
    , opacity : Float
    , strokeWidth : Int
    , positionDeltaSlider : SingleSlider Msg
    , colorDeltaSlider : SingleSlider Msg
    , maxCirclesSlider : SingleSlider Msg
    , radiusSlider : SingleSlider Msg
    , opacitySlider : SingleSlider Msg
    , strokeWidthSlider : SingleSlider Msg
    }



-- INIT
-- TODO: don't need both slider and values in config, slider contains value


init : () -> ImageConfig
init () =
    { height = 500
    , width = 500
    , positionDelta = 5
    , colorDelta = 5
    , maxCircles = 1000
    , radius = 5
    , opacity = 1
    , strokeWidth = 1
    , positionDeltaSlider = SingleSlider.init { min = 0, max = 100, value = 5, onChange = UpdatePositionDelta, step = 1 }
    , colorDeltaSlider = SingleSlider.init { min = 0, max = 100, value = 5, onChange = UpdateColorDelta, step = 1 }
    , maxCirclesSlider = SingleSlider.init { min = 1, max = 10000, value = 500, onChange = UpdateMaxCircles, step = 100 }
    , radiusSlider = SingleSlider.init { min = 1, max = 100, value = 5, onChange = UpdateRadius, step = 1 }
    , opacitySlider = SingleSlider.init { min = 0, max = 1, value = 1, onChange = UpdateOpacity, step = 0.05 }
    , strokeWidthSlider = SingleSlider.init { min = 0, max = 100, value = 5, onChange = UpdateStrokeWidth, step = 1 }
    }



-- Msg


type Msg
    = UpdatePositionDelta Float
    | UpdateColorDelta Float
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
                , positionDeltaSlider = SingleSlider.update (Basics.toFloat value) imageConfig.positionDeltaSlider
            }

        UpdateColorDelta new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | colorDelta = value
                , colorDeltaSlider = SingleSlider.update (Basics.toFloat value) imageConfig.colorDeltaSlider
            }

        UpdateMaxCircles new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | maxCircles = value
                , maxCirclesSlider = SingleSlider.update (Basics.toFloat value) imageConfig.maxCirclesSlider
            }

        UpdateRadius new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | radius = value
                , radiusSlider = SingleSlider.update (Basics.toFloat value) imageConfig.radiusSlider
            }

        UpdateStrokeWidth new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | strokeWidth = value
                , strokeWidthSlider = SingleSlider.update (Basics.toFloat value) imageConfig.strokeWidthSlider
            }

        UpdateOpacity value ->
            { imageConfig
                | opacity = value
                , opacitySlider = SingleSlider.update value imageConfig.opacitySlider
            }



-- VIEW


view : ImageConfig -> Html Msg
view imageConfig =
    div []
        [ text "Circle Count"
        , SingleSlider.view imageConfig.maxCirclesSlider
        , text "Position Delta"
        , SingleSlider.view imageConfig.positionDeltaSlider
        , text "Color Delta"
        , SingleSlider.view imageConfig.colorDeltaSlider
        , br [] []
        , text "Radius"
        , SingleSlider.view imageConfig.radiusSlider
        , text "Opacity"
        , SingleSlider.view imageConfig.opacitySlider
        , text "Stroke Width"
        , SingleSlider.view imageConfig.strokeWidthSlider
        ]
