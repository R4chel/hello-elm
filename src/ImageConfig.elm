module ImageConfig exposing (ImageConfig, Msg, init, update, view)

import Basics exposing (Float, Int)
import Html exposing (Html, button, div, text)
import SingleSlider exposing (SingleSlider)



-- MODEL


type alias ImageConfig =
    { height : Int
    , width : Int
    , positionDelta : Int
    , maxCircles : Int
    , radius : Int
    , opacity : Float
    , positionDeltaSlider : SingleSlider Msg
    , maxCirclesSlider : SingleSlider Msg
    , radiusSlider : SingleSlider Msg
    , opacitySlider : SingleSlider Msg
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
    , positionDeltaSlider = SingleSlider.init { min = 0, max = 100, value = 5, onChange = UpdatePositionDelta, step = 1 }
    , maxCirclesSlider = SingleSlider.init { min = 1, max = 10000, value = 500, onChange = UpdateMaxCircles, step = 100 }
    , radiusSlider = SingleSlider.init { min = 1, max = 100, value = 5, onChange = UpdateRadius, step = 1 }
    , opacitySlider = SingleSlider.init { min = 0, max = 1, value = 1, onChange = UpdateOpacity, step = 0.05 }
    }



-- Msg


type Msg
    = UpdatePositionDelta Float
    | UpdateMaxCircles Float
    | UpdateRadius Float
    | UpdateOpacity Float


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

        UpdateOpacity value ->
            { imageConfig
                | opacity = value
                , opacitySlider = SingleSlider.update value imageConfig.opacitySlider
            }



-- VIEW


view : ImageConfig -> Html Msg
view imageConfig =
    div []
        [ SingleSlider.view imageConfig.positionDeltaSlider
        , SingleSlider.view imageConfig.maxCirclesSlider
        , SingleSlider.view imageConfig.radiusSlider
        , SingleSlider.view imageConfig.opacitySlider
        ]
