module ImageConfig exposing (ImageConfig, Msg, init, update, view)

import Basics exposing (Float, Int)
import Html exposing (Html, button, div, text)
import SingleSlider exposing (SingleSlider)



-- MODEL


type alias ImageConfig =
    { height : Int, width : Int, positionDelta : Int, maxCircles : Int, positionDeltaSlider : SingleSlider Msg, maxCirclesSlider : SingleSlider Msg }



-- INIT
-- TODO: don't need both slider and values in config, slider contains value


init : () -> ImageConfig
init () =
    { height = 500
    , width = 500
    , positionDelta = 5
    , maxCircles = 1000
    , positionDeltaSlider = SingleSlider.init { min = 0, max = 100, value = 5, onChange = UpdatePositionDelta, step = 1 }
    , maxCirclesSlider = SingleSlider.init { min = 1, max = 10000, value = 500, onChange = UpdateMaxCircles, step = 100 }
    }



-- Msg


type Msg
    = UpdatePositionDelta Float
    | UpdateMaxCircles Float


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
                , positionDeltaSlider = SingleSlider.update new_value imageConfig.positionDeltaSlider
            }

        UpdateMaxCircles new_value ->
            let
                value =
                    round new_value
            in
            { imageConfig
                | maxCircles = value
                , maxCirclesSlider = SingleSlider.update new_value imageConfig.maxCirclesSlider
            }



-- VIEW


view : ImageConfig -> Html Msg
view imageConfig =
    div [] [ SingleSlider.view imageConfig.positionDeltaSlider, SingleSlider.view imageConfig.maxCirclesSlider ]
