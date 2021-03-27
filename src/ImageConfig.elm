module ImageConfig exposing (ImageConfig, Msg, init, update, view)

import Basics exposing (Float, Int)
import Html exposing (Html, button, div, text)
import SingleSlider exposing (SingleSlider)



-- MODEL


type alias ImageConfig =
    { height : Int, width : Int, positionDelta : Int, maxCircles : Int, positionDeltaSlider : SingleSlider Msg }



-- INIT


init : () -> ImageConfig
init () =
    { height = 500
    , width = 500
    , positionDelta = 5
    , maxCircles = 1000
    , positionDeltaSlider = SingleSlider.init { min = 0, max = 100, value = 5, onChange = UpdatePositionDelta, step = 1 }
    }



-- Msg


type Msg
    = UpdatePositionDelta Float


update : Msg -> ImageConfig -> ImageConfig
update msg imageConfig =
    case msg of
        UpdatePositionDelta delta ->
            { imageConfig | positionDelta = round delta }



-- VIEW


view : ImageConfig -> Html Msg
view imageConfig =
    div [] [ SingleSlider.view imageConfig.positionDeltaSlider ]
