module Circle exposing (Circle, CircleUpdate, ColorUpdate, ComparablePosition, InternalColor, internal_color_to_css_color, new_circle, random_circle_update, update_circle)

import Color exposing (Color)
import Direction exposing (Direction(..))
import ImageConfig exposing (ImageConfig)
import Random


type alias ColorUpdate =
    { r_delta : Int, g_delta : Int, b_delta : Int }


type alias CircleUpdate =
    { direction : Direction, color_update : ColorUpdate }


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
        Direction.generator
        random_color_update


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



-- UPDATE


update_position : ImageConfig -> Direction -> Position -> Position
update_position imageConfig direction position =
    case direction of
        North ->
            { position | y = clamp 0 imageConfig.height (position.y + imageConfig.position_delta) }

        South ->
            { position | y = clamp 0 imageConfig.height (position.y - imageConfig.position_delta) }

        East ->
            { position | x = clamp 0 imageConfig.width (position.x + imageConfig.position_delta) }

        West ->
            { position | x = clamp 0 imageConfig.width (position.x - imageConfig.position_delta) }


update_color : InternalColor -> ColorUpdate -> InternalColor
update_color color color_update =
    { red = clamp 0 255 (color.red + color_update.r_delta)
    , green = clamp 0 255 (color.green + color_update.g_delta)
    , blue = clamp 0 255 (color.blue + color_update.b_delta)
    }


update_circle : ImageConfig -> CircleUpdate -> Circle -> Circle
update_circle imageConfig circle_update circle =
    { position = update_position imageConfig circle_update.direction circle.position
    , color = update_color circle.color circle_update.color_update
    }
