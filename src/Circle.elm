module Circle exposing (Circle, CircleUpdate, ColorUpdate, ComparablePosition, InternalColor, fillColor, newCircle, randomCircleUpdate, updateCircle)

import Color exposing (Color)
import Direction exposing (Direction)
import ImageConfig exposing (ImageConfig)
import Random


type alias ColorUpdate =
    { rDelta : Int, gDelta : Int, bDelta : Int }


type alias CircleUpdate =
    { direction : Direction, colorUpdate : ColorUpdate }


randomColorUpdate : Random.Generator ColorUpdate
randomColorUpdate =
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


randomCircleUpdate : ImageConfig -> Random.Generator CircleUpdate
randomCircleUpdate imageConfig =
    Random.map2
        CircleUpdate
        (Direction.generator imageConfig.positionDelta)
        randomColorUpdate


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


internalColorToColor : InternalColor -> Color
internalColorToColor color =
    Color.rgb255 color.red color.green color.blue


internalColorToCssColor : InternalColor -> String
internalColorToCssColor color =
    internalColorToColor color
        |> Color.toCssString


type alias Circle =
    { position : Position
    , color : InternalColor
    , radius : Int
    }


newCircle : Circle
newCircle =
    { position = { x = 10, y = 10 }, color = { red = 100, green = 0, blue = 100 }, radius = 5 }



-- UPDATE


updatePosition : ImageConfig -> Direction -> Position -> Position
updatePosition imageConfig direction position =
    { x = clamp 0 imageConfig.width (position.x + direction.xDelta)
    , y = clamp 0 imageConfig.height (position.y + direction.yDelta)
    }


updateColor : InternalColor -> ColorUpdate -> InternalColor
updateColor color colorUpdate =
    { red = clamp 0 255 (color.red + colorUpdate.rDelta)
    , green = clamp 0 255 (color.green + colorUpdate.gDelta)
    , blue = clamp 0 255 (color.blue + colorUpdate.bDelta)
    }


updateCircle : ImageConfig -> CircleUpdate -> Circle -> Circle
updateCircle imageConfig circleUpdate circle =
    { position = updatePosition imageConfig circleUpdate.direction circle.position
    , color = updateColor circle.color circleUpdate.colorUpdate
    , radius = circle.radius
    }


fillColor : Circle -> String
fillColor circle =
    internalColorToCssColor circle.color
