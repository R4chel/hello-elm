module Direction exposing (Direction(..), generator)

import Random


type Direction
    = North
    | South
    | East
    | West


generator : Random.Generator Direction
generator =
    Random.uniform North [ South, East, West ]
