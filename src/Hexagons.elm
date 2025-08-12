module Hexagons exposing (..)

import List
import Utils


type LinePosition
    = TopLeft
    | TopMiddle
    | TopRight
    | BottomLeft
    | BottomMiddle
    | BottomRight


type Direction
    = Forwards -- The right end of the line is the front
    | Backwards -- The left end of the line is the front


type LineFromTesalatedHexagons
    = LineFromTesalatedHexagons
        { x : Int
        , y : Int
        , position : LinePosition
        , direction : Direction
        }


lineFromTesalatedHexagonsToInt : LineFromTesalatedHexagons -> Float -> Int
lineFromTesalatedHexagonsToInt (LineFromTesalatedHexagons { x, y, position }) h =
    (x * numberOfHexagonsVertically h + y)
        * 6
        + (case position of
            TopLeft ->
                0

            TopMiddle ->
                1

            TopRight ->
                2

            BottomLeft ->
                3

            BottomMiddle ->
                4

            BottomRight ->
                5
          )


stringFromLineFromTesalatedHexagons : LineFromTesalatedHexagons -> String
stringFromLineFromTesalatedHexagons (LineFromTesalatedHexagons { x, y, position, direction }) =
    "{x = "
        ++ String.fromInt x
        ++ ", y = "
        ++ String.fromInt y
        ++ ", position = "
        ++ (case position of
                TopLeft ->
                    "TopLeft"

                TopMiddle ->
                    "TopMiddle"

                TopRight ->
                    "TopRight"

                BottomLeft ->
                    "BottomLeft"

                BottomMiddle ->
                    "BottomMiddle"

                BottomRight ->
                    "BottomRight"
           )
        ++ ", direction = "
        ++ (case direction of
                Forwards ->
                    "Forwards"

                Backwards ->
                    "Backwards"
           )
        ++ "}"


numberOfHexagonsHorizontally : Float -> Int
numberOfHexagonsHorizontally width =
    floor (width / 150)


numberOfHexagonsVertically : Float -> Int
numberOfHexagonsVertically height =
    floor (height / 100)


hexagonTop : Float -> Float -> Float -> Float -> List (Utils.Line Float)
hexagonTop x y w h =
    [ ( x, y + 0.5 * h )
    , ( x + 0.25 * w, y )
    , ( x + 0.75 * w, y )
    , ( x + w, y + 0.5 * h )
    ]
        |> Utils.mapInPairs Utils.Line


getHexagonTops : Int -> Int -> List (Utils.Line Float)
getHexagonTops x y =
    hexagonTop
        (Basics.toFloat (x * 150))
        (Basics.toFloat (y * 100))
        100
        100
        ++ hexagonTop
            (Basics.toFloat (x * 150) + 75)
            (Basics.toFloat (y * 100) + 50)
            100
            100


tesalatedHexagonLines : Float -> Float -> List (Utils.Line Float)
tesalatedHexagonLines w h =
    List.range 0 (numberOfHexagonsHorizontally w - 1)
        |> List.concatMap
            (\x ->
                List.range 0 (numberOfHexagonsVertically h - 1)
                    |> List.concatMap
                        (\y -> getHexagonTops x y)
            )


type LineMovementDirection
    = MoveLeft
    | MoveRight


moveLineFromTesalatedHexagons : LineFromTesalatedHexagons -> LineMovementDirection -> Float -> Float -> LineFromTesalatedHexagons
moveLineFromTesalatedHexagons line movementDirection width height =
    let
        (LineFromTesalatedHexagons { x, y, position, direction }) =
            line

        out =
            case ( position, direction, movementDirection ) of
                ( TopLeft, Forwards, MoveRight ) ->
                    { x = x, y = y, position = TopMiddle, direction = Forwards }

                ( TopMiddle, Forwards, MoveRight ) ->
                    { x = x, y = y, position = TopRight, direction = Forwards }

                ( TopRight, Forwards, MoveRight ) ->
                    { x = x, y = y, position = BottomLeft, direction = Backwards }

                ( BottomLeft, Forwards, MoveRight ) ->
                    { x = x, y = y, position = BottomMiddle, direction = Forwards }

                ( BottomMiddle, Forwards, MoveRight ) ->
                    { x = x, y = y, position = BottomRight, direction = Forwards }

                ( BottomRight, Forwards, MoveRight ) ->
                    { x = x + 1, y = y + 1, position = TopLeft, direction = Backwards }

                ( TopLeft, Backwards, MoveRight ) ->
                    { x = x - 1, y = y, position = BottomMiddle, direction = Backwards }

                ( TopMiddle, Backwards, MoveRight ) ->
                    { x = x - 1, y = y - 1, position = BottomRight, direction = Backwards }

                ( TopRight, Backwards, MoveRight ) ->
                    { x = x, y = y - 1, position = BottomLeft, direction = Forwards }

                ( BottomLeft, Backwards, MoveRight ) ->
                    { x = x, y = y + 1, position = TopMiddle, direction = Backwards }

                ( BottomMiddle, Backwards, MoveRight ) ->
                    { x = x, y = y, position = TopRight, direction = Backwards }

                ( BottomRight, Backwards, MoveRight ) ->
                    { x = x + 1, y = y, position = TopLeft, direction = Forwards }

                ( TopLeft, Forwards, MoveLeft ) ->
                    { x = x - 1, y = y - 1, position = BottomRight, direction = Backwards }

                ( TopMiddle, Forwards, MoveLeft ) ->
                    { x = x, y = y - 1, position = BottomLeft, direction = Forwards }

                ( TopRight, Forwards, MoveLeft ) ->
                    { x = x, y = y, position = BottomMiddle, direction = Forwards }

                ( BottomLeft, Forwards, MoveLeft ) ->
                    { x = x, y = y, position = TopRight, direction = Backwards }

                ( BottomMiddle, Forwards, MoveLeft ) ->
                    { x = x + 1, y = y, position = TopLeft, direction = Forwards }

                ( BottomRight, Forwards, MoveLeft ) ->
                    { x = x + 1, y = y + 1, position = TopMiddle, direction = Forwards }

                ( TopLeft, Backwards, MoveLeft ) ->
                    { x = x - 1, y = y, position = BottomRight, direction = Forwards }

                ( TopMiddle, Backwards, MoveLeft ) ->
                    { x = x, y = y, position = TopLeft, direction = Backwards }

                ( TopRight, Backwards, MoveLeft ) ->
                    { x = x, y = y, position = TopMiddle, direction = Backwards }

                ( BottomLeft, Backwards, MoveLeft ) ->
                    { x = x, y = y + 1, position = TopRight, direction = Forwards }

                ( BottomMiddle, Backwards, MoveLeft ) ->
                    { x = x, y = y, position = BottomLeft, direction = Backwards }

                ( BottomRight, Backwards, MoveLeft ) ->
                    { x = x, y = y, position = BottomMiddle, direction = Backwards }
    in
    if out.x < 0 || out.y < 0 || out.x >= numberOfHexagonsHorizontally width || out.y >= numberOfHexagonsVertically height then
        line

    else
        LineFromTesalatedHexagons out
