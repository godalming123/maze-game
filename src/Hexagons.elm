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
        * 2
        + (case position of
            TopLeft ->
                0

            TopMiddle ->
                0

            TopRight ->
                0

            BottomLeft ->
                1

            BottomMiddle ->
                1

            BottomRight ->
                1
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


type HexagonTop
    = HexagonTop
        { line0 : Utils.Line Float
        , line1 : Utils.Line Float
        , line2 : Utils.Line Float
        , hexagon : List ( Float, Float )
        , center : ( Float, Float )
        }


hexagonTop : Float -> Float -> Float -> Float -> HexagonTop
hexagonTop x y w h =
    let
        p0 =
            ( x, y + 0.5 * h )

        p1 =
            ( x + 0.25 * w, y )

        p2 =
            ( x + 0.75 * w, y )

        p3 =
            ( x + w, y + 0.5 * h )

        p4 =
            ( x + 0.75 * w, y + h )

        p5 =
            ( x + 0.25 * w, y + h )
    in
    HexagonTop
        { line0 = Utils.Line p0 p1
        , line1 = Utils.Line p1 p2
        , line2 = Utils.Line p2 p3
        , hexagon = [ p0, p1, p2, p3, p4, p5, p0 ]
        , center = ( x + 0.5 * w, y + 0.5 * h )
        }


getHexagonTops : Int -> Int -> List HexagonTop
getHexagonTops x y =
    [ hexagonTop
        (Basics.toFloat (x * 150))
        (Basics.toFloat (y * 100))
        100
        100
    , hexagonTop
        (Basics.toFloat (x * 150) + 75)
        (Basics.toFloat (y * 100) + 50)
        100
        100
    ]


tesalatedHexagonLines : Float -> Float -> List HexagonTop
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


type LeftOrRight
    = Left
    | Right


getHexagonFromXY : Float -> Float -> ( LeftOrRight, Int, Int )
getHexagonFromXY x y =
    -- If the x and y input is on the edge of a hexagon, then this function
    -- should round the output x or y down
    let
        ( yMacro, yMicro ) =
            Utils.modBy y 100

        absoluteDeltaBetweenYMicroAnd50 =
            abs (yMicro - 50)

        ( xMacro, xMicro ) =
            Utils.modBy (x - absoluteDeltaBetweenYMicroAnd50 / 2) 150
    in
    if xMicro <= 100 - absoluteDeltaBetweenYMicroAnd50 then
        ( Left, xMacro, yMacro )

    else
        ( Right
        , xMacro
        , if yMicro > 50 then
            yMacro

          else
            yMacro - 1
        )
