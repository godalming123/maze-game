module Main exposing (..)

import Array
import Browser
import Browser.Events
import Hexagons
import Html
import Html.Events
import Json.Decode
import List
import Svg
import Svg.Attributes as SvgAttr
import Utils



-- TODO: Resize the hexagon grid to fill the browser window
-- TODO: Make the hexagon grid infinite
-- TODO: Zoom in to the map to hide most of the path when playing
-- TODO: Add keybindings to zoom in and out of the map when editing it
-- TODO: Add the ability to set a finish point as well as a spawn point
-- TODO: Add dark mode


main : Program () Model Model
main =
    Browser.element
        { init =
            \_ ->
                ( { mode = Editing
                  , msg = ""
                  , cursorPos =
                        Hexagons.LineFromTesalatedHexagons
                            { x = 0
                            , y = 0
                            , position = Hexagons.TopLeft
                            , direction = Hexagons.Forwards
                            }
                  , borderInteraction = Nothing
                  , spawnPointPos = -1
                  , linesFromTesalatedHexagons =
                        Array.repeat
                            -- TODO: Do not hard code 30
                            30
                            { line0 = LineState { isBorder = False }, line1 = LineState { isBorder = False }, line2 = LineState { isBorder = False } }
                  , width = 500
                  , height = 500
                  }
                , Cmd.none
                )
        , update = \newState -> \_ -> ( newState, Cmd.none )
        , view = view
        , subscriptions =
            \model ->
                Browser.Events.onKeyDown
                    (Json.Decode.map
                        (\k ->
                            case Utils.get k (getKeybindings model) of
                                Just x ->
                                    x.newState

                                Maybe.Nothing ->
                                    { model | msg = "Unrecognized keybinding " ++ k }
                        )
                        (Json.Decode.field "key" Json.Decode.string)
                    )
        }


moveCursor : Hexagons.LineMovementDirection -> Model -> Model
moveCursor direction model =
    { model
        | cursorPos = Hexagons.moveLineFromTesalatedHexagons model.cursorPos direction (Basics.toFloat model.width) (Basics.toFloat model.height)
        , linesFromTesalatedHexagons =
            case model.borderInteraction of
                Nothing ->
                    model.linesFromTesalatedHexagons

                SetBorder active ->
                    model.linesFromTesalatedHexagons
                        |> setLineStyleAtPosition model.height model.cursorPos (LineState { isBorder = active })
    }


getKeybindings : Model -> Utils.OrderedDict String { label : String, newState : Model }
getKeybindings m =
    let
        model =
            { m | msg = "" }
    in
    case model.mode of
        Editing ->
            Utils.dictFromList
                -- TODO: Use icons to indicate how keybindings move the cursor instead of text
                [ ( "h", { label = "Ⓗ Move left", newState = moveCursor Hexagons.MoveLeft model } )
                , ( "l", { label = "Ⓛ Move right", newState = moveCursor Hexagons.MoveRight model } )
                , ( "j", { label = "Ⓙ Move left then right", newState = List.foldl moveCursor model [ Hexagons.MoveLeft, Hexagons.MoveRight ] } )
                , ( "k", { label = "Ⓚ Move left, right, right, and left", newState = List.foldl moveCursor model [ Hexagons.MoveLeft, Hexagons.MoveRight, Hexagons.MoveRight, Hexagons.MoveLeft ] } )
                , ( "t"
                  , { label = "Ⓣ Toggle direction"
                    , newState =
                        let
                            (Hexagons.LineFromTesalatedHexagons cursorPos) =
                                model.cursorPos

                            newDirection =
                                case cursorPos.direction of
                                    Hexagons.Forwards ->
                                        Hexagons.Backwards

                                    Hexagons.Backwards ->
                                        Hexagons.Forwards
                        in
                        { model | cursorPos = Hexagons.LineFromTesalatedHexagons { cursorPos | direction = newDirection } }
                    }
                  )
                , ( "s"
                  , { label = "Ⓢ Set spawn point"
                    , newState = { model | spawnPointPos = Hexagons.lineFromTesalatedHexagonsToInt model.cursorPos (Basics.toFloat model.height) }
                    }
                  )
                , ( "p"
                  , { label = "Ⓟ Play"
                    , newState =
                        if model.spawnPointPos == -1 then
                            { model | msg = "Cannot play when spawn point is not set" }

                        else
                            let
                                hexagon =
                                    Hexagons.tesalatedHexagonLines (Basics.toFloat model.width) (Basics.toFloat model.height) |> Array.fromList |> Array.get model.spawnPointPos
                            in
                            case hexagon of
                                Maybe.Nothing ->
                                    { model | msg = "Error: model.spawnPointPos (" ++ String.fromInt model.spawnPointPos ++ ") is out of range" }

                                Maybe.Just (Hexagons.HexagonTop x) ->
                                    { model | mode = Playing x.center }
                    }
                  )
                , ( "b"
                  , case model.borderInteraction of
                        -- TODO: Use an icon at the back of the cursor to indicate the `borderCreation` state instead of text
                        Nothing ->
                            { label = "Ⓑ Not changing borders", newState = { model | borderInteraction = SetBorder True } }

                        SetBorder True ->
                            { label = "Ⓑ Creating borders", newState = { model | borderInteraction = SetBorder False } }

                        SetBorder False ->
                            { label = "Ⓑ Removing borders", newState = { model | borderInteraction = Nothing } }
                  )
                ]

        Playing _ ->
            Utils.dictFromList
                -- TODO: Allow multiple direction keybindings to be used at the same time
                [ ( "Escape", { label = "␛ Edit", newState = { model | mode = Editing } } )
                , ( "ArrowLeft", { label = "⮈ Move left", newState = movePlayer model -10 0 } )
                , ( "ArrowRight", { label = "⮊ Move right", newState = movePlayer model 10 0 } )
                , ( "ArrowUp", { label = "⮉ Move up", newState = movePlayer model 0 -10 } )
                , ( "ArrowDown", { label = "⮋ Move down", newState = movePlayer model 0 10 } )
                ]


type HexagonHorizontalDelta
    = Delta
    | SameLeft
    | SameRight


getHexagonCoordDelta :
    Float
    -> Float
    -> Float
    -> Float
    ->
        { hexagonHorizontalDelta : HexagonHorizontalDelta
        , lowerHexagonXCoord : Int
        , lowerHexagonYCoord : Int
        , hexagonAbsoluteXDelta : Int
        , hexagonAbsoluteYDelta : Int
        }
getHexagonCoordDelta x0 y0 x1 y1 =
    let
        ( hexagon0Horizontal, hexagon0X, hexagon0Y ) =
            Hexagons.getHexagonFromXY x0 y0

        ( hexagon1Horizontal, hexagon1X, hexagon1Y ) =
            Hexagons.getHexagonFromXY x1 y1
    in
    { hexagonHorizontalDelta =
        case ( hexagon0Horizontal, hexagon1Horizontal ) of
            ( Hexagons.Left, Hexagons.Left ) ->
                SameLeft

            ( Hexagons.Right, Hexagons.Right ) ->
                SameRight

            ( _, _ ) ->
                Delta
    , lowerHexagonXCoord = min hexagon0X hexagon1X
    , lowerHexagonYCoord = min hexagon0Y hexagon1Y
    , hexagonAbsoluteXDelta = abs (hexagon0X - hexagon1X)
    , hexagonAbsoluteYDelta = abs (hexagon0Y - hexagon1Y)
    }


movePlayer : Model -> Float -> Float -> Model
movePlayer model dx dy =
    case model.mode of
        Editing ->
            model

        Playing ( prevX, prevY ) ->
            let
                ( newX, newY ) =
                    ( prevX + dx, prevY + dy )

                { hexagonHorizontalDelta, lowerHexagonXCoord, lowerHexagonYCoord, hexagonAbsoluteXDelta, hexagonAbsoluteYDelta } =
                    getHexagonCoordDelta prevX prevY newX newY

                allowMovement =
                    if (newX < 0) || (newY < 0) || (newX > toFloat model.width) || (newY > toFloat model.width) then
                        False

                    else if hexagonAbsoluteXDelta == 0 && hexagonAbsoluteYDelta == 0 && not (hexagonHorizontalDelta == Delta) then
                        True

                    else
                        case
                            getLineStyleAtPosition model.height
                                (Hexagons.LineFromTesalatedHexagons
                                    (case ( hexagonHorizontalDelta, hexagonAbsoluteXDelta, hexagonAbsoluteYDelta ) of
                                        ( Delta, 1, 1 ) ->
                                            { x = lowerHexagonXCoord + 1, y = lowerHexagonYCoord + 1, position = Hexagons.TopLeft, direction = Hexagons.Forwards }

                                        ( SameLeft, 0, 1 ) ->
                                            { x = lowerHexagonXCoord, y = lowerHexagonYCoord + 1, position = Hexagons.TopMiddle, direction = Hexagons.Forwards }

                                        ( Delta, 0, 1 ) ->
                                            { x = lowerHexagonXCoord, y = lowerHexagonYCoord + 1, position = Hexagons.TopRight, direction = Hexagons.Forwards }

                                        ( Delta, 0, 0 ) ->
                                            { x = lowerHexagonXCoord, y = lowerHexagonYCoord, position = Hexagons.BottomLeft, direction = Hexagons.Forwards }

                                        ( SameRight, 0, 1 ) ->
                                            { x = lowerHexagonXCoord, y = lowerHexagonYCoord + 1, position = Hexagons.BottomMiddle, direction = Hexagons.Forwards }

                                        ( Delta, 1, 0 ) ->
                                            { x = lowerHexagonXCoord, y = lowerHexagonYCoord, position = Hexagons.BottomRight, direction = Hexagons.Forwards }

                                        _ ->
                                            { x = -1, y = -1, position = Hexagons.TopLeft, direction = Hexagons.Forwards }
                                    )
                                )
                                model.linesFromTesalatedHexagons
                        of
                            Maybe.Nothing ->
                                False

                            Maybe.Just (LineState { isBorder }) ->
                                not isBorder
            in
            if allowMovement then
                { model | mode = Playing ( newX, newY ) }

            else
                model


type BorderInteraction
    = Nothing
    | SetBorder Bool


type LineState
    = LineState { isBorder : Bool }


lineStateToLineProps : Bool -> LineState -> ( Utils.Color, Utils.LineStyle )
lineStateToLineProps isEditing (LineState line) =
    if line.isBorder then
        ( Utils.Color "red", Utils.NoArrow )

    else if isEditing then
        ( Utils.Color "grey", Utils.NoArrow )

    else
        ( Utils.noColor, Utils.NoArrow )


type Mode
    = Editing
    | Playing ( Float, Float )


type alias Model =
    { msg : String
    , mode : Mode
    , cursorPos : Hexagons.LineFromTesalatedHexagons
    , borderInteraction : BorderInteraction
    , spawnPointPos : Int
    , linesFromTesalatedHexagons : Array.Array { line0 : LineState, line1 : LineState, line2 : LineState }
    , width : Int
    , height : Int
    }


updateLineStyleAtPosition : Int -> Hexagons.LineFromTesalatedHexagons -> (b -> ( b, out )) -> out -> Array.Array { a | line0 : b, line1 : b, line2 : b } -> ( Array.Array { a | line0 : b, line1 : b, line2 : b }, out )
updateLineStyleAtPosition height (Hexagons.LineFromTesalatedHexagons position) updateFunc defaultOut hexagonsStyles =
    Utils.updateArray
        (Hexagons.lineFromTesalatedHexagonsToInt (Hexagons.LineFromTesalatedHexagons position) (Basics.toFloat height))
        (\value ->
            case position.position of
                Hexagons.TopLeft ->
                    ( { value | line0 = Tuple.first (updateFunc value.line0) }, Tuple.second (updateFunc value.line0) )

                Hexagons.BottomLeft ->
                    ( { value | line0 = Tuple.first (updateFunc value.line0) }, Tuple.second (updateFunc value.line0) )

                Hexagons.TopMiddle ->
                    ( { value | line1 = Tuple.first (updateFunc value.line1) }, Tuple.second (updateFunc value.line1) )

                Hexagons.BottomMiddle ->
                    ( { value | line1 = Tuple.first (updateFunc value.line1) }, Tuple.second (updateFunc value.line1) )

                Hexagons.TopRight ->
                    ( { value | line2 = Tuple.first (updateFunc value.line2) }, Tuple.second (updateFunc value.line2) )

                Hexagons.BottomRight ->
                    ( { value | line2 = Tuple.first (updateFunc value.line2) }, Tuple.second (updateFunc value.line2) )
        )
        defaultOut
        hexagonsStyles


setLineStyleAtPosition : Int -> Hexagons.LineFromTesalatedHexagons -> c -> Array.Array { a | line0 : c, line1 : c, line2 : c } -> Array.Array { a | line0 : c, line1 : c, line2 : c }
setLineStyleAtPosition height position styleToSet hexagonsStyles =
    Tuple.first (updateLineStyleAtPosition height position (\_ -> ( styleToSet, () )) () hexagonsStyles)


getLineStyleAtPosition : Int -> Hexagons.LineFromTesalatedHexagons -> Array.Array { a | line0 : b, line1 : b, line2 : b } -> Maybe b
getLineStyleAtPosition height position hexagonsStyles =
    Tuple.second (updateLineStyleAtPosition height position (\x -> ( x, Maybe.Just x )) Maybe.Nothing hexagonsStyles)


view : Model -> Html.Html Model
view model =
    let
        (Hexagons.LineFromTesalatedHexagons { direction }) =
            model.cursorPos

        tesalatedHexagonsHexagonTops =
            model.linesFromTesalatedHexagons
                |> Array.map
                    (\hexagonTop ->
                        { line0 = lineStateToLineProps (model.mode == Editing) hexagonTop.line0
                        , line1 = lineStateToLineProps (model.mode == Editing) hexagonTop.line1
                        , line2 = lineStateToLineProps (model.mode == Editing) hexagonTop.line2
                        , hexagonColor = Utils.noColor
                        }
                    )

        tesalatedHexagons =
            Utils.concatMap2
                (\(Hexagons.HexagonTop { line0, line1, line2, hexagon }) ->
                    \hexagonTop ->
                        Utils.svgPath hexagonTop.hexagonColor Utils.noColor hexagon
                            :: Utils.svgArrow hexagonTop.line0 line0
                            ++ Utils.svgArrow hexagonTop.line1 line1
                            ++ Utils.svgArrow hexagonTop.line2 line2
                )
                (Hexagons.tesalatedHexagonLines (Basics.toFloat model.width) (Basics.toFloat model.height))
                (Array.toList
                    (case model.mode of
                        Editing ->
                            tesalatedHexagonsHexagonTops
                                |> setLineStyleAtPosition
                                    model.height
                                    model.cursorPos
                                    ( Utils.Color "black"
                                    , case direction of
                                        Hexagons.Forwards ->
                                            Utils.ArrowAtEnd

                                        Hexagons.Backwards ->
                                            Utils.ArrowAtStart
                                    )
                                |> Utils.updateArray model.spawnPointPos (\props -> ( { props | hexagonColor = Utils.Color "green" }, () )) ()
                                |> Tuple.first

                        Playing _ ->
                            tesalatedHexagonsHexagonTops
                    )
                )
    in
    Html.div
        []
        (Utils.foldl (\_ -> \value -> \acc -> acc ++ [ Html.button [ Html.Events.onClick value.newState ] [ Html.text value.label ] ]) [] (getKeybindings model)
            ++ Html.br [] []
            :: (if model.msg == "" then
                    []

                else
                    [ Html.text model.msg, Html.br [] [] ]
               )
            ++ [ case tesalatedHexagons of
                    Err { lengthOfList1, lengthOfList2 } ->
                        Html.text ("Error: Length of list 1 is " ++ String.fromInt lengthOfList1 ++ ", but length of list 2 is " ++ String.fromInt lengthOfList2)

                    Ok value ->
                        Svg.svg
                            [ SvgAttr.width (String.fromInt model.width)
                            , SvgAttr.height (String.fromInt model.height)
                            , SvgAttr.viewBox ("0 0 " ++ String.fromInt model.width ++ " " ++ String.fromInt model.height)
                            ]
                            (value
                                ++ (case model.mode of
                                        Editing ->
                                            []

                                        Playing ( x, y ) ->
                                            [ Svg.circle [ SvgAttr.r "5", SvgAttr.cx (String.fromFloat x), SvgAttr.cy (String.fromFloat y) ] [] ]
                                   )
                            )
               ]
        )
