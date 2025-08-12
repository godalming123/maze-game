module Main exposing (..)

import Array
import Browser
import Browser.Events
import Hexagons
import Html
import Json.Decode
import List
import Svg
import Svg.Attributes as SvgAttr
import Utils



-- TODO: Resize the hexagon grid to fill the browser window
-- TODO: Make the hexagon grid infinite


main : Program () Model (List Msg)
main =
    Browser.element
        { init =
            \_ ->
                ( Model
                    { cursorPos =
                        Hexagons.LineFromTesalatedHexagons
                            { x = 0
                            , y = 0
                            , position = Hexagons.TopLeft
                            , direction = Hexagons.Forwards
                            }
                    , borderInteraction = Nothing
                    , linesFromTesalatedHexagons =
                        Array.repeat
                            -- TODO: Do not hard code 90
                            90
                            { isBorder = False }
                    , width = 500
                    , height = 500
                    }
                , Cmd.none
                )
        , update =
            \msgs ->
                \model ->
                    ( List.foldl
                        (\msg -> \m -> update msg m)
                        model
                        msgs
                    , Cmd.none
                    )
        , view = view
        , subscriptions =
            \_ ->
                Browser.Events.onKeyDown
                    (Json.Decode.map
                        (\s ->
                            case s of
                                "h" ->
                                    [ MoveCursor Hexagons.MoveLeft ]

                                "l" ->
                                    [ MoveCursor Hexagons.MoveRight ]

                                "k" ->
                                    [ MoveCursor Hexagons.MoveLeft, MoveCursor Hexagons.MoveRight, MoveCursor Hexagons.MoveRight, MoveCursor Hexagons.MoveLeft ]

                                "j" ->
                                    [ MoveCursor Hexagons.MoveLeft, MoveCursor Hexagons.MoveRight ]

                                "t" ->
                                    [ ToggleDirection ]

                                "b" ->
                                    [ ToggleBorderCreation ]

                                x ->
                                    Debug.log ("Unrecognized keybinding: " ++ x)
                                        []
                        )
                        (Json.Decode.field "key" Json.Decode.string)
                    )
        }


type Msg
    = MoveCursor Hexagons.LineMovementDirection
    | ToggleDirection
    | ToggleBorderCreation


type BorderInteraction
    = Nothing
    | SetBorder Bool


type Model
    = Model
        { cursorPos : Hexagons.LineFromTesalatedHexagons
        , borderInteraction : BorderInteraction
        , linesFromTesalatedHexagons : Array.Array { isBorder : Bool }
        , width : Int
        , height : Int
        }


update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        MoveCursor direction ->
            Model
                { model
                    | cursorPos = Hexagons.moveLineFromTesalatedHexagons model.cursorPos direction (Basics.toFloat model.width) (Basics.toFloat model.height)
                    , linesFromTesalatedHexagons =
                        case model.borderInteraction of
                            Nothing ->
                                model.linesFromTesalatedHexagons

                            SetBorder active ->
                                Array.set
                                    (Hexagons.lineFromTesalatedHexagonsToInt model.cursorPos (Basics.toFloat model.height))
                                    { isBorder = active }
                                    model.linesFromTesalatedHexagons
                }

        ToggleDirection ->
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
            Model { model | cursorPos = Hexagons.LineFromTesalatedHexagons { cursorPos | direction = newDirection } }

        ToggleBorderCreation ->
            Model
                { model
                    | borderInteraction =
                        case model.borderInteraction of
                            Nothing ->
                                SetBorder True

                            SetBorder True ->
                                SetBorder False

                            SetBorder False ->
                                Nothing
                }


view : Model -> Html.Html (List Msg)
view (Model { cursorPos, linesFromTesalatedHexagons, width, height, borderInteraction }) =
    let
        (Hexagons.LineFromTesalatedHexagons { direction }) =
            cursorPos

        cursorIndex =
            Hexagons.lineFromTesalatedHexagonsToInt cursorPos (Basics.toFloat height)

        tesalatedHexagonsLinesColors =
            Array.map
                (\line ->
                    if line.isBorder then
                        ( Utils.Color "red", Utils.NoArrow )

                    else
                        ( Utils.Color "grey", Utils.NoArrow )
                )
                linesFromTesalatedHexagons
                |> Array.set cursorIndex
                    ( Utils.Color "black"
                    , case direction of
                        Hexagons.Forwards ->
                            Utils.ArrowAtEnd

                        Hexagons.Backwards ->
                            Utils.ArrowAtStart
                    )

        tesalatedHexagons =
            Utils.concatMap2 Utils.svgArrow
                (Array.toList tesalatedHexagonsLinesColors)
                (Hexagons.tesalatedHexagonLines (Basics.toFloat width) (Basics.toFloat height))
    in
    Html.div
        []
        [ Html.text
            -- TODO: Use icons to indicate how keybindings move the cursor instead of text
            -- TODO: Make it posisble to press these as buttons for devices without a keyboard
            ("Ⓗ Move left Ⓛ Move right Ⓙ Move left then right Ⓚ Move left, right, right, and left Ⓣ Toggle direction Ⓑ "
                ++ (case borderInteraction of
                        -- TDOO: Use an icon at the back of the cursor to indicate the `borderCreation` state instead of text
                        Nothing ->
                            "Not changing borders"

                        SetBorder True ->
                            "Creating borders"

                        SetBorder False ->
                            "Removing borders"
                   )
            )
        , Html.br [] []
        , case tesalatedHexagons of
            Err { lengthOfList1, lengthOfList2 } ->
                Html.text ("Error: Length of list 1 is " ++ String.fromInt lengthOfList1 ++ ", but length of list 2 is " ++ String.fromInt lengthOfList2)

            Ok value ->
                Svg.svg
                    [ SvgAttr.width (String.fromInt width)
                    , SvgAttr.height (String.fromInt height)
                    , SvgAttr.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
                    ]
                    value
        ]
