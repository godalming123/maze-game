module Utils exposing (..)

import Array
import Dict
import List
import String
import Svg
import Svg.Attributes as SvgAttr


modByLoop : Int -> number -> number -> ( Int, number )
modByLoop numberOfSubtractions numberBeingSubtractedFrom amountToSubtract =
    if numberBeingSubtractedFrom > amountToSubtract then
        modByLoop (numberOfSubtractions + 1) (numberBeingSubtractedFrom - amountToSubtract) amountToSubtract

    else if numberBeingSubtractedFrom < 0 then
        modByLoop (numberOfSubtractions - 1) (numberBeingSubtractedFrom + amountToSubtract) amountToSubtract

    else
        ( numberOfSubtractions, numberBeingSubtractedFrom )


modBy : number -> number -> ( Int, number )
modBy numberBeingSubtractedFrom amountToSubtract =
    modByLoop 0 numberBeingSubtractedFrom amountToSubtract


type OrderedDict key value
    = OrderedDict { order : List key, items : Dict.Dict key value }


dictFromList : List ( comparable, value ) -> OrderedDict comparable value
dictFromList list =
    OrderedDict { items = Dict.fromList list, order = list |> List.map (\elem -> Tuple.first elem) }


get : comparable -> OrderedDict comparable value -> Maybe value
get key (OrderedDict dict) =
    Dict.get key dict.items


foldl : (comparable -> value -> b -> b) -> b -> OrderedDict comparable value -> b
foldl func acc (OrderedDict { order, items }) =
    List.foldl
        (\item ->
            \a ->
                case Dict.get item items of
                    Just x ->
                        func item x a

                    Nothing ->
                        a
        )
        acc
        order


updateArray : Int -> (a -> ( a, out )) -> out -> Array.Array a -> ( Array.Array a, out )
updateArray index func defaultOut arr =
    case Array.get index arr of
        Nothing ->
            ( arr, defaultOut )

        Just value ->
            let
                ( newValue, out ) =
                    func value
            in
            ( Array.set index newValue arr, out )


mapInPairsLoop : List b -> a -> (a -> a -> b) -> List a -> List b
mapInPairsLoop out prevItem func list =
    case List.head list of
        Just x ->
            mapInPairsLoop (out ++ [ func prevItem x ]) x func (List.drop 1 list)

        Nothing ->
            out


mapInPairs : (a -> a -> b) -> List a -> List b
mapInPairs func list =
    case List.head list of
        Just x ->
            mapInPairsLoop [] x func (List.drop 1 list)

        Nothing ->
            []


concatMap2 : (a -> b -> List result) -> List a -> List b -> Result { lengthOfList1 : Int, lengthOfList2 : Int } (List result)
concatMap2 f l1 l2 =
    let
        l1Len =
            List.length l1

        l2Len =
            List.length l2
    in
    if l1Len == l2Len then
        Result.Ok (List.map2 f l1 l2 |> List.concatMap (\item -> item))

    else
        Result.Err { lengthOfList1 = l1Len, lengthOfList2 = l2Len }


type Line numberType
    = Line ( numberType, numberType ) ( numberType, numberType )


findXy : Float -> Float -> Float -> Float -> ( Float, Float )
findXy startX startY bearingInRadians distance =
    ( startX + sin bearingInRadians * distance, startY + cos bearingInRadians * distance )


findBearingInRadians : Float -> Float -> Float -> Float -> Float
findBearingInRadians x1 y1 x2 y2 =
    atan2 (x2 - x1) (y2 - y1)


type Color
    = Color String


noColor : Color
noColor =
    Color "none"


arrowHead : Float -> Float -> Color -> Float -> Svg.Svg msg
arrowHead x y color bearingInRadians =
    svgPath
        noColor
        color
        [ findXy x y (bearingInRadians - degrees 45) 10
        , ( x, y )
        , findXy x y (bearingInRadians + degrees 45) 10
        ]


type LineStyle
    = NoArrow
    | ArrowAtStart
    | ArrowAtEnd


svgArrow : ( Color, LineStyle ) -> Line Float -> List (Svg.Svg msg)
svgArrow ( Color c, style ) (Line ( x1, y1 ) ( x2, y2 )) =
    Svg.line
        [ SvgAttr.stroke c
        , SvgAttr.x1 (String.fromFloat x1)
        , SvgAttr.y1 (String.fromFloat y1)
        , SvgAttr.x2 (String.fromFloat x2)
        , SvgAttr.y2 (String.fromFloat y2)
        ]
        []
        :: (case style of
                NoArrow ->
                    []

                ArrowAtStart ->
                    [ arrowHead x1 y1 (Color c) (findBearingInRadians x1 y1 x2 y2) ]

                ArrowAtEnd ->
                    [ arrowHead x2 y2 (Color c) (findBearingInRadians x2 y2 x1 y1) ]
           )


svgPath : Color -> Color -> List ( Float, Float ) -> Svg.Svg msg
svgPath (Color fillColor) (Color strokeColor) points =
    Svg.path
        [ SvgAttr.style ("fill:" ++ fillColor ++ ";stroke:" ++ strokeColor ++ ";stroke-width:1;")
        , SvgAttr.d
            ("M"
                ++ String.join " "
                    (points
                        |> List.map
                            (\point ->
                                let
                                    ( x, y ) =
                                        point
                                in
                                String.fromFloat x ++ " " ++ String.fromFloat y
                            )
                    )
            )
        ]
        []
