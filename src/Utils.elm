module Utils exposing (..)

import List
import String
import Svg
import Svg.Attributes as SvgAttr


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


arrowHead : Float -> Float -> Color -> Float -> Svg.Svg msg
arrowHead x y color bearingInRadians =
    svgPath color
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


svgPath : Color -> List ( Float, Float ) -> Svg.Svg msg
svgPath (Color c) points =
    Svg.path
        [ SvgAttr.style ("fill:none;stroke:" ++ c ++ ";stroke-width:1;")
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
