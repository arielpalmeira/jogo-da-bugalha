module Dice exposing (side0, side1, side2, side3, side4, side5, side6)

import Element exposing (Color, toRgb)
import Hex
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


colorAsText : Color -> String
colorAsText color =
    let
        { red, green, blue } =
            toRgb color

        red_ =
            red * 255 |> round |> Hex.toString

        green_ =
            green * 255 |> round |> Hex.toString

        blue_ =
            blue * 255 |> round |> Hex.toString
    in
    String.concat [ "#", red_, green_, blue_ ]


pip : Color -> Int -> Int -> Int -> Svg msg
pip color r_ x_ y_ =
    circle
        [ cx <| String.fromInt x_
        , cy <| String.fromInt y_
        , r <| String.fromInt r_
        , fill <| colorAsText color
        ]
        []


base : Color -> List (Svg msg) -> Html msg
base color pips =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        (rect
            [ width "110"
            , height "110"
            , x "5"
            , y "5"
            , rx "15"
            , ry "15"
            , fillOpacity "0"
            , strokeWidth "5"
            , stroke <| colorAsText color
            ]
            []
            :: pips
        )


side0 : Color -> Html msg
side0 color =
    base color []


side1 : Color -> Html msg
side1 color =
    base color [ pip color 30 60 60 ]


side2 : Color -> Html msg
side2 color =
    let
        c_ x_ y_ =
            pip color 20 x_ y_
    in
    base color
        [ c_ 80 40
        , c_ 40 80
        ]


side3 : Color -> Html msg
side3 color =
    let
        c_ x_ y_ =
            pip color 15 x_ y_
    in
    base color
        [ c_ 85 35
        , c_ 60 60
        , c_ 35 85
        ]


side4 : Color -> Html msg
side4 color =
    let
        c_ x_ y_ =
            pip color 15 x_ y_
    in
    base color
        [ c_ 35 35
        , c_ 85 35
        , c_ 35 85
        , c_ 85 85
        ]


side5 : Color -> Html msg
side5 color =
    let
        c_ x_ y_ =
            pip color 10 x_ y_
    in
    base color
        [ c_ 35 35
        , c_ 85 35
        , c_ 60 60
        , c_ 35 85
        , c_ 85 85
        ]


side6 : Color -> Html msg
side6 color =
    let
        c_ x_ y_ =
            pip color 10 x_ y_
    in
    base color
        [ c_ 35 35
        , c_ 85 35
        , c_ 35 60
        , c_ 85 60
        , c_ 35 85
        , c_ 85 85
        ]
