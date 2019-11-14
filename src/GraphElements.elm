module GraphElements exposing (bar)

import Svg exposing (..)
import Svg.Attributes as A exposing (class, height, style, viewBox, width, x, y)


bar : Int -> Svg msg
bar barHeight =
    svg
        [ x "0px"
        , y "0px"
        , viewBox ("-25 0 100 " ++ String.fromInt barHeight)
        , A.style "enable-background:new 0 0 175 400;"
        ]
        [ g []
            [ rect
                [ x "0.5"
                , y "0"
                , class "st0"
                , width "50"
                , height (String.fromInt barHeight)
                ]
                []
            ]
        ]
