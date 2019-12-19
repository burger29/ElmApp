module GraphElements exposing (bar, barColor)

import Svg exposing (..)
import Svg.Attributes as A exposing (class, height, style, viewBox, width, x, y, fill)


bar : Int -> Int -> Svg msg
bar barHeight colorBar =
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
                , fill (barColor colorBar)
                ]
                []
            ]
        ]


barColor: Int -> String
barColor sumResults =
  if sumResults >= 3 then
    "#6cc24a"

  else if sumResults <= -3 then
    "#ff671f"

  else
    "#151746"
