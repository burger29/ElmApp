module GraphicalElements exposing (bar, barColor, doubleArrow)

import Svg exposing (..)
import Svg.Attributes as A exposing (class, d, fill, height, id, style, transform, viewBox, width, x, y)


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


barColor : Int -> String
barColor sumResults =
    if sumResults >= 3 then
        "#6cc24a"

    else if sumResults <= -3 then
        "#ff671f"

    else
        "#151746"


doubleArrow : Svg msg
doubleArrow =
    svg
        [ width "22.003"
        , height "16.901"
        , viewBox "0 0 22.003 16.901"
        ]
        [ Svg.path
            [ A.id "Path_45"
            , A.d "M1.274-15.392v4.338l4.789,4.1L1.274-2.829V1.509l9.648-8.468Zm12.355,0v4.338l4.789,4.1-4.789,4.13V1.509l9.648-8.468Z"
            , A.transform "translate(-1.274 15.392)"
            , fill "#f6621c"
            ]
            []
        ]
