module GraphElements exposing (bar)

import Svg exposing (..)
import Svg.Attributes exposing (style, height, width, x, y, viewBox, class)


bar : Int -> Svg msg
bar barHeight =
  svg
  [ x "0px"
  , y "0px"
  , viewBox ("-25 0 100 " ++ (String.fromInt barHeight))
  , style "enable-background:new 0 0 175 400;"
  ]
  [
    g []
      [
        rect [ x "0.5"
              , y "0"
              , class "st0"
              , width "50"
              , height (String.fromInt barHeight)
             ] []
      ]
  ]