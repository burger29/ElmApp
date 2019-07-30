module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Increment
    | Decrement
    | ClearCount

init : Model
init =
  { count = 2
  }


type alias Model =
  { count: Int
  }



update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { count = (model.count + 1)
            }

        Decrement ->
            { count = (model.count - 1)
            }

        ClearCount ->
            { count = 0
            }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count ) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] []
        , button [ onClick ClearCount ] [ text "Zero" ]
        ]
