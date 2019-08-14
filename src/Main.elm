module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Increment
    | Decrement
    | ClearCount

init : Model
init =
  { count = 1
  , questions = ["double quote","proper syntax"]
  }


type alias Model =
  { count: Int
  , questions: List String
  }



update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = (model.count + 1)
            }

        Decrement ->
            { model | count = (model.count - 1)
            }

        ClearCount ->
            { model | count = 0
            }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count ) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] []
        , button [ onClick ClearCount ] [ text "Zero" ]
        , div [] ( List.map renderQuestion model.questions )
        ]


renderQuestion : String -> Html msg
renderQuestion questionstring =
      p [] [ text questionstring ]
