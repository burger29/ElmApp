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
  , prompts = initialPrompts
  }


initialPrompts : List Prompt
initialPrompts =
  [
    { question = "Water is good."
    , responseOptions = [Agree, Neutral, Disagree]
    , selectedResponse = Nothing
    , answer = "Agree"
    }
  ]


type alias Model =
  { count: Int
  , questions: List String
  , prompts: List Prompt
  }


type Response
    = Agree
    | Neutral
    | Disagree


type alias Prompt =
  { question: String
  , responseOptions: List Response
  , selectedResponse: Maybe Response
  , answer: String
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
        , div [] ( List.map renderQuestion model.prompts )
        , div [] ( List.map renderAnswer model.prompts )
        ]


renderQuestion : Prompt -> Html msg
renderQuestion prompt =
      p [] [ text prompt.question ]


renderAnswer : Prompt -> Html msg
renderAnswer prompt =
      p [] [ text prompt.answer ]
