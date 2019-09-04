module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text, p, ul, li )
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Array


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = ClearCount
    | SelectResponse Response Int Prompt

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
    , responseOptions = [Agree , Neutral , Disagree ]
    , selectedResponse =  Nothing
    }
    ,
    { question = "You can nuke hurricanes."
    , responseOptions = [Agree , Neutral , Disagree ]
    , selectedResponse =  Nothing
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


responseArray = Array.fromList [ "Agree", "Neutral", "Disagree" ]


convertResponse : Response -> String
convertResponse someResponse =
      case someResponse of

        Agree -> "Agree"

        Neutral -> "Neutral"

        Disagree -> "Disagree"



type alias Prompt =
  {
  question: String
  , responseOptions: List Response
  , selectedResponse: Maybe Response
  }


update : Msg -> Model -> Model
update msg model =
    case msg of

        ClearCount ->
            { model | count = 0
            }

        SelectResponse response index prompt ->

            let
              beforeIndex =
                List.take index model.prompts

              afterIndex =
                List.drop (index + 1) model.prompts

              updatedPrompt =
                { prompt | selectedResponse = Just response }

              promptWithSelection =
                { }



            in
            model



view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [] [ text (String.fromInt model.count ) ]
        , div [] []
        , button [ onClick ClearCount, class "btn btn-info" ] [ text "Zero" ]
        , div [] ( List.indexedMap renderQuestion model.prompts )
        ]


renderQuestion : Int -> Prompt -> Html Msg
renderQuestion index prompt =
      div []
      [  p [] [ text prompt.question ]
      , ul [ class "list-group" ] ( List.map
      (\ x -> renderResponseList x index prompt.selectedResponse prompt) prompt.responseOptions )
      ]


renderResponseList : Response -> Int -> Maybe Response -> Prompt -> Html Msg
renderResponseList response index maybeSelectedResponse prompt =

    let
      maybeActive =

        case maybeSelectedResponse of

          Just selectedResponse ->
            if selectedResponse == response then
              "active"
            else ""
          Nothing ->
            ""
    in

    li
      [ class ( "list-group-item list-group-item-action" ++ maybeActive )
      , onClick ( SelectResponse response index prompt )
      ] [ text ( convertResponse response )]
