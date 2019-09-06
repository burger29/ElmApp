module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text, p, ul, li, h1 )
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Array


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = SelectResponse Response Int Prompt

type alias Model =
  { questions: List String
  , prompts: List Prompt
  }


init : Model
init =
  { questions = ["double quote","proper syntax"]
  , prompts = initialPrompts
  }


initialPrompts : List Prompt
initialPrompts =
  [
    { question = "Water is good."
    , responseOptions = [ StronglyAgree, Agree , Neutral , Disagree, StronglyDisagree ]
    , selectedResponse =  Nothing
    }
    ,
    { question = "You can nuke hurricanes."
    , responseOptions = [ StronglyAgree, Agree , Neutral , Disagree, StronglyDisagree ]
    , selectedResponse =  Nothing
    }
    ,
    { question = "Your team needs ITProTV."
    , responseOptions = [ StronglyAgree, Agree , Neutral , Disagree, StronglyDisagree ]
    , selectedResponse =  Nothing
    }
  ]


type Response
    = Agree
    | StronglyAgree
    | Neutral
    | Disagree
    | StronglyDisagree


convertResponse : Response -> String
convertResponse someResponse =
      case someResponse of

        Agree -> "Agree"

        Neutral -> "Neutral"

        Disagree -> "Disagree"

        StronglyAgree -> "Strongly Agree"

        StronglyDisagree -> "Strongly Disagree"



type alias Prompt =
  { question: String
  , responseOptions: List Response
  , selectedResponse: Maybe Response
  }


update : Msg -> Model -> Model
update msg model =
    case msg of

        SelectResponse response index prompt ->

            let
              beforeIndex =
                List.take index model.prompts

              afterIndex =
                List.drop (index + 1) model.prompts

              updatedPrompt =
                { prompt | selectedResponse = Just response }

              updatedModel =
                { model | prompts = beforeIndex ++ [updatedPrompt] ++ afterIndex }


            in
            updatedModel



view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "text-body text-monospace" ] [ text "Grade Your Team" ]
        , div [] ( List.indexedMap renderQuestion model.prompts )
        , div [ class "pt-4 pb-4 pl-3 bg-dark text-light text-monospace"] [ text ( renderResponses model.prompts )]
        ]


renderResponses : List Prompt -> String
renderResponses prompts =
      let
          answerList =
            List.indexedMap
                (\ index prompt ->
                  case prompt.selectedResponse of
                    Just response ->
                      ( String.fromInt (index + 1) ) ++ ". " ++ convertResponse response ++ " "
                    Nothing ->
                      ( String.fromInt (index + 1) ) ++ ". No Response "
                ) prompts
      in
        String.concat answerList


renderQuestion : Int -> Prompt -> Html Msg
renderQuestion index prompt =
      div [ class "pt-5 text-light bg-dark" ]
      [  p [ class "pl-3 font-weight-bold text-monospace" ] [ text prompt.question ]
      , ul [ class "list-group list-group-horizontal" ] ( List.map
      (\ x -> renderResponseList x index prompt.selectedResponse prompt) prompt.responseOptions )
      ]


renderResponseList : Response -> Int -> Maybe Response -> Prompt -> Html Msg
renderResponseList response index maybeSelectedResponse prompt =

    let
      maybeActive =

        case maybeSelectedResponse of

          Just selectedResponse ->
            if selectedResponse == response then
              " active"
            else ""
          Nothing ->
            ""
    in

    li
      [ class ( "list-group-item list-group-item-dark text-monospace list-group-item-action" ++ maybeActive )
      , onClick ( SelectResponse response index prompt )
      ] [ text ( convertResponse response )]
