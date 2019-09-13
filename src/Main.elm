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
  , state: ModelState
  , results : Int
  }


type ModelState
  = AnsweringQuestions
  | ShowingResults


init : Model
init =
  { questions = ["double quote","proper syntax"]
  , prompts = (List.map promptBuilder questionList)
  , state = AnsweringQuestions
  , results = 0
  }



promptBuilder : String -> Prompt
promptBuilder question =

      { question = question
      , responseOptions = [ StronglyAgree, Agree , Neutral , Disagree, StronglyDisagree ]
      , selectedResponse =  Nothing
    }


questionList : List String
questionList =
  [
    "My team can clearly articulate their goals"
    ,
    "My team feels recognized for their accomplishments"
    ,
    "All team members have personal development plans and see regular progress towards their goals"
    ,
    "My team feels empowered to make decisions"
    ,
    "My team is more efficient when I’m not there"
    ,
    "My team has productive meetings that everyone is involved in (but only when necessary)"
    ,
    "Team members will openly express their opinions and concerns"
    ,
    "Other people want to be on our team"
    ,
    "My team has created their own set of operating guidelines and practices which they are fully bought into"
    ,
    "All team members hold each other, including me, accountable for outcomes"
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


checkResponses : Prompt -> Bool
checkResponses prompt =
      case prompt.selectedResponse of
        Just selectedResponse -> True
        Nothing -> False


allowScoring : List Prompt -> Bool
allowScoring prompts =
      List.all checkResponses prompts


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

              updatedPrompts =
                beforeIndex ++ [updatedPrompt] ++ afterIndex

              updatedState =
                if allowScoring updatedPrompts then
                  ShowingResults
                else
                  AnsweringQuestions

              updatedModel =
                { model | prompts = updatedPrompts, state = updatedState }

            in
            updatedModel



view : Model -> Html Msg
view model =
    case model.state of

      AnsweringQuestions ->
          div [ class "container" ]
            [ h1 [ class "text-body" ] [ text "Grade Your Team" ]
            , div [] ( List.indexedMap renderQuestion model.prompts )
            , div [] []
            ]

      ShowingResults ->
          div [ class "container" ]
           [ div [] [ text ( String.fromInt model.results) ]

           ]



scoreResponse : Response -> Int
scoreResponse selectedResponse =
        case selectedResponse of

          StronglyAgree -> 5
          Agree -> 4
          Neutral -> 3
          Disagree -> 2
          StronglyDisagree -> 1

scoreResponses : List Response -> Int
scoreResponses responses =

  -- let
  --   f : Response -> Int
  --   f = \ response -> scoreResponse response
  --   f     response =  scoreResponse response
  --   f              =  scoreResponse
  -- in
    -- List.sum ( List.map scoreResponse responses )
    responses
      |> List.map scoreResponse
      |> List.sum



renderQuestion : Int -> Prompt -> Html Msg
renderQuestion index prompt =
      div [ class "pt-5 text-light bg-dark" ]
      [  p [ class "pl-3 font-weight-bold" ] [ text prompt.question ]
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
      [ class ( "list-group-item list-group-item-dark list-group-item-action" ++ maybeActive )
      , onClick ( SelectResponse response index prompt )
      ] [ text ( convertResponse response )]
