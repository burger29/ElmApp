module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text, p, ul, li, h1 )
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Array


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = SelectResponse Response Prompt
    | ResetQuiz
    | PreviousQuestion



type alias Model =
  { questions: List String
  , prompts: List Prompt
  , state: ModelState
  , results: Int
  }


type ModelState
  = AnsweringQuestions
  | ShowingResults
  | AllQuestions


init : Model
init =
  { questions = ["double quote","proper syntax"]
  , prompts = (List.indexedMap promptBuilder questionList)
  , state = AnsweringQuestions
  , results = 0
  }



promptBuilder : Int -> String -> Prompt
promptBuilder index question =

      { question = question
      , responseOptions = [ StronglyAgree, Agree , Neutral , Disagree, StronglyDisagree ]
      , selectedResponse =  Nothing
      , index = index
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


type alias Prompt =
  { question: String
  , responseOptions: List Response
  , selectedResponse: Maybe Response
  , index: Int
  }


checkResponses : Prompt -> Bool
checkResponses prompt =
      case prompt.selectedResponse of
        Just selectedResponse -> True
        Nothing -> False


allowSubmit : List Prompt -> Bool
allowSubmit prompts =
      List.all checkResponses prompts


update : Msg -> Model -> Model
update msg model =

    case msg of

        SelectResponse response prompt ->

            let
              beforeIndex =
                List.take prompt.index model.prompts

              afterIndex =
                List.drop (prompt.index + 1) model.prompts

              updatedPrompt =
                { prompt | selectedResponse = Just response }

              updatedPrompts =
                beforeIndex ++ [updatedPrompt] ++ afterIndex

              updatedResults =
                sumResponse updatedPrompts

              updatedModel =
                { model | prompts = updatedPrompts, results = updatedResults }

            in
            updatedModel

        ResetQuiz ->
            init

        PreviousQuestion ->
            init


view : Model -> Html Msg
view model =
    let
        maybeFirstPrompt =
          nextUnansweredQuestion model.prompts
    in
    case model.state of

      AllQuestions ->
          div [ class "container" ]
            [ h1 [ class "text-body" ] [ text "Grade Your Team" ]
            , div [] ( List.map renderQuestion model.prompts )
            , div [] []
            ]

      ShowingResults ->
          div [ class "container" ]
           [ div [] [ text ( String.fromInt (model.results)) ]
           , button [ class "btn btn-primary", onClick ResetQuiz ] [ text "Reset" ]
           ]

      AnsweringQuestions ->
          div [ class "container" ]
            [ h1 [ class "text-body" ] [ text "Grade Your Team" ]
            , div [] [ renderFirstQuestion maybeFirstPrompt ]
            , div [ class "d-flexjustify-content-start" ] [
                button [ class "btn btn-danger" ] [ text "Previous" ]
                -- , button [ class "btn btn-primary", onClick NextQuestion ] [ text "Next" ]
                  ]
            ]


nextUnansweredQuestion : List Prompt -> Maybe Prompt
nextUnansweredQuestion prompts =
      List.filter filterOutUnanswered prompts
        |> List.head

--
-- handlePrompt : Maybe Prompt -> Prompt
-- handlePrompt maybePrompt =
--       case maybePrompt of
--           Just prompt ->
--             prompt
--
--           Nothing ->


filterOutAnswered : Prompt -> Bool
filterOutAnswered prompt =
      case prompt.selectedResponse of
          Just selectedResponse ->
            True

          Nothing ->
            False


filterOutUnanswered : Prompt -> Bool
filterOutUnanswered prompt =
      case prompt.selectedResponse of
          Just selectedResponse ->
            False

          Nothing ->
            True


scoreResponse : Response -> Int
scoreResponse response =
        case response of
          StronglyAgree -> 5
          Agree -> 4
          Neutral -> 3
          Disagree -> 2
          StronglyDisagree -> 1



selectedResponseOrZero : Maybe Response -> Int
selectedResponseOrZero maybeResponse =
      case maybeResponse of
          Just response ->
            scoreResponse response

          Nothing ->
            0


sumResponse : List Prompt -> Int
sumResponse prompts =
      List.map (\ prompt -> selectedResponseOrZero prompt.selectedResponse) prompts
        |> List.sum


--
--
-- scoreResponses : List Prompt -> Int
-- scoreResponses prompts =
--     -- List.sum ( List.map scoreResponse responses )
--     prompts.selectedResponse
--       |> List.map scoreResponse
--       |> List.sum



renderQuestion : Prompt -> Html Msg
renderQuestion prompt =
        div [ class "pt-5 text-light bg-dark" ]
        [  p [ class "pl-3 font-weight-bold" ] [ text prompt.question ]
        , ul [ class "list-group list-group-horizontal" ] ( List.map
        (\ x -> renderResponseList x prompt.selectedResponse prompt) prompt.responseOptions )
        ]



renderFirstQuestion : Maybe Prompt -> Html Msg
renderFirstQuestion maybePrompt =
    case maybePrompt of
      Just prompt ->
        div [ class "pt-5 text-light bg-dark" ]
        [  p [ class "pl-3 font-weight-bold" ] [ text prompt.question ]
        , ul [ class "list-group list-group-horizontal" ] ( List.map
        (\ x -> renderResponseList x prompt.selectedResponse prompt) prompt.responseOptions )
        ]
      Nothing ->
        div [] [ text "Nothing to see here." ]



convertResponse : Response -> String
convertResponse someResponse =
      case someResponse of

        Agree -> "Agree"

        Neutral -> "Neutral"

        Disagree -> "Disagree"

        StronglyAgree -> "Strongly Agree"

        StronglyDisagree -> "Strongly Disagree"


renderResponseList : Response -> Maybe Response -> Prompt -> Html Msg
renderResponseList response maybeSelectedResponse prompt =

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
      , onClick ( SelectResponse response prompt )
      ] [ text ( convertResponse response )]
