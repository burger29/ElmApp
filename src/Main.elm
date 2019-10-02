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
  { prompts: List Prompt
  , state: ModelState
  , results: Int
  }


type ModelState
  = AnsweringQuestions
  | ShowingResults
  | AllQuestions


init : Model
init =
  { prompts = (List.indexedMap promptBuilder questionList )
  , state = AnsweringQuestions
  , results = 0
  }



promptBuilder : Int -> (String, Int) -> Prompt
promptBuilder index question =

      { question = Tuple.first question
      , responseOptions = [ StronglyAgree, Agree , Neutral , Disagree, StronglyDisagree ]
      , selectedResponse =  Nothing
      , variable = Tuple.second question
      , index = index
    }


questionList : List ( String, Int )
questionList =
  [
    ("My team can clearly articulate their goals", 1 )
    ,
    ("My team feels recognized for their accomplishments", 1)
    ,
    ("All team members have personal development plans and see regular progress towards their goals", 1)
    ,
    ("My team feels empowered to make decisions", 1)
    ,
    ("My team is more efficient when I’m not there", -1)
    ,
    ("My team has productive meetings that everyone is involved in (but only when necessary)", 1)
    ,
    ("Team members will openly express their opinions and concerns", 1)
    ,
    ("Other people want to be on our team", 1)
    ,
    ("My team has created their own set of operating guidelines and practices which they are fully bought into", 1)
    ,
    ("All team members hold each other, including me, accountable for outcomes", 1)
  ]


type Response
    = Agree
    | StronglyAgree
    | Neutral
    | Disagree
    | StronglyDisagree


type alias Prompt =
  { question: String
  , variable: Int
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
            -- let
            --
            --   updatedSelect =
            --     { prompt | selectedResponse = Nothing }
            --
            --   updatedPrompt =
            --     List.singleton updatedSelect
            --
            --   updatedPrompts =
            --     updatedPrompt ++ model.prompts
            --
            --   updatedModel =
            --     { model | prompts = updatedPrompts }
            --
            -- in
            --
            -- case findLastAnswered model.prompts of
            --   Just prompts ->
            --     updatedModel
            --   Nothing ->
            --     init


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
            , div [] [ renderFirstQuestion model maybeFirstPrompt ]
            , div [ class "d-flexjustify-content-start" ] [
                -- button [ class "btn btn-danger" ] [ text "Previous" ]
                -- , button [ class "btn btn-primary", onClick NextQuestion ] [ text "Next" ]
                  ]
            ]


nextUnansweredQuestion : List Prompt -> Maybe Prompt
nextUnansweredQuestion prompts =
      List.filter filterOutUnanswered prompts
        |> List.head


answeredQuestions : List Prompt -> List Prompt
answeredQuestions prompts =
      List.filter filterOutAnswered prompts


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


findLastAnswered : List Prompt -> Maybe Prompt
findLastAnswered prompts =
      let
        listLength =
          List.filter filterOutAnswered prompts
            |> List.length
      in

      answeredQuestions prompts
        |> List.drop (listLength - 1)
          |> List.head




scoreResponse : Response -> Int
scoreResponse response =
        case response of
          StronglyAgree -> 2
          Agree -> 1
          Neutral -> 0
          Disagree -> -1
          StronglyDisagree -> -2



selectedResponseOrZero : Maybe Response -> Int
selectedResponseOrZero maybeResponse =
      case maybeResponse of
          Just response ->
            scoreResponse response

          Nothing ->
            0


sumResponse : List Prompt -> Int
sumResponse prompts =
      List.map (\ prompt -> selectedResponseOrZero prompt.selectedResponse * prompt.variable ) prompts
        |> List.sum



renderQuestion : Prompt -> Html Msg
renderQuestion prompt =
        div [ class "pt-5 text-light bg-dark" ]
        [  p [ class "pl-3 font-weight-bold" ] [ text prompt.question ]
        , ul [ class "list-group list-group-horizontal" ] ( List.map
        (\ x -> renderResponseList x prompt.selectedResponse prompt) prompt.responseOptions )
        ]



renderFirstQuestion : Model -> Maybe Prompt -> Html Msg
renderFirstQuestion model maybePrompt =
    case maybePrompt of
      Just prompt ->
        div [ class "pt-5" ]
        [ p [] [ text (String.fromInt (prompt.index + 1)) ]
        , p [ class "pl-3 font-weight-bold" ] [ text prompt.question ]
        , ul [ class "list-group list-group-horizontal" ] ( List.map
        (\ x -> renderResponseList x prompt.selectedResponse prompt) prompt.responseOptions )
        -- , button [ class "btn btn-danger", onClick (PreviousQuestion prompt) ] [ text "Previous" ]
        ]
      Nothing ->
        div [] [ div [] [ text ( String.fromInt (model.results)) ]
          , button [ class "btn btn-primary", onClick ResetQuiz ] [ text "Reset" ] ]



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
      [ class ( "list-group-item list-group-item-action inactive-button" ++ maybeActive )
      , onClick ( SelectResponse response prompt )
      ] [ text ( convertResponse response )]
