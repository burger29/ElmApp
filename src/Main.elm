module Main exposing (main, update, view)

import Browser
import Html exposing (Html, button, div, text, p, ul, li, h1, img )
import Html.Events exposing (onClick)
import Html.Attributes as A exposing (class, src, height, width, cols)
import Array as Array
import Data exposing (questionList)
import Types exposing (PromptCategory(..), Question(..), Msg(..), Prompt, Response(..), Model, ModelState(..), HighOrLow(..), Results)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import GraphElements exposing (bar)



main =
    Browser.sandbox { init = init, update = update, view = view }



init : Model
init =
  let
    newPrompts : List Question -> List Prompt
    newPrompts listQuestions =
      List.indexedMap
        (\index (Question s i pc) ->
          { question = s
          , responseOptions = [ StronglyAgree, Agree , Neutral , Disagree, StronglyDisagree ]
          , selectedResponse =  Nothing
          , variable = i
          , index = index
          , promptCategory = pc
          }
        ) listQuestions

  in

  { prompts = newPrompts questionList
  , state = AnsweringQuestions
  , results = Results 0 0 0 0
  }


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

              currentResults =
                model.results

              updatedResults : Results
              updatedResults  =
                case updatedPrompt.promptCategory of
                  SafetyCulture ->
                    { currentResults | sc =
                      (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.sc
                    }
                  AgileMindset ->
                    { currentResults | am =
                      (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.am
                    }
                  CoachingLeadership ->
                    { currentResults | cl =
                      (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.cl
                    }
                  CollaborativeCulture ->
                    { currentResults | cc =
                      (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.cc
                    }
                  SCCC ->
                    { currentResults
                      | sc = (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.sc
                      , cc = (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.cc
                    }
                  CCCL ->
                    { currentResults
                      | cl = (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.cl
                      , cc = (selectedResponseOrZero updatedPrompt.selectedResponse) * updatedPrompt.variable + currentResults.cc
                    }

              updatedModel =
                { model | prompts = updatedPrompts, results = updatedResults }

            in
            updatedModel

        ResetQuiz ->
            init


convertResponse : Response -> String
convertResponse someResponse =
      case someResponse of

        Agree -> "AGREE"

        Neutral -> "NEUTRAL"

        Disagree -> "DISAGREE"

        StronglyAgree -> "STRONGLY AGREE"

        StronglyDisagree -> "STRONGLY DISAGREE"


checkResponses : Prompt -> Bool
checkResponses prompt =
      case prompt.selectedResponse of
        Just selectedResponse -> True
        Nothing -> False


allowSubmit : List Prompt -> Bool
allowSubmit prompts =
      List.all checkResponses prompts


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


--VIEW AND HTML MSGS

view : Model -> Html Msg
view model =
    let
        maybeFirstPrompt =
          nextUnansweredQuestion model.prompts
    in
    case model.state of

      ShowingResults ->
          Html.div [ A.class "container" ] []

      AnsweringQuestions ->
        Html.div []
          [ Html.div [ A.class "container-fluid" ]
              [ Html.div []
                [ img [ A.class "img-fluid", src "https://assets.itpro.tv/go/RateYourTeam/mockup.png"] []
                ]
              ]
          , Html.div [ A.class "container" ]
              [ Html.div [] [ renderFirstQuestion model maybeFirstPrompt ]
              , Html.div [ A.class "d-flexjustify-content-start" ] []
              ]
          ]

renderQuestion : Prompt -> Html Msg
renderQuestion prompt =
        Html.div [ A.class "pt-5 Html.text-light bg-dark" ]
        [  p [ A.class "pl-3 font-weight-bold" ] [ Html.text prompt.question ]
        , ul [ A.class "list-group list-group-horizontal-sm" ] ( List.map
        (\ x -> renderResponseList x prompt.selectedResponse prompt) prompt.responseOptions )
        ]



renderFirstQuestion : Model -> Maybe Prompt -> Html Msg
renderFirstQuestion model maybePrompt =
    case maybePrompt of
      Just prompt ->
        Html.div [ A.class "pt-5" ]
        [ Html.div [ A.class "intro"]
          [ Html.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur."
          ]
        , Html.div [] [ Html.text "video will go here"]
        , p [ A.class "question-number" ] [ Html.text (String.fromInt (prompt.index + 1) ++ ".") ]
        , p [ A.class "question" ] [ Html.text prompt.question ]
        , ul [ A.class "list-group list-group-horizontal-sm" ] ( List.map
        (\ x -> renderResponseList x prompt.selectedResponse prompt) prompt.responseOptions )
        ]
      Nothing ->
        Html.div [ A.class "container" ]
          [ Html.div [ A.class "row align-items-end" ]
              [ Html.div [ A.class "col-3 text-center"]
                [ bar 30
                , Html.text "Safety Culture"
                ]
              , Html.div [ A.class "col-3 text-center"]
                [ bar 60
                , Html.text "Agile Mindset"
                ]
              , Html.div [ A.class "col-3 text-center"]
                [ bar 30
                , Html.text "Coaching Leadership"
                ]
              , Html.div [ A.class "col-3 text-center"]
                [ bar 90
                , Html.text "Collaborative Culture"
                ]
              ]
          , button [ A.class "button-reset", onClick ResetQuiz ] [ Html.text "Reset" ]
          ]



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
      [ A.class ( "button-responses list-group-item flex-fill" ++ maybeActive )
      , onClick ( SelectResponse response prompt )
      ] [ Html.text ( convertResponse response )]
