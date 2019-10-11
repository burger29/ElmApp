module Main exposing (main, update, view)

import Array as Array
import Browser
import Data exposing (questionList, resultsParagraphList, videoframe)
import GraphElements exposing (bar)
import Html exposing (Html, button, div, h1, h3, img, li, p, text, ul)
import Html.Attributes as A exposing (class, cols, height, src, width)
import Html.Events exposing (onClick)
import Json.Encode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (Model, ModelState(..), Msg(..), Prompt, PromptCategory(..), Question(..), Response(..), Results)


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
                    , responseOptions = [ StronglyAgree, Agree, Neutral, Disagree, StronglyDisagree ]
                    , selectedResponse = Nothing
                    , variable = i
                    , index = index
                    , promptCategory = pc
                    }
                )
                listQuestions
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
                    beforeIndex ++ [ updatedPrompt ] ++ afterIndex

                currentResults =
                    model.results

                updatedResults : Results
                updatedResults =
                    case updatedPrompt.promptCategory of
                        SafetyCulture ->
                            { currentResults
                                | sc =
                                    selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.sc
                            }

                        AgileMindset ->
                            { currentResults
                                | am =
                                    selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.am
                            }

                        CoachingLeadership ->
                            { currentResults
                                | cl =
                                    selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.cl
                            }

                        CollaborativeCulture ->
                            { currentResults
                                | cc =
                                    selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.cc
                            }

                        SCCC ->
                            { currentResults
                                | sc = selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.sc
                                , cc = selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.cc
                            }

                        CCCL ->
                            { currentResults
                                | cl = selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.cl
                                , cc = selectedResponseOrZero updatedPrompt.selectedResponse * updatedPrompt.variable + currentResults.cc
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
        Agree ->
            "AGREE"

        Neutral ->
            "NEUTRAL"

        Disagree ->
            "DISAGREE"

        StronglyAgree ->
            "STRONGLY AGREE"

        StronglyDisagree ->
            "STRONGLY DISAGREE"


checkResponses : Prompt -> Bool
checkResponses prompt =
    case prompt.selectedResponse of
        Just selectedResponse ->
            True

        Nothing ->
            False


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
        StronglyAgree ->
            2

        Agree ->
            1

        Neutral ->
            0

        Disagree ->
            -1

        StronglyDisagree ->
            -2


selectedResponseOrZero : Maybe Response -> Int
selectedResponseOrZero maybeResponse =
    case maybeResponse of
        Just response ->
            scoreResponse response

        Nothing ->
            0


createResultsParagraph : Results -> String
createResultsParagraph value =
    let
        sentenceOne =
            if value.sc >= 1 then
                "String 1"

            else
                "String 2"

        sentenceTwo =
            if value.am >= 1 then
                "String 3"

            else
                "String 4"

        sentenceThree =
            if value.cl >= 1 then
                "String 5"

            else
                "String 6"

        sentenceFour =
            if value.sc >= 1 then
                "String 7"

            else
                "String 8"
    in
    String.join " " [ sentenceOne, sentenceTwo, sentenceThree, sentenceFour ]


createListCourses : Results -> String
createListCourses value =
    let
        sentenceOne =
            if value.sc >= 1 then
                "String 1"

            else
                "String 2"

        sentenceTwo =
            if value.am >= 1 then
                "String 3"

            else
                "String 4"

        sentenceThree =
            if value.cl >= 1 then
                "String 5"

            else
                "String 6"

        sentenceFour =
            if value.sc >= 1 then
                "String 7"

            else
                "String 8"
    in
    String.join " " [ sentenceOne, sentenceTwo, sentenceThree, sentenceFour ]



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
                [ Html.div [ A.class "container-fluid p-0" ]
                    [ Html.div []
                        [ img [ A.class "img-fluid", src "https://assets.itpro.tv/go/RateYourTeam/mockup.png" ] []
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
        [ p [ A.class "pl-3 font-weight-bold" ] [ Html.text prompt.question ]
        , ul [ A.class "list-group list-group-horizontal-sm" ]
            (List.map
                (\x -> renderResponseList x prompt.selectedResponse prompt)
                prompt.responseOptions
            )
        ]


renderFirstQuestion : Model -> Maybe Prompt -> Html Msg
renderFirstQuestion model maybePrompt =
    let
        exposeResults =
            model.results
    in
    case maybePrompt of
        Just prompt ->
            Html.div [ A.class "pt-5" ]
                [ Html.div [ A.class "intro container" ] [ Html.text "How well do you think your team is performing? Harvard instructor and project management master Jo Peacock can help you access your team’s performance under your leadership. Check out the video below and take the quiz to get your team rated by an expert." ]
                , Html.div [ A.class "pt-5 row justify-content-center" ] [ Html.div [ A.class "pt-5 embed-responsive embed-responsive-16by9 video-div col-8" ] [ videoframe ] ]
                , p [ A.class "question-number pt-5" ] [ Html.text (String.fromInt (prompt.index + 1) ++ ".") ]
                , p [ A.class "question" ] [ Html.text prompt.question ]
                , ul [ A.class "list-group list-group-horizontal-sm response-list" ]
                    (List.map
                        (\x -> renderResponseList x prompt.selectedResponse prompt)
                        prompt.responseOptions
                    )
                ]

        Nothing ->
            Html.div [ A.class "container" ]
                [ Html.div [ A.class "row pt-5 align-items-end no-gutters" ]
                    [ Html.div [ A.class "col-3 offset-0 col-md-2 offset-md-2 bar-style" ]
                        [ bar ((exposeResults.sc + 6) * 12)
                        ]
                    , Html.div [ A.class "col-3 col-md-2 text-center bar-style" ]
                        [ bar ((exposeResults.am + 6) * 12)
                        ]
                    , Html.div [ A.class "col-3 col-md-2 text-center bar-style" ]
                        [ bar ((exposeResults.cl + 6) * 12)
                        ]
                    , Html.div [ A.class "col-3 col-md-2 text-center bar-style" ]
                        [ bar ((exposeResults.cc + 6) * 12)
                        ]
                    ]
                , Html.div [ A.class "row align-items-start no-gutters" ]
                    [ Html.div [ A.class "col-3 offset-0 col-md-2 offset-md-2 bar-style" ]
                        [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Safety Culture" ]
                        ]
                    , Html.div [ A.class "col-3 col-md-2 text-center bar-style" ]
                        [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Agile Mindset" ]
                        ]
                    , Html.div [ A.class "col-3 col-md-2 text-center bar-style" ]
                        [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Coaching Leadership" ]
                        ]
                    , Html.div [ A.class "col-3 col-md-2 text-center bar-style" ]
                        [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Collaborative Culture" ]
                        ]
                    ]

                -- , button [ A.class "button-reset", onClick ResetQuiz ] [ Html.text "Reset" ]
                , Html.div [ A.class "response-header" ] [ Html.text "More about your team" ]
                , Html.div [ A.class "response-style" ] [ Html.text (createResultsParagraph exposeResults) ]
                , Html.div [ A.class "list-courses" ] [ Html.text (createListCourses exposeResults) ]
                ]


renderResponseList : Response -> Maybe Response -> Prompt -> Html Msg
renderResponseList response maybeSelectedResponse prompt =
    let
        maybeActive =
            case maybeSelectedResponse of
                Just selectedResponse ->
                    if selectedResponse == response then
                        " active"

                    else
                        ""

                Nothing ->
                    ""
    in
    li
        [ A.class ("button-responses list-group-item flex-fill" ++ maybeActive)
        , onClick (SelectResponse response prompt)
        ]
        [ Html.text (convertResponse response) ]
