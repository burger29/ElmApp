module Main exposing (main, update, view)

import Array as Array
import Browser
import Data exposing (questionList, resultsParagraphList, videoframe)
import GraphElements exposing (bar)
import Html exposing (Html, button, div, form, h1, h3, h5, img, input, label, li, p, span, text, ul)
import Html.Attributes as A exposing (action, attribute, class, cols, disabled, for, height, id, maxlength, method, minlength, name, placeholder, required, src, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (FormData, Model, ModelState(..), Msg(..), Prompt, PromptCategory(..), Question(..), Response(..), Results)


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    let
        prompts =
            newPrompts questionList

        emptyFormData =
            { formFirstName = ""
            , formLastName = ""
            , formEmail = ""
            , formCompany = ""
            }
    in
    { prompts = prompts
    , state = selectState prompts
    , results = Results 0 0 0 0
    , formData = emptyFormData
    , allowSubmit = False
    }


newPrompts : List Question -> List Prompt
newPrompts listQuestions =
    List.indexedMap
        (\index (Question s i pc ls) ->
            { question = s
            , responseOptions = [ StronglyAgree, Agree, Neutral, Disagree, StronglyDisagree ]
            , selectedResponse = Nothing
            , variable = i
            , index = index
            , promptCategory = pc
            , feedbackList = ls
            }
        )
        listQuestions


selectState : List Prompt -> ModelState
selectState prompts =
    case nextUnansweredQuestion prompts of
        Just prompt ->
            AnsweringQuestions prompt

        Nothing ->
            FillingOutForm


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
                    { model | prompts = updatedPrompts, results = updatedResults, state = selectState updatedPrompts }
            in
            updatedModel

        ChangeModelState ->
            let
                updatedModelState =
                    { model | state = ShowingResults }

                conditional =
                    model.allowSubmit
            in
            if conditional then
                updatedModelState

            else
                model

        UpdateFormFirstName updatedFirstname ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formFirstName = updatedFirstname }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }

        UpdateFormLastName updatedLastname ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formLastName = updatedLastname }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }

        UpdateFormEmail updatedEmail ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formEmail = updatedEmail }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }

        UpdateFormCompany updatedCompany ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formCompany = updatedCompany }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }


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


isFieldEmpty : String -> Bool
isFieldEmpty field =
    field
        |> String.trim
        |> String.isEmpty
        |> not


checkToSubmit : FormData -> Bool
checkToSubmit input =
    let
        listOfFields =
            []

        checkedFirstName =
            isFieldEmpty input.formFirstName :: listOfFields

        checkedLastName =
            isFieldEmpty input.formLastName :: checkedFirstName

        checkedEmail =
            isFieldEmpty input.formEmail :: checkedLastName

        checkedCompany =
            isFieldEmpty input.formCompany :: checkedEmail
    in
    List.all (\item -> item == True) checkedCompany


feedbackIndex : Response -> Int
feedbackIndex response =
    case response of
        StronglyAgree ->
            2

        Agree ->
            2

        Neutral ->
            1

        Disagree ->
            0

        StronglyDisagree ->
            0


selectedFeedbackOrZero : Maybe Response -> Int
selectedFeedbackOrZero maybeResponse =
    case maybeResponse of
        Just response ->
            feedbackIndex response

        Nothing ->
            0



-- displayFeedback : List String -> Int -> List String
-- displayFeedback feedbackList score =
--     case feedbackList of
--         2 ->
--
--
--VIEW AND HTML MSGS


view : Model -> Html Msg
view model =
    let
        formFirstName =
            model.formData.formFirstName

        formLastName =
            model.formData.formLastName

        formEmail =
            model.formData.formEmail

        formCompany =
            model.formData.formCompany
    in
    case model.state of
        AnsweringQuestions prompt ->
            div []
                [ div [ A.class "container-fluid p-0" ]
                    [ div []
                        [ img [ A.class "img-fluid", src "https://assets.itpro.tv/go/RateYourTeam/mockup.png" ] []
                        ]
                    ]
                , div [ A.class "container" ]
                    [ div [ A.class "intro container" ] [ Html.text "How well do you think your team is performing? Harvard instructor and project management master Jo Peacock can help you access your team’s performance under your leadership. Check out the video below and take the quiz to get your team rated by an expert." ]
                    , div [ A.class "pt-5 row justify-content-center" ] [ div [ A.class "pt-5 embed-responsive embed-responsive-16by9 video-div col-8" ] [ videoframe ] ]
                    , p [ A.class "question-number pt-5" ] [ Html.text (String.fromInt (prompt.index + 1) ++ ".") ]
                    , p [ A.class "question" ] [ Html.text prompt.question ]
                    , ul [ A.class "list-group list-group-horizontal-sm response-list" ]
                        (List.map
                            (\x -> renderResponseList x prompt.selectedResponse prompt)
                            prompt.responseOptions
                        )
                    ]
                ]

        FillingOutForm ->
            div []
                [ div [ A.class "container-fluid p-0" ]
                    [ div []
                        [ img [ A.class "img-fluid", src "https://assets.itpro.tv/go/RateYourTeam/mockup.png" ] []
                        ]
                    ]
                , div [ A.class "container" ]
                    [ div [ A.class "row" ]
                        [ div [ A.class "col-12 col-md-6 offset-md-3 justify-content-center unlock-results" ]
                            [ Html.text "Unlock Your Results!"
                            , form
                                [ A.class "form-group justify-content-center row"
                                , A.method "post"
                                ]
                                [ label [ A.class "sr-only", for "pardot_firstName" ] [ Html.text "First Name" ]
                                , div [ A.class "col-12" ]
                                    [ input
                                        [ onInput UpdateFormFirstName
                                        , A.class "form-control flex-fill mt-4 mr-sm-2"
                                        , placeholder "First Name"
                                        , A.id "pardot_firstName"
                                        , A.name "pardot_firstName"
                                        , A.type_ "text"
                                        , A.value formFirstName
                                        , required True
                                        , minlength 2
                                        , maxlength 40
                                        ]
                                        []
                                    ]
                                , label [ A.class "sr-only", for "pardot_lastName" ] [ Html.text "Last Name" ]
                                , div [ A.class "col-12" ]
                                    [ input
                                        [ onInput UpdateFormLastName
                                        , A.class "form-control flex-fill mt-4 mr-sm-2"
                                        , placeholder "Last Name"
                                        , A.id "pardot_lastName"
                                        , A.name "pardot_lastName"
                                        , A.type_ "text"
                                        , A.value formLastName
                                        , required True
                                        , minlength 2
                                        , maxlength 40
                                        ]
                                        []
                                    ]
                                , label [ A.class "sr-only", for "pardot_email" ] [ Html.text "Email" ]
                                , div [ A.class "col-12" ]
                                    [ input
                                        [ onInput UpdateFormEmail
                                        , A.class "form-control flex-fill mt-4 mr-sm-2"
                                        , placeholder "Email"
                                        , A.id "pardot_email"
                                        , A.name "pardot_email"
                                        , A.type_ "text"
                                        , A.value formEmail
                                        , required True
                                        , minlength 2
                                        , maxlength 40
                                        ]
                                        []
                                    ]
                                , label [ A.class "sr-only", for "pardot_company" ] [ Html.text "Company" ]
                                , div [ A.class "col-12" ]
                                    [ input
                                        [ onInput UpdateFormCompany
                                        , A.class "form-control flex-fill mt-4 mr-sm-2"
                                        , placeholder "Company"
                                        , A.id "pardot_company"
                                        , A.name "pardot_company"
                                        , A.type_ "text"
                                        , A.value formCompany
                                        , required True
                                        , minlength 2
                                        , maxlength 40
                                        ]
                                        []
                                    ]
                                ]
                            , div [ A.class "row justify-content-center pt-4" ]
                                [ button [ A.class "button-submit", onClick ChangeModelState ] [ Html.text "Submit" ]
                                ]
                            ]
                        ]
                    ]
                ]

        ShowingResults ->
            let
                exposeResults =
                    model.results
            in
            div []
                [ div [ A.class "container-fluid p-0" ]
                    [ div []
                        [ img [ A.class "img-fluid", src "https://assets.itpro.tv/go/RateYourTeam/mockup.png" ] []
                        ]
                    ]
                , div [ A.class "container" ]
                    [ div [ A.class "row pt-5 align-items-end no-gutters" ]
                        [ div [ A.class "col-3 offset-0 col-md-2 offset-md-2 bar-style" ]
                            [ bar ((exposeResults.sc + 6) * 12)
                            ]
                        , div [ A.class "col-3 col-md-2 text-center bar-style" ]
                            [ bar ((exposeResults.am + 6) * 12)
                            ]
                        , div [ A.class "col-3 col-md-2 text-center bar-style" ]
                            [ bar ((exposeResults.cl + 6) * 12)
                            ]
                        , div [ A.class "col-3 col-md-2 text-center bar-style" ]
                            [ bar ((exposeResults.cc + 6) * 12)
                            ]
                        ]
                    , div [ A.class "row align-items-start no-gutters" ]
                        [ div [ A.class "col-3 offset-0 col-md-2 offset-md-2 bar-style" ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Safety Culture" ]
                            ]
                        , div [ A.class "col-3 col-md-2 text-center bar-style" ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Agile Mindset" ]
                            ]
                        , div [ A.class "col-3 col-md-2 text-center bar-style" ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Coaching Leadership" ]
                            ]
                        , div [ A.class "col-3 col-md-2 text-center bar-style" ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Collaborative Culture" ]
                            ]
                        ]
                    , div [ A.class "response-header" ] [ Html.text "More about your team" ]

                    -- , div [ A.class "response-style" ] []
                    , div [ A.class "list-courses" ]
                        [ ul []
                            (List.map
                                (\prompt ->
                                    li [] [ Html.text prompt.question ]
                                )
                                    model.prompts
                            )
                        ]
                    ]
                ]


renderQuestion : Prompt -> Html Msg
renderQuestion prompt =
    div [ A.class "pt-5 Html.text-light bg-dark" ]
        [ p [ A.class "pl-3 font-weight-bold" ] [ Html.text prompt.question ]
        , ul [ A.class "list-group list-group-horizontal-sm" ]
            (List.map
                (\x -> renderResponseList x prompt.selectedResponse prompt)
                prompt.responseOptions
            )
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
