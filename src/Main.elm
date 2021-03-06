module Main exposing (main, update, view)

import Array as Array
import Browser
import Data exposing (questionList, videoframe)
import GraphicalElements exposing (bar, doubleArrow)
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , form
        , h1
        , h3
        , h5
        , img
        , input
        , label
        , li
        , p
        , span
        , text
        , ul
        )
import Html.Attributes as A
    exposing
        ( action
        , alt
        , attribute
        , class
        , cols
        , disabled
        , for
        , height
        , id
        , maxlength
        , method
        , minlength
        , name
        , pattern
        , placeholder
        , property
        , required
        , src
        , type_
        , value
        , width
        )
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
import Url as Url
import Url.Parser as UrlParser exposing ((<?>))
import Url.Parser.Query as Query
import Json.Decode as Decode
import Json.Encode
import Ports exposing (saveResponses, saveResults)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types
    exposing
        ( Feedback(..)
        , Flags
        , FormData
        , Model
        , ModelState(..)
        , Msg(..)
        , Prompt
        , PromptCategory(..)
        , Question(..)
        , Response(..)
        , Results
        , unwrapFeedback
        )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        prompts =
            newPrompts questionList

        emptyFormData =
            { formFirstName = ""
            , formLastName = ""
            , formEmail = ""
            , formCompany = ""
            }

        savedResponses =
            case flags.startingState of
                Just responses ->
                    List.map intToSelectedResponse responses

                Nothing ->
                    List.repeat (List.length prompts) Nothing

        savedPrompts =
            List.map2
                (\savedResponse prompt ->
                    { prompt | selectedResponse = savedResponse }
                )
                savedResponses
                prompts

        savedResults =
            case Decode.decodeValue Decode.string flags.startingResults of
                Ok results ->
                    decodeResults results

                Err _ ->
                    Results 0 0 0 0

        maybePageUrl =
            Url.fromString flags.pageUrl



    in
    ( { prompts = savedPrompts
      , state = selectState savedPrompts maybePageUrl
      , results = savedResults
      , formData = emptyFormData
      , allowSubmit = False
      , maybePageUrl = maybePageUrl
      }
    , Cmd.none
    )


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


decodeResults : String -> Results
decodeResults resultsJson =
    case Decode.decodeString resultsDecoder resultsJson of
        Ok results ->
            results

        Err _ ->
            Results 0 0 0 -3


resultsDecoder : Decode.Decoder Results
resultsDecoder =
    Decode.map4 Results
        (Decode.field "sc" Decode.int)
        (Decode.field "am" Decode.int)
        (Decode.field "cl" Decode.int)
        (Decode.field "cc" Decode.int)


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


selectState : List Prompt -> Maybe Url.Url -> ModelState
selectState prompts maybePageUrl =
    let
        nextPrompt =
            case nextUnansweredQuestion prompts of
                Just prompt ->
                    AnsweringQuestions prompt

                Nothing ->
                    FillingOutForm

        maybeModelState =
          case maybePageUrl of
              Just url ->
                  UrlParser.parse (parseModelState prompts) url

              Nothing ->
                  Just ErrorState

    in
    Maybe.withDefault ErrorState maybeModelState



parseModelState : List Prompt -> UrlParser.Parser (ModelState -> a) a
parseModelState prompts =
    UrlParser.s "rate-your-team" <?> modelStateQueryParser prompts


modelStateQueryParser : List Prompt -> Query.Parser ModelState
modelStateQueryParser prompts =
    Query.map (modelStateFromString prompts) (Query.string "submit")


modelStateFromString: List Prompt -> Maybe String -> ModelState
modelStateFromString prompts maybeQuery  =
    case maybeQuery of
        Just query ->
          if query == "success" then
            ShowingResults
          else
            FillingOutForm

        Nothing ->
            case nextUnansweredQuestion prompts of
                Just prompt ->
                    AnsweringQuestions prompt

                Nothing ->
                    FillingOutForm


update : Msg -> Model -> ( Model, Cmd msg )
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
                    { model | prompts = updatedPrompts, results = updatedResults, state = selectState updatedPrompts model.maybePageUrl }

                responsesList =
                    List.map selectedResponseToInt updatedModel.prompts
            in
            ( updatedModel, Cmd.batch [ saveResponses responsesList, saveResults updatedResults ] )

        ChangeModelState ->
            let
                updatedModelState =
                    { model | state = ShowingResults }

                conditional =
                    model.allowSubmit
            in
            if conditional then
                ( updatedModelState, Cmd.none )

            else
                ( model, Cmd.none )

        UpdateFormFirstName updatedFirstname ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formFirstName = updatedFirstname }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            ( { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }, Cmd.none )

        UpdateFormLastName updatedLastname ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formLastName = updatedLastname }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            ( { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }, Cmd.none )

        UpdateFormEmail updatedEmail ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formEmail = updatedEmail }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            ( { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }, Cmd.none )

        UpdateFormCompany updatedCompany ->
            let
                formData =
                    model.formData

                updatedFormData =
                    { formData | formCompany = updatedCompany }

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            ( { model | formData = updatedFormData, allowSubmit = updatedAllowSubmit }, Cmd.none )

        UpdateCheckToSubmit ->
            let
                formData =
                    model.formData

                updatedAllowSubmit =
                    checkToSubmit formData
            in
            ( { model | allowSubmit = updatedAllowSubmit }, Cmd.none )


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


feedbackToIndex : Response -> Int
feedbackToIndex response =
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


promptToFeedback : Prompt -> Feedback
promptToFeedback prompt =
    let
        feedbackIndex =
            feedbackToIndex <|
                Maybe.withDefault Neutral prompt.selectedResponse
    in
    Maybe.withDefault (Feedback "No feedback found." prompt.promptCategory) <|
        Array.get feedbackIndex (Array.fromList prompt.feedbackList)


selectedResponseToInt : Prompt -> Int
selectedResponseToInt prompt =
    case prompt.selectedResponse of
        Just response ->
            case response of
                StronglyAgree ->
                    1

                Agree ->
                    2

                Neutral ->
                    3

                Disagree ->
                    4

                StronglyDisagree ->
                    5

        Nothing ->
            0


intToSelectedResponse : Int -> Maybe Response
intToSelectedResponse flag =
    case flag of
        1 ->
            Just StronglyAgree

        2 ->
            Just Agree

        3 ->
            Just Neutral

        4 ->
            Just Disagree

        5 ->
            Just StronglyDisagree

        _ ->
            Nothing


resultsToIndex : Int -> Int
resultsToIndex results =
    if results >= 3 then
        0

    else if results <= -3 then
        2

    else
        1


resultsToCourses : Int -> List String -> String
resultsToCourses x courses =
    Array.fromList courses
        |> Array.get (resultsToIndex x)
        |> Maybe.withDefault "No courses."


resultsToOverviews : Int -> List String -> String
resultsToOverviews x courses =
    Array.fromList courses
        |> Array.get (resultsToIndex x)
        |> Maybe.withDefault "https://player.vimeo.com/video/391234360?autoplay=0"


feedbackHandler : List PromptCategory -> List Prompt -> List (Html msg)
feedbackHandler promptCategories prompts =
    List.foldl
        (\category allFeedback ->
            case category of
                SafetyCulture ->
                    allFeedback ++ groupFeedback [ filteredFeedback category prompts ++ filteredFeedback SCCC prompts ] category

                CollaborativeCulture ->
                    allFeedback ++ groupFeedback [ filteredFeedback category prompts ++ filteredFeedback CCCL prompts ++ filteredFeedback SCCC prompts ] category

                CoachingLeadership ->
                    allFeedback ++ groupFeedback [ filteredFeedback category prompts ++ filteredFeedback CCCL prompts ] category

                SCCC ->
                    allFeedback

                CCCL ->
                    allFeedback

                _ ->
                    allFeedback ++ groupFeedback [ filteredFeedback category prompts ] category
        )
        []
        promptCategories


groupFeedback : List (List (Html msg)) -> PromptCategory -> List (Html msg)
groupFeedback feedbackLists pc =
    [ Html.h4 [ A.class "text-center feeback-header" ]
        [ Html.text <|
            "Insights on your team's "
                ++ promptCategoryToString pc
        ]
    , ul [ A.class "list-feedback pb-5" ] <|
        List.concat feedbackLists
    ]


filteredFeedback : PromptCategory -> List Prompt -> List (Html msg)
filteredFeedback pc prompts =
    List.filter
        (\prompt -> prompt.promptCategory == pc)
        prompts
        |> List.map
            (\prompt ->
                li []
                    [ promptToFeedback prompt
                        |> unwrapFeedback
                        |> Html.text
                    ]
            )


promptCategoryToString : PromptCategory -> String
promptCategoryToString pc =
    case pc of
        SafetyCulture ->
            "Safety Culture"

        AgileMindset ->
            "Agile Mindset"

        CoachingLeadership ->
            "Coaching Leadership"

        CollaborativeCulture ->
            "Collaborative Culture"

        _ ->
            ""



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
                    [ div [ A.class "banner-image d-flex justify-content-center" ]
                        [ img [ A.class "img-fluid", A.alt "Rate Your Team", src "https://assets.itpro.tv/go/RateYourTeam/mockup.jpg" ] []
                        ]
                    ]
                , div [ A.class "container" ]
                    [ div [ A.class "d-flex justify-content-center tracker" ]
                        [ Html.span [ A.class "active-tracker" ] [ Html.text "Step 1" ]
                        , Html.span [] [ Html.text "//" ]
                        , Html.span [] [ Html.text "Step 2" ]
                        , Html.span [] [ Html.text "//" ]
                        , Html.span [] [ Html.text "Step 3" ]
                        ]
                    , Html.h1 [ A.class "page-one-header" ]
                        [ Html.text "How well do you think your team is performing?"
                        ]
                    , div [ A.class "intro container text-center lead" ]
                        [ Html.text "Take this 10-question quiz, created by ITIL® master Jo Peacock, and get personalized, actionable results."
                        ]
                    , div [ A.class "pt-5 row justify-content-center" ]
                        [ div [ A.class "embed-responsive embed-responsive-16by9 video-div col-8" ] [ videoframe "https://player.vimeo.com/video/391234360?autoplay=0" ]
                        ]
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
                    [ div [ A.class "banner-image d-flex justify-content-center" ]
                        [ img [ A.class "img-fluid", src "https://assets.itpro.tv/go/RateYourTeam/mockup.jpg" ] []
                        ]
                    ]
                , div [ A.class "d-flex justify-content-center tracker" ]
                    [ Html.span [] [ Html.text "Step 1" ]
                    , Html.span [] [ Html.text "//" ]
                    , Html.span [ A.class "active-tracker" ] [ Html.text "Step 2" ]
                    , Html.span [] [ Html.text "//" ]
                    , Html.span [] [ Html.text "Step 3" ]
                    ]
                , div [ A.class "container" ]
                    [ div [ A.class "row" ]
                        [ div [ A.class "col-12 col-md-6 offset-md-3 justify-content-center unlock-results text-center" ]
                            [ Html.text "Unlock Your Results!"
                            , form
                                [ A.class "form-group justify-content-center row"
                                , A.method "post"
                                , A.action "https://go.itpro.tv/l/425902/2020-02-06/8qzydq"
                                ]
                                [ label [ A.class "sr-only", for "pardot_firstName" ] [ Html.text "First Name" ]
                                , div [ A.class "col-12" ]
                                    [ input
                                        [ onInput UpdateFormFirstName
                                        , onFocus UpdateCheckToSubmit
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
                                        , onFocus UpdateCheckToSubmit
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
                                        , onFocus UpdateCheckToSubmit
                                        , A.class "form-control flex-fill mt-4 mr-sm-2"
                                        , placeholder "Email"
                                        , A.id "pardot_email"
                                        , A.name "pardot_email"
                                        , A.type_ "email"
                                        , A.value formEmail
                                        , required True
                                        , minlength 6
                                        , maxlength 40
                                        ]
                                        []
                                    ]
                                , label [ A.class "sr-only", for "pardot_company" ] [ Html.text "Company" ]
                                , div [ A.class "col-12" ]
                                    [ input
                                        [ onInput UpdateFormCompany
                                        , onFocus UpdateCheckToSubmit
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
                                , input
                                    (disabledButton model)
                                    []
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
                    [ div [ A.class "banner-image d-flex justify-content-center" ]
                        [ img [ A.class "img-fluid", src "https://assets.itpro.tv/go/RateYourTeam/mockup.jpg" ] []
                        ]
                    ]
                , div [ A.class "d-flex justify-content-center tracker" ]
                    [ Html.span [] [ Html.text "Step 1" ]
                    , Html.span [] [ Html.text "//" ]
                    , Html.span [] [ Html.text "Step 2" ]
                    , Html.span [] [ Html.text "//" ]
                    , Html.span [ A.class "active-tracker" ] [ Html.text "Step 3" ]
                    ]
                , Html.h1 [ A.class "response-header text-center" ] [ Html.text "YOUR RESULTS" ]
                , div [ A.class "container" ]
                    [ div [ A.class "row pt-5 align-items-end no-gutters" ]
                        [ div [ A.class "col-3 offset-0 col-md-2 offset-md-2" ]
                            [ bar ((exposeResults.sc + 6) * 12) exposeResults.sc
                            ]
                        , div [ A.class "col-3 col-md-2 text-center" ]
                            [ bar ((exposeResults.am + 6) * 12) exposeResults.am
                            ]
                        , div [ A.class "col-3 col-md-2 text-center" ]
                            [ bar ((exposeResults.cl + 6) * 12) exposeResults.cl
                            ]
                        , div [ A.class "col-3 col-md-2 text-center" ]
                            [ bar ((exposeResults.cc + 6) * 12) exposeResults.cc
                            ]
                        ]
                    , div [ A.class "row align-items-start no-gutters pb-5" ]
                        [ div [ A.class "col-3 offset-0 col-md-2 offset-md-2 " ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Safety Culture" ]
                            ]
                        , div [ A.class "col-3 col-md-2 text-center " ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Agile Mindset" ]
                            ]
                        , div [ A.class "col-3 col-md-2 text-center  pl-2 pr-2" ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Coaching Leadership" ]
                            ]
                        , div [ A.class "col-3 col-md-2 text-center  pl-2 pr-2" ]
                            [ Html.h4 [ A.class "bar-label text-center" ] [ Html.text "Collaborative Culture" ]
                            ]
                        ]
                    , div [ A.class "key" ]
                        [ div [ A.class "row mb-5  align-items-start d-flex" ]
                            [ div [ A.class "col-1" ]
                                [ div [ A.class " key-square-green" ] []
                                ]
                            , div [ A.class "col-3 key-label" ] [ Html.text "Efficient AND Effective" ]
                            , div [ A.class "col-1" ]
                                [ div [ A.class "key-square-blue" ] []
                                ]
                            , div [ A.class "col-3 key-label" ] [ Html.text "Efficient OR Effective" ]
                            , div [ A.class "col-1" ]
                                [ div [ A.class "key-square-orange" ] []
                                ]
                            , div [ A.class "col-3 key-label" ] [ Html.text "Needs Improvement" ]
                            ]
                        ]
                    , div [ A.class "pb-5" ]
                        (feedbackHandler
                            [ SafetyCulture
                            , AgileMindset
                            , CoachingLeadership
                            , CollaborativeCulture
                            , CCCL
                            , SCCC
                            ]
                            model.prompts
                        )
                    , div [ A.class "d-flex justify-content-center rc-header" ] [ Html.text "Recommended Courses For Your Team" ]
                    , div [ A.class "row text-center align-items-center justify-content-center" ]
                        [ div [ A.class "col-12 col-md-6 pb-5" ]
                            [ Html.h4 [ A.class "text-center course-recommendations" ] [ Html.text (resultsToCourses exposeResults.sc [ "Management of Risk® Foundation", "ITIL®4 Direct, Plan, & Improve", "DevOps Foundation" ]) ]
                            , div [ A.class "embed-responsive embed-responsive-16by9" ] [ videoframe (resultsToOverviews exposeResults.sc [ "https://player.vimeo.com/video/349665066?autoplay=0", "https://player.vimeo.com/video/363848139?api=1&player_id=vimeoplayer", "https://player.vimeo.com/video/334731289?autoplay=0" ]) ]
                            ]
                        , div [ A.class "col-12 col-md-6 pb-5" ]
                            [ Html.h4 [ A.class "text-center course-recommendations" ] [ Html.text (resultsToCourses exposeResults.am [ "AgileSHIFT®", "Agile Foundation", "Agile Scrum Master" ]) ]
                            , div [ A.class "embed-responsive embed-responsive-16by9" ] [ videoframe (resultsToOverviews exposeResults.am [ "https://player.vimeo.com/video/305580479?autoplay=0", "https://player.vimeo.com/video/255558343?autoplay=0", "https://player.vimeo.com/video/285162807?autoplay=0" ]) ]
                            ]
                        , div [ A.class "col-12 col-md-6 pb-5" ]
                            [ Html.h4 [ A.class "text-center course-recommendations" ] [ Html.text (resultsToCourses exposeResults.cl [ "ITIL®4 Direct, Plan, & Improve", "Management of Risk® Practitioner", "Management of Risk® Foundation" ]) ]
                            , div [ A.class "embed-responsive embed-responsive-16by9" ] [ videoframe (resultsToOverviews exposeResults.cl [ "https://player.vimeo.com/video/363848139?api=1&player_id=vimeoplayer", "https://player.vimeo.com/video/374492741?autoplay=0", "https://player.vimeo.com/video/349665066?autoplay=0" ]) ]
                            ]
                        , div [ A.class "col-12 col-md-6 pb-5" ]
                            [ Html.h4 [ A.class "text-center course-recommendations" ] [ Html.text (resultsToCourses exposeResults.cc [ "DevOps Foundation", "DevOps Professional", "SIAM Foundation" ]) ]
                            , div [ A.class "embed-responsive embed-responsive-16by9" ] [ videoframe (resultsToOverviews exposeResults.cc [ "https://player.vimeo.com/video/334731289?autoplay=0", "https://player.vimeo.com/video/301870101?autoplay=0", "https://player.vimeo.com/video/261002322?autoplay=0" ]) ]
                            ]
                        ]
                    ]
                , div [ A.class "CTA-section" ]
                    [ div [ A.class "row align-items-center" ]
                        [ div [ A.class "col-12 text-center" ]
                            [ Html.text "Ready to upskill your team? Schedule a demo or buy now."
                            ]
                        ]
                    , div [ A.class "row" ]
                        [ div [ A.class "col-12 d-flex justify-content-center" ]
                            [ Html.a
                                [ A.class "text-center btn btn-lg button-CTA", A.href "https://www.itpro.tv/", A.target "_blank", A.rel "noopener noreferrer" ]
                                [ Html.text "View Business Plans" ]
                            ]
                        ]
                    ]
                ]

        ErrorState ->
            div [] [ Html.text "If you're seeing this, something has gone horribly wrong. Please try clearing your cache and refreshing."]


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


disabledButton : Model -> List (Html.Attribute Msg)
disabledButton model =
    let
        buttonclass =
            if model.allowSubmit then
                "button-submit"

            else
                "button-disabled"
    in
    [ A.class (buttonclass ++ " btn btn-lg")
    , A.type_ "submit"
    , A.value "Submit"
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
