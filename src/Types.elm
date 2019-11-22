module Types
    exposing
        ( Feedback(..)
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


type alias Model =
    { prompts : List Prompt
    , state : ModelState
    , results : Results
    , formData : FormData
    , allowSubmit : Bool
    }


type alias Prompt =
    { question : String
    , variable : Int
    , responseOptions : List Response
    , selectedResponse : Maybe Response
    , index : Int
    , promptCategory : PromptCategory
    , feedbackList : List Feedback
    }


type alias Results =
    { sc : Int
    , am : Int
    , cl : Int
    , cc : Int
    }


type Response
    = Agree
    | StronglyAgree
    | Neutral
    | Disagree
    | StronglyDisagree


type Msg
    = SelectResponse Response Prompt
    | ChangeModelState
    | UpdateFormFirstName String
    | UpdateFormLastName String
    | UpdateFormEmail String
    | UpdateFormCompany String


type PromptCategory
    = SafetyCulture
    | AgileMindset
    | CoachingLeadership
    | CollaborativeCulture
    | CCCL
    | SCCC


type Question
    = Question String Int PromptCategory (List Feedback)


type ModelState
    = AnsweringQuestions Prompt
    | FillingOutForm
    | ShowingResults


type Feedback
    = Feedback String


unwrapFeedback : Feedback -> String
unwrapFeedback (Feedback string) =
    string


type alias FormData =
    { formFirstName : String
    , formLastName : String
    , formEmail : String
    , formCompany : String
    }
