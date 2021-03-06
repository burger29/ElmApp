module Types exposing
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

import Json.Decode as Decode
import Url as Url 


type alias Model =
    { prompts : List Prompt
    , state : ModelState
    , results : Results
    , formData : FormData
    , allowSubmit : Bool
    , maybePageUrl : Maybe Url.Url
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
    | UpdateCheckToSubmit


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
    | ErrorState


type Feedback
    = Feedback String PromptCategory


unwrapFeedback : Feedback -> String
unwrapFeedback (Feedback string _) =
    string


type alias FormData =
    { formFirstName : String
    , formLastName : String
    , formEmail : String
    , formCompany : String
    }


type alias Flags =
    { startingState : Maybe (List Int)
    , startingResults : Decode.Value
    , pageUrl : String
    }
