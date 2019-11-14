module Types exposing (PromptCategory(..), Question(..), Msg (..), Response(..), Prompt, Model, ModelState(..), Results)


type alias Model =
  { prompts: List Prompt
  , state: ModelState
  , results: Results
  }

type alias Prompt =
  { question: String
  , variable: Int
  , responseOptions: List Response
  , selectedResponse: Maybe Response
  , index: Int
  , promptCategory: PromptCategory
  }

type alias Results =
  { sc: Int
  , am: Int
  , cl: Int
  , cc: Int
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

type PromptCategory
    = SafetyCulture
    | AgileMindset
    | CoachingLeadership
    | CollaborativeCulture
    | CCCL
    | SCCC

type Question
    = Question String Int PromptCategory

type ModelState
  = AnsweringQuestions Prompt
  | FillingOutForm
  | ShowingResults
