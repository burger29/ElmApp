module Data exposing (questionList, videoframe, resultsParagraphList)

import Types exposing (PromptCategory(..), Question(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode
import Array as Array

questionList : List Question
questionList =
  [ Question "My team can clearly articulate their goals" 1 AgileMindset
    ,
    Question "My team feels recognized for their accomplishments" 1 CoachingLeadership
    ,
    Question "All team members have personal development plans and see regular progress towards their goals" 1 CoachingLeadership
    ,
    Question "My team feels empowered to make decisions" 1 SafetyCulture
    ,
    Question "My team is more efficient when I’m not there" -1 CCCL
    ,
    Question "My team has productive meetings that everyone is involved in (but only when necessary)" 1 CollaborativeCulture
    ,
    Question "Team members will openly express their opinions and concerns" 1 SCCC
    ,
    Question "Other people want to be on our team" 1 SafetyCulture
    ,
    Question "My team has created their own set of operating guidelines and practices which they are fully bought into" 1 AgileMindset
    ,
    Question "All team members hold each other, including me, accountable for outcomes" 1 AgileMindset
  ]

videoframe =
  iframe
  [ width 560
  , height 315
  , src "https://www.youtube.com/embed/YihH5Gs1V9Q"
  , property "allowfullscreen" (Json.Encode.string "true")
  , class "embed-responsive-item"
  ]
  []

resultsParagraphList : List String
resultsParagraphList =
  [ "String1"
  , "String2"
  , "String3"
  , "String4"
  , "String5"
  , "String6"
  , "String7"
  , "String8"
  ]
