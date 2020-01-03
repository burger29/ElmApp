module Data exposing (questionList, videoframe)

import Array as Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode
import Types exposing (Feedback(..), PromptCategory(..), Question(..), Msg(..))


questionList : List Question
questionList =
    [ Question "My team can clearly articulate their goals"
        1
        AgileMindset
        (feedbackListFromStrings
            [ "Either you are not clear on the goals for your team, or you are struggling to articulate or communicate those goals."
            , "Your team have goals, but they are not clear enough for them to achieve. Looks for ways to improve your communication approach."
            , "Your communication hits the mark and you encourage collaboration!"
            ]
        )
    , Question "My team feels recognized for their accomplishments"
        1
        CoachingLeadership
        (feedbackListFromStrings
            [ "Your team may not be able to deliver expected results and may feel oppressed and invisible."
            , "Some accomplishments are recognised and rewarded, but these needs to be a consistent approach to motivate your team."
            , "You have a motivated team which leads to high morale. Think about how you can maintain that level."
            ]
        )
    , Question "All team members have personal development plans and see regular progress towards their goals"
        1
        CoachingLeadership
        (feedbackListFromStrings
            [ "Team members do not feel valued. Their skills are not being developed which will have a detrimental effect on team performance and their ability to meet expectations."
            , "Development is ADHOC and may be limited to certain individuals or specific topics. Consider how personal development contributes to team achievements."
            , "Team members feel valued, and enhancing their skills ensures your team’s bandwidth is continually increasing. Ensure you are prepared for team members to move on in their career and plan for replacements."
            ]
        )
    , Question "My team feels empowered to make decisions"
        1
        SafetyCulture
        (feedbackListFromStrings
            [ "Decisions are most effective and efficient when made at the point with the most information. Time is wasted on escalating decisions that are able to be made within the team."
            , "The team is comfortable with making some decisions, but a lot are unnecessarily escalated. Consider delegating decisions and showing team members that they are entrusted with these."
            , "Team members feel trusted and decisions are made at the point where the most knowledge exists."
            ]
        )
    , Question "My team is more efficient when I’m not there"
        -1
        CCCL
        (feedbackListFromStrings
            [ "Time is wasted escalating concerns and decisions to you that should be managed within the team. Beware of micro-management."
            , "The team is comfortable with allocating some of their own workload, but still look to you to confirm decisions and add validate direction."
            , "You allow your team the flexibility to collaborate and make appropriate decisions, and rely on expectations being met."
            ]
        )
    , Question "My team has productive meetings that everyone is involved in (but only when necessary)"
        1
        CollaborativeCulture
        (feedbackListFromStrings
            [ "Time is wasted in unnecessary meetings, and meetings without purpose or agenda. Be careful to engage in efficient meeting etiquette."
            , "You have some unnecessary and unproductive meetings, but your team are learning to embrace new ways of collaboration."
            , "You have an environment that doesn’t encourage meetings just for the sake of having a meeting. Collaboration is key skill of your team."
            ]
        )
    , Question "Team members will openly express their opinions and concerns"
        1
        SCCC
        (feedbackListFromStrings
            [ "Your team may not feel as though their contributions are valued may feel oppressed and invisible."
            , "Your team is content within their own environment, but this will need your attention to ensure that satisfaction levels do not drop."
            , "Team members are confident in your ability to listen and to accept feedback."
            ]
        )
    , Question "Other people want to be on our team"
        1
        SafetyCulture
        (feedbackListFromStrings
            [ "People don’t see your team as an aspiration or a career progression."
            , "Your team culture is productive, but is not seen as dynamic and a great place to work. Consider and approach to team visibility."
            , "Your team feels nurtured and encouraged, whilst having a clear direction. Having the right team culture is important to you and others recognise that."
            ]
        )
    , Question "My team has created their own set of operating guidelines and practices which they are fully bought into"
        1
        AgileMindset
        (feedbackListFromStrings
            [ "Your team is in need of processes and controls that they are able and willing to follow."
            , "You have processes established within the team, but are still evolving their maturity; a consistency of usage; and a level of flexibility."
            , "There is a fine line between having an open and relaxed culture, and having a lack of control. You seem to have found the perfect balance and also understand the emotional impact of too many dictated and rigid controls."
            ]
        )
    , Question "All team members hold each other, including me, accountable for outcomes"
        1
        AgileMindset
        (feedbackListFromStrings
            [ "Teams members are more comfortable with rigid rules and guidelines, than are empowered to hold themselves accountable."
            , "Team members have explored making some decisions themselves, but are more comfortable with allowing you to make decisions. Explore ways of enforcing decision making control further down the chain of command."
            , "You are a true servant leader. Guiding the team from within, so that you are perceived as a truly valuable team member and not a manager."
            ]
        )
    ]


videoframe : String -> Html Msg
videoframe url =
    iframe
        [ width 560
        , height 315
        , src (url)
        , property "allowfullscreen" (Json.Encode.string "true")
        , class "embed-responsive-item"
        ]
        []


feedbackListFromStrings : List String -> List Feedback
feedbackListFromStrings strings =
    List.map (\s -> Feedback s) strings
