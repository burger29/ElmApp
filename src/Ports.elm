port module Ports exposing (saveResponses, saveResults)

import Json.Encode as Encode
import Types exposing (Results)


port storeResponses : String -> Cmd msg


port storeResults : String -> Cmd msg


saveResponses : List Int -> Cmd msg
saveResponses responses =
    storeResponses <|
        Encode.encode 0 <|
            Encode.list Encode.int responses


saveResults : Results -> Cmd msg
saveResults results =
    storeResults <|
        Encode.encode 0 <|
            encodeResults results


encodeResults : Results -> Encode.Value
encodeResults results =
    Encode.object
        [ ( "sc", Encode.int results.sc )
        , ( "am", Encode.int results.am )
        , ( "cl", Encode.int results.cl )
        , ( "cc", Encode.int results.cc )
        ]
