port module Ports exposing (saveResponses)
import Json.Encode as Encode


port storeResponses : String -> Cmd msg

saveResponses : List Int -> Cmd msg
saveResponses responses =
    storeResponses
      <| Encode.encode 0
      <| Encode.list Encode.int responses
