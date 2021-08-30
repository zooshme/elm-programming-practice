port module WebSocket exposing (..)


port listen : String -> Cmd msg


port receive : (String -> msg) -> Sub msg
