module Data.User exposing (User, request, setColor)

{-|

@docs User, request, setColor

-}

import Http
import Json.Decode


{-| -}
type alias User =
  { id : Int
  , name : String
  , username : String
  , settings : Settings
  }


{-| -}
type alias Settings =
  { color : String
  }


{-| -}
request : Int -> Http.Request User
request id =
  Http.get ("https://jsonplaceholder.typicode.com/users/" ++ toString id) decoder


{-| -}
setColor : String -> User -> User
setColor color ({ settings } as user) =
  { user | settings = { settings | color = color } }



-- INTERNAL


decoder : Json.Decode.Decoder User
decoder =
  Json.Decode.map3 init
    (Json.Decode.field "id" Json.Decode.int)
    (Json.Decode.field "name" Json.Decode.string)
    (Json.Decode.field "username" Json.Decode.string)


init : Int -> String -> String -> User
init id name username =
  { id = id
  , name = name
  , username = username
  , settings = Settings "lightpink"
  }
