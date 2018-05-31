module Data.Post exposing (Post, request)

{-|

@docs Post, request

-}

import Http
import Json.Decode


{-| -}
type alias Post =
    { id : Int
    , title : String
    , body : String
    }


{-| -}
request : Http.Request (List Post)
request =
    Http.get "https://jsonplaceholder.typicode.com/posts" (Json.Decode.list decoder)


decoder : Json.Decode.Decoder Post
decoder =
    Json.Decode.map3 Post
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "body" Json.Decode.string)
