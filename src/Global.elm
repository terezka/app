module Global exposing (Global, init)

import User


{-| -}
type alias Global =
    { user : User.User }


{-| -}
init : Global
init =
    { user = () }
