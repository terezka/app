module Route exposing (Route(..), Tab(..), button, link, navigate, toUrl)

import Html
import Navigation
import Spa


{-| -}
type Route
    = Home
    | Profile Int Tab


{-| -}
type Tab
    = Progress
    | Submissions


{-| -}
link : (Route -> msg) -> Route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
link =
    Spa.link toUrl


{-| -}
button : (Route -> msg) -> Route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
button =
    Spa.button toUrl


{-| -}
toUrl : Route -> String
toUrl route =
    case route of
        Home ->
            "/home"

        Profile id tab ->
            "/profile/" ++ toString id ++ "/" ++ String.toLower (toString tab)


{-| -}
navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (toUrl route)
