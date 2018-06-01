module Route exposing (Route(..), Tab(..), button, composer, link, navigate, parser)

{-|

@docs Route, Tab, parser, composer
@docs button, link
@docs navigate

-}

import App
import Html
import Navigation
import UrlParser as P exposing ((</>))


{-| -}
type Route
    = Home
    | Profile Int Tab


{-| -}
type Tab
    = Progress
    | Submissions


{-| -}
parser : P.Parser (Route -> Route) Route
parser =
    P.oneOf
        [ P.map Profile (P.s "profile" </> P.int </> P.map Progress P.top)
        , P.map Profile (P.s "profile" </> P.int </> P.map Progress (P.s "progress"))
        , P.map Profile (P.s "profile" </> P.int </> P.map Submissions (P.s "submissions"))
        , P.map Home (P.s "home")
        , P.map Home P.top
        ]


{-| -}
link : (Route -> msg) -> Route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
link =
    App.link composer


{-| -}
button : (Route -> msg) -> Route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
button =
    App.button composer


{-| -}
composer : Route -> String
composer route =
    case route of
        Home ->
            "/"

        Profile id tab ->
            "/profile/" ++ toString id ++ "/" ++ String.toLower (toString tab)


{-| -}
navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (composer route)
