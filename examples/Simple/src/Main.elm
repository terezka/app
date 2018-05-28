module Main exposing (main)

import Dict
import Navigation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Profile as Profile
import Route
import Spa
import UrlParser as P exposing ((</>))


-- TODO: redirects


main : Spa.Application App
main =
    Spa.application init
        [ Spa.route Profile.page profile
        , Spa.route Home.page home
        ]
        NotFound.page


profile : P.Parser (Profile.Args -> a) a
profile =
    P.map Profile.Args <|
        P.oneOf
            [ P.s "profile" </> P.int </> P.map Route.Progress P.top
            , P.s "profile" </> P.int </> P.map Route.Progress (P.s "progress")
            , P.s "profile" </> P.int </> P.map Route.Submissions (P.s "submissions")
            ]


home : P.Parser (() -> a) a
home =
    P.map () <|
        P.oneOf
            [ P.s "home"
            , P.top
            ]



-- APP


type alias App =
    { home : Maybe Home.Model
    , profiles : Dict.Dict Int Profile.Model
    , notFound : Maybe NotFound.Model
    }


init : App
init =
    { home = Nothing
    , profiles = Dict.empty
    , notFound = Nothing
    }
