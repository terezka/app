module Main exposing (main)

import Navigation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Profile as Profile
import Route
import Spa
import UrlParser as P exposing ((</>))


-- TODO: redirects


main : Spa.Program App
main =
    Spa.program
        { init = init
        , pages =
            [ Spa.route Profile.page <|
                List.map (P.map Profile.Args)
                    [ P.s "profile" </> P.int </> P.map Route.Progress P.top
                    , P.s "profile" </> P.int </> P.map Route.Progress (P.s "progress")
                    , P.s "profile" </> P.int </> P.map Route.Submissions (P.s "submissions")
                    ]
            , Spa.route Home.page <|
                List.map (P.map ())
                    [ P.s "home"
                    , P.top
                    ]
            ]
        , default =
            Spa.default NotFound.page
        }



-- APP


type alias App =
    { home : Maybe Home.Model
    , profile : Maybe Profile.Model
    , notFound : Maybe NotFound.Model
    }


init : App
init =
    { home = Nothing
    , profile = Nothing
    , notFound = Nothing
    }
