# Application Program

This is a simple frame for creating a single page app in Elm.

## Installation

```bash
elm package install terezka/app
```


## Example main

Look in `/examples` for elaboration.

```elm

import App
import Dict
import Html
import Navigation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Profile as Profile
import Route


main : App.Application App Page Msg
main =
    App.application
        { init = init
        , parser = parser
        , load = load
        , save = save
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- APP MODEL


type alias App =
    { home : Maybe Home.Model
    , profiles : Dict.Dict Int Profile.Model
    , notFound : Maybe NotFound.Model
    }



-- APP PAGE


type Page
    = Profile Profile.Model
    | Home Home.Model
    | NotFound NotFound.Model



-- APP INIT


init : App
init =
    { home = Nothing
    , profiles = Dict.empty
    , notFound = Nothing
    }



-- ROUTE PARSER


parser : P.Parser (Route.Route -> Route.Route) Route.Route
parser =
    P.oneOf
        [ P.map Route.Profile (P.s "profile" </> P.int </> P.map Route.Progress P.top)
        , P.map Route.Profile (P.s "profile" </> P.int </> P.map Route.Progress (P.s "progress"))
        , P.map Route.Profile (P.s "profile" </> P.int </> P.map Route.Submissions (P.s "submissions"))
        , P.map Route.Home (P.s "home")
        , P.map Route.Home P.top
        ]



-- LOAD PAGE


load : Maybe Route.Route -> App -> ( Page, Cmd Msg )
load route app =
    case route of
        Just (Route.Profile id tab) ->
            Profile.init (Dict.get id app.profiles) id tab
                |> Tuple.mapFirst Profile
                |> Tuple.mapSecond (Cmd.map ProfileMsg)

        Just Route.Home ->
            Home.init app.home
                |> Tuple.mapFirst Home
                |> Tuple.mapSecond (Cmd.map HomeMsg)

        Nothing ->
            NotFound.init app.notFound
                |> Tuple.mapFirst NotFound
                |> Tuple.mapSecond (Cmd.map NotFoundMsg)



-- SAVE PAGE


save : Page -> App -> App
save page app =
    case page of
        Profile model ->
            { app | profiles = Dict.insert model.id model app.profiles }

        Home model ->
            { app | home = Just model }

        NotFound model ->
            { app | notFound = Just model }



-- PAGE UPDATE


type Msg
    = ProfileMsg Profile.Msg
    | HomeMsg Home.Msg
    | NotFoundMsg NotFound.Msg


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case ( msg, page ) of
        ( ProfileMsg msg, Profile model ) ->
            Profile.update msg model
                |> Tuple.mapFirst Profile
                |> Tuple.mapSecond (Cmd.map ProfileMsg)

        ( HomeMsg msg, Home model ) ->
            Home.update msg model
                |> Tuple.mapFirst Home
                |> Tuple.mapSecond (Cmd.map HomeMsg)

        ( NotFoundMsg msg, NotFound model ) ->
            NotFound.update msg model
                |> Tuple.mapFirst NotFound
                |> Tuple.mapSecond (Cmd.map NotFoundMsg)

        ( _, _ ) ->
            ( page, Cmd.none )



-- PAGE VIEW


view : Page -> Html.Html Msg
view page =
    case page of
        Profile model ->
            Html.map ProfileMsg (Profile.view model)

        Home model ->
            Html.map HomeMsg (Home.view model)

        NotFound model ->
            Html.map NotFoundMsg (NotFound.view model)
```
