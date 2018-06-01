module Main exposing (main)

import Dict
import Flags.App as App
import Html
import Json.Decode as Json
import Navigation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Profile as Profile
import Route


main : App.Application App Route.Route Page Msg
main =
    App.application
        { init = init
        , decoder = decoder
        , broken = \err -> ( Broken err, Cmd.none )
        , composer = Route.composer
        , parser = Route.parser
        , load = load
        , save = save
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type alias App =
    { hasFlags : Bool
    , home : Maybe Home.Model
    , profiles : Dict.Dict Int Profile.Model
    , notFound : Maybe NotFound.Model
    }


type Page
    = Profile Profile.Model
    | Home Home.Model
    | NotFound NotFound.Model
    | Broken String


type alias Flags =
    { hasFlags : Bool }


decoder : Json.Decoder Flags
decoder =
    Json.field "has_flags" Json.bool
        |> Json.map Flags


init : Flags -> App
init flags =
    { hasFlags = flags.hasFlags
    , home = Nothing
    , profiles = Dict.empty
    , notFound = Nothing
    }


load : Result Navigation.Location Route.Route -> App -> ( Page, Cmd Msg )
load route app =
    case route of
        Ok (Route.Profile id tab) ->
            Profile.init (Dict.get id app.profiles) id tab
                |> Tuple.mapFirst Profile
                |> Tuple.mapSecond (Cmd.map ProfileMsg)

        Ok Route.Home ->
            Home.init app.home
                |> Tuple.mapFirst Home
                |> Tuple.mapSecond (Cmd.map HomeMsg)

        Err location ->
            NotFound.init app.notFound location
                |> Tuple.mapFirst NotFound
                |> Tuple.mapSecond (Cmd.map NotFoundMsg)


save : Page -> App -> App
save page app =
    case page of
        Profile model ->
            { app | profiles = Dict.insert model.id model app.profiles }

        Home model ->
            { app | home = Just model }

        NotFound model ->
            { app | notFound = Just model }

        Broken _ ->
            app


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


view : Page -> Html.Html Msg
view page =
    case page of
        Profile model ->
            Html.map ProfileMsg (Profile.view model)

        Home model ->
            Html.map HomeMsg (Home.view model)

        NotFound model ->
            Html.map NotFoundMsg (NotFound.view model)

        Broken err ->
            Html.text err
