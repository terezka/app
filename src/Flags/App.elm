module Flags.App exposing (Application, Config, application)

{-|


# Create an app with flags

@docs Config, Application, application

-}

import Html exposing (Html)
import Json.Decode as Json
import Navigation exposing (Location)
import UrlParser as Parser exposing (Parser)


-- APPLICATION


{-| -}
type alias Application app route page msg =
    Platform.Program Json.Value (Model route app page) (Msg route msg)


{-| Almost identitcal to `App.Config`, except for some important differences:

    - `init` takes flags.
    - `decoder` decodes your flags
    - `broken` gives you the decoding error and returns a page.

Note: Want to handle decoding errors your own way? Just set `decoder`
to `Json.succeed` and have your `init` take a `Json.Value` which you can
then decode for real however you like.

-}
type alias Config flags app route page msg =
    { init : flags -> app
    , decoder : Json.Decoder flags
    , broken : String -> ( page, Cmd msg )
    , parser : Parser (route -> route) route
    , composer : route -> String
    , load : Result Location route -> app -> ( page, Cmd msg )
    , save : page -> app -> app
    , update : msg -> page -> ( page, Cmd msg )
    , view : page -> Html msg
    , subscriptions : page -> Sub msg
    }


{-| Create a single page app with flags
-}
application : Config flags app route page msg -> Application app route page msg
application config =
    Navigation.programWithFlags ReceiveLocation
        { init = init config
        , view = view config
        , update = update config
        , subscriptions = subscriptions config
        }



-- APPLICATION / INTERNAL / MODEL


type alias Model route app page =
    { route : Maybe route
    , app : Result String app
    , page : page
    }



-- APPLICATION / INTERNAL / INIT


init : Config flags app route page msg -> Json.Value -> Location -> ( Model route app page, Cmd (Msg route msg) )
init config json location =
    case Json.decodeValue config.decoder json of
        Ok flags ->
            reinit config (config.init flags) (Parser.parsePath config.parser location) location

        Err err ->
            config.broken err
                |> Tuple.mapFirst (Model Nothing (Err err))
                |> Tuple.mapSecond (Cmd.map PageMsg)


reinit : Config flags app route page msg -> app -> Maybe route -> Location -> ( Model route app page, Cmd (Msg route msg) )
reinit config app route location =
    let
        redirectCmd =
            route
                |> Maybe.map checkForRedirect
                |> Maybe.withDefault Cmd.none

        checkForRedirect route_ =
            let
                rewritten =
                    config.composer route_
            in
            if rewritten == location.pathname then
                Cmd.none
            else
                Navigation.modifyUrl rewritten

        routeResult =
            route
                |> Maybe.map Ok
                |> Maybe.withDefault (Err location)
    in
    config.load routeResult app
        |> Tuple.mapFirst (Model route (Ok app))
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ Cmd.map PageMsg cmd, redirectCmd ])



-- APPLICATION / INTERNAL / UPDATE


type Msg route msg
    = ReceiveLocation Location
    | PageMsg msg


update : Config flags app route page msg -> Msg route msg -> Model route app page -> ( Model route app page, Cmd (Msg route msg) )
update config msg model =
    case msg of
        ReceiveLocation location ->
            let
                route =
                    Parser.parsePath config.parser location
            in
            case model.app of
                Err err ->
                    ( model, Cmd.none )

                Ok app ->
                    if route == model.route then
                        ( model, Cmd.none )
                    else
                        reinit config (config.save model.page app) route location

        PageMsg msg ->
            config.update msg model.page
                |> Tuple.mapFirst (Model model.route model.app)
                |> Tuple.mapSecond (Cmd.map PageMsg)



-- APPLICATION / INTERNAL / VIEW


view : Config flags app route page msg -> Model route app page -> Html.Html (Msg route msg)
view config model =
    config.view model.page
        |> Html.map PageMsg



-- APPLICATION / INTERNAL / SUBS


subscriptions : Config flags app route page msg -> Model route app page -> Sub (Msg route msg)
subscriptions config model =
    config.subscriptions model.page
        |> Sub.map PageMsg
