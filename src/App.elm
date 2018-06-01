module App exposing (Application, Config, application, button, link)

{-|


# Create an app

@docs Config, Application, application


# Link helpers

@docs link, button

-}

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Navigation exposing (Location)
import UrlParser as Parser exposing (Parser)


-- APPLICATION


{-| -}
type alias Application app route page msg =
    Platform.Program Never (Model route app page) (Msg route msg)


{-| The configuration. In this world, your model will
contain data which is shared across all pages, as well as past
model of each page if you wish to keep them. These past models
can then be used to reinitialize your page when re-visiting it.

  - init: The initial state of all your data and pages.
  - parser: The parser for your routes.
  - composer: Turn your route into a URL pathname again.
  - load: Get the past model of your page, if any, and initialize a page.
  - save: Save the model once you're leaving the page.
  - update: Update your current page.
  - view: View your current page.

-}
type alias Config app route page msg =
    { init : ( app, Cmd msg )
    , parser : Parser (route -> route) route
    , composer : route -> String
    , load : Result Location route -> app -> ( page, Cmd msg )
    , save : page -> app -> app
    , update : msg -> page -> ( page, Cmd msg )
    , view : page -> Html msg
    , subscriptions : page -> Sub msg
    }


{-| Create a single page app.
-}
application : Config app route page msg -> Application app route page msg
application config =
    Navigation.program ReceiveLocation
        { init = init config config.init
        , view = view config
        , update = update config
        , subscriptions = subscriptions config
        }



-- APPLICATION / INTERNAL / MODEL


type alias Model route app page =
    { route : Maybe route
    , app : app
    , page : page
    }



-- APPLICATION / INTERNAL / INIT


init : Config app route page msg -> ( app, Cmd msg ) -> Location -> ( Model route app page, Cmd (Msg route msg) )
init config ( app, appCmd ) location =
    reinit config app (Parser.parsePath config.parser location) location
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ Cmd.map PageMsg appCmd, cmd ])


reinit : Config app route page msg -> app -> Maybe route -> Location -> ( Model route app page, Cmd (Msg route msg) )
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
        |> Tuple.mapFirst (Model route app)
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ Cmd.map PageMsg cmd, redirectCmd ])



-- APPLICATION / INTERNAL / UPDATE


type Msg route msg
    = ReceiveLocation Location
    | PageMsg msg


update : Config app route page msg -> Msg route msg -> Model route app page -> ( Model route app page, Cmd (Msg route msg) )
update config msg model =
    case msg of
        ReceiveLocation location ->
            let
                route =
                    Parser.parsePath config.parser location
            in
            if route == model.route then
                ( model, Cmd.none )
            else
                reinit config (config.save model.page model.app) route location

        PageMsg msg ->
            config.update msg model.page
                |> Tuple.mapFirst (Model model.route model.app)
                |> Tuple.mapSecond (Cmd.map PageMsg)



-- APPLICATION / INTERNAL / VIEW


view : Config app route page msg -> Model route app page -> Html.Html (Msg route msg)
view config model =
    config.view model.page
        |> Html.map PageMsg



-- APPLICATION / INTERNAL / SUBS


subscriptions : Config app route page msg -> Model route app page -> Sub (Msg route msg)
subscriptions config model =
    config.subscriptions model.page
        |> Sub.map PageMsg



-- APPLICATION / LINKS


{-| A link which prevents default.
-}
link : (route -> String) -> (route -> msg) -> route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
link toString toMsg nextRoute attributes =
    Html.a (linkAttributes toString toMsg nextRoute attributes)


{-| A button link which prevents default.
-}
button : (route -> String) -> (route -> msg) -> route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
button toString toMsg nextRoute attributes =
    Html.button (linkAttributes toString toMsg nextRoute attributes)


linkAttributes : (route -> String) -> (route -> msg) -> route -> List (Html.Attribute msg) -> List (Html.Attribute msg)
linkAttributes toString onSuccess nextRoute attributes =
    attributes
        ++ [ onClick (onSuccess nextRoute)
           , Html.Attributes.target "_blank"
           , Html.Attributes.href (toString nextRoute)
           ]


onClick : msg -> Html.Attribute msg
onClick successMsg =
    let
        -- TODO what is this all about
        isCtrlOrMeta =
            Json.map2 (\isCtrl isMeta -> isCtrl || isMeta)
                (Json.field "ctrlKey" Json.bool)
                (Json.field "metaKey" Json.bool)

        finish preventDefault =
            if preventDefault then
                Json.fail "Invalid click"
            else
                Json.succeed successMsg

        decoder =
            Json.andThen finish isCtrlOrMeta
    in
    Html.Events.onWithOptions "click" { stopPropagation = False, preventDefault = True } decoder
