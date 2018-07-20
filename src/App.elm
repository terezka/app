module App exposing (Application, Page, Route, application, button, link, page, pageWithFlags, redirect, route)

{-|


### List of content

  - Definitions
  - Create an [application](#Application)
  - Create a [route](#Route)
  - Create a [page](#Page)
  - Link [helpers](#link)

**Please read the definitions! The rest of this doc will assume that you did.**


# Definitions

  - Application: An application is a "single page app", where single actually
    means multiple. It's confusing, so now it's all just called an application.
  - Session: A session is a structure of data which is shared amongst your pages.
  - Route: A route is a URL rendering to a particular page.
  - Page: A page is full elm life cycle, meaning the bundeling of
    a model, view, update, subscriptions, and if you have flags, a decoder,
    as well as a few functions to describe how the page fits in with the
    rest of your application. See [Page](#Page) for more info.


# Application

@docs Application, application


## Route

@docs Route, route, redirect


## Page

@docs Page, page, pageWithFlags


### Link helpers

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
type alias Application session app =
    Platform.Program Json.Value (Model session app) (Msg session app)


{-| Create an application.

Arguments:

  - The initial state of your session
  - The initial state of your app
  - Your [routes](#Route)
  - Your fallback page if no route matches the URL

-}
application : session -> app -> List (Route session app) -> Page Location session app -> Application session app
application session init routes fallback =
    Navigation.programWithFlags ReceiveLocation
        { init = appInit fallback routes session init
        , view = appView
        , update = appUpdate fallback routes
        , subscriptions = appSubscriptions
        }



-- PAGE


{-| -}
type Page args session app
    = Page (Json.Value -> args -> AnonymousPage session app)


{-| A page referes to an Elm lifecycle which is run when the user
is on one or more routes.

Arguments

  - init: This initializes your page. It is run everytime you land on
    a URL which referes to this page. It gets the session data, the
    previous state of the page, if any, and the arguments extracted
    from the url.
  - update: The standard* update function.
  - view: The standard* view function.
  - subscriptions: The standard* subscriptions function.
  - session: Update your session for each update on this page.
  - save: Store your state in your main app structure.
  - load: Fetch your state from your main app structure.

*Check the official guide if you don't know what this means!

-}
page :
    { init : session -> Maybe model -> args -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html.Html msg
    , subscriptions : model -> Sub msg
    , session : model -> session -> session
    , save : session -> args -> model -> app -> app
    , load : session -> args -> app -> Maybe model
    }
    -> Page args session app
page config =
    Page <|
        \_ args -> toAnonymousPage config args


{-| Just like `page`, but with a few extra options:

  - decoder: The decoder of your flags.
  - init: The init also receives the flags.
  - fallback: The page loaded when the flags decoder fails!

-}
pageWithFlags :
    { decoder : Json.Decoder flags
    , init : flags -> session -> Maybe model -> args -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html.Html msg
    , subscriptions : model -> Sub msg
    , session : model -> session -> session
    , save : session -> args -> model -> app -> app
    , load : session -> args -> app -> Maybe model
    , fallback : Page String session app
    }
    -> Page args session app
pageWithFlags config =
    Page <|
        \json args ->
            case Json.decodeValue config.decoder json of
                Ok flags ->
                    toAnonymousPage
                        { init = config.init flags
                        , update = config.update
                        , view = config.view
                        , subscriptions = config.subscriptions
                        , session = config.session
                        , save = config.save
                        , load = config.load
                        }
                        args

                Err err ->
                    let
                        (Page fallback) =
                            config.fallback
                    in
                    fallback json err



-- ROUTE


{-| -}
type Route session app
    = Route (Json.Value -> Location -> Maybe (AnonymousPage session app))
    | Redirect (Location -> Maybe String)


{-| Make a route. Pass a page and a parser.
-}
route : Page args session app -> Parser (args -> args) args -> Route session app
route (Page toPage) parser =
    Route <|
        \json url ->
            case Parser.parsePath parser url of
                Just args ->
                    Just (toPage json args)

                Nothing ->
                    Nothing


{-| Redirect to another URL. Pass a URL maker and the parser for the
urls which you want to be redirected from.
-}
redirect : (args -> String) -> Parser (args -> args) args -> Route session app
redirect toUrl parser =
    Redirect <|
        \url ->
            case Parser.parsePath parser url of
                Just args ->
                    Just (toUrl args)

                Nothing ->
                    Nothing



-- APPLICATION / INTERNAL / MODEL


type alias Model session app =
    { flags : Json.Value
    , session : session
    , page : AnonymousPage session app
    , app : app
    }



-- APPLICATION / INTERNAL / INIT


appInit : Page Location session app -> List (Route session app) -> session -> app -> Json.Value -> Location -> ( Model session app, Cmd (Msg session app) )
appInit fallback routes session app json location =
    case handleRoute fallback routes json location of
        Ok page_ ->
            Tuple.mapFirst (Model json session page_) (page_.init session app)

        Err url ->
            ( Model json session blank_ app
            , Maybe.map Navigation.modifyUrl url
                |> Maybe.withDefault Cmd.none
            )



-- APPLICATION / INTERNAL / UPDATE


type Msg session app
    = ReceiveLocation Location
    | PageMsg (Model session app -> ( Model session app, Cmd (Msg session app) ))


appUpdate : Page Location session app -> List (Route session app) -> Msg session app -> Model session app -> ( Model session app, Cmd (Msg session app) )
appUpdate fallback routes msg model =
    case msg of
        ReceiveLocation location ->
            appInit fallback routes model.session model.app model.flags location

        PageMsg updatePage ->
            updatePage model



-- APPLICATION / INTERNAL / VIEW


appView : Model session app -> Html.Html (Msg session app)
appView model =
    model.page.view model.session model.app



-- APPLICATION / INTERNAL / SUBS


appSubscriptions : Model session app -> Sub (Msg session app)
appSubscriptions model =
    model.page.subscriptions model.session model.app



-- APPLICATION / INTERNAL / PAGE


type alias AnonymousPage session app =
    { init : session -> app -> ( app, Cmd (Msg session app) )
    , view : session -> app -> Html.Html (Msg session app)
    , subscriptions : session -> app -> Sub (Msg session app)
    }


toAnonymousPage :
    { init : session -> Maybe model -> args -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html.Html msg
    , subscriptions : model -> Sub msg
    , session : model -> session -> session
    , save : session -> args -> model -> app -> app
    , load : session -> args -> app -> Maybe model
    }
    -> args
    -> AnonymousPage session app
toAnonymousPage config args =
    let
        init_ : session -> app -> ( app, Cmd (Msg session app) )
        init_ session app =
            config.init session (config.load session args app) args
                |> Tuple.mapFirst (\model -> config.save session args model app)
                |> Tuple.mapSecond (Cmd.map anonymizeMsg)

        view_ : session -> app -> Html.Html (Msg session app)
        view_ session app =
            case config.load session args app of
                Just model ->
                    Html.map anonymizeMsg (config.view model)

                Nothing ->
                    Html.text ""

        subscriptions_ : session -> app -> Sub (Msg session app)
        subscriptions_ session app =
            case config.load session args app of
                Just model ->
                    Sub.map anonymizeMsg (config.subscriptions model)

                Nothing ->
                    Sub.none

        anonymizeMsg : msg -> Msg session app
        anonymizeMsg msg =
            PageMsg <|
                \inner ->
                    case config.load inner.session args inner.app of
                        Just model ->
                            let
                                ( model_, cmd ) =
                                    config.update msg model

                                session =
                                    config.session model_ inner.session
                            in
                            ( { inner
                                | app = config.save session args model_ inner.app
                                , session = session
                              }
                            , Cmd.map anonymizeMsg cmd
                            )

                        Nothing ->
                            ( inner, Cmd.none )
    in
    AnonymousPage init_ view_ subscriptions_


blank_ : AnonymousPage session app
blank_ =
    { init = \_ a -> ( a, Cmd.none )
    , view = \_ _ -> Html.text ""
    , subscriptions = \_ _ -> Sub.none
    }


handleRoute : Page Location session app -> List (Route session app) -> Json.Value -> Location -> Result (Maybe String) (AnonymousPage session app)
handleRoute (Page fallback) routes json url =
    let
        search route result =
            case result of
                Just done ->
                    Just done

                Nothing ->
                    case route of
                        Route toPage ->
                            case toPage json url of
                                Just page ->
                                    Just (Ok page)

                                Nothing ->
                                    Nothing

                        Redirect toNewUrl ->
                            Just <| Err (toNewUrl url)
    in
    case List.foldl search Nothing routes of
        Nothing ->
            Ok (fallback json url)

        Just result ->
            result



-- APPLICATION / LINKS


{-| -}
link : (route -> String) -> (route -> msg) -> route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
link toString toMsg nextRoute attributes =
    Html.a (linkAttributes toString toMsg nextRoute attributes)


{-| -}
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
    Html.Events.onWithOptions "click" { preventDefault = True, stopPropagation = False } decoder
