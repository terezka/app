module App exposing (Application, Page, Route, application, button, link, page, pageWithFlags, route)

{-|


### List of content

  - Definitions
  - Create an [application](#Application)
  - Create a [route](#Route)
  - Create a [page](#Page)
      - [Link helpers](#link)


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

@docs Route, route


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


{-| Create an application. Pass your
-}
application : session -> app -> List (Route session app) -> Page Location session app -> Application session app
application session init routes fallback =
    Navigation.programWithFlags ReceiveLocation
        { init = appInit fallback routes session init
        , view = appView
        , update = appUpdate fallback routes
        , subscriptions = appSubscriptions
        }


{-| -}
type Page args session app
    = Page (Json.Value -> args -> AnonymousPage session app)


{-| -}
pageWithFlags :
    { decoder : Json.Decoder flags
    , init : flags -> session -> Maybe model -> args -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html.Html msg
    , subscriptions : model -> Sub msg
    , session : model -> session -> session
    , save : args -> model -> app -> app
    , load : args -> app -> Maybe model
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


{-| -}
page :
    { init : session -> Maybe model -> args -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html.Html msg
    , subscriptions : model -> Sub msg
    , session : model -> session -> session
    , save : args -> model -> app -> app
    , load : args -> app -> Maybe model
    }
    -> Page args session app
page config =
    Page <|
        \_ args -> toAnonymousPage config args


{-| -}
type Route session app
    = Route (Json.Value -> Location -> Maybe (AnonymousPage session app))


{-| -}
route : Page args session app -> Parser (args -> args) args -> Route session app
route (Page toPage) parser =
    Route <|
        \json url ->
            case Parser.parsePath parser url of
                Just args ->
                    Just (toPage json args)

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
    let
        page_ =
            findPageForRoute fallback routes json location
    in
    Tuple.mapFirst (Model json session page_) (page_.init session app)



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
    model.page.view model.app



-- APPLICATION / INTERNAL / SUBS


appSubscriptions : Model session app -> Sub (Msg session app)
appSubscriptions model =
    model.page.subscriptions model.app



-- APPLICATION / INTERNAL / PAGE


type alias AnonymousPage session app =
    { init : session -> app -> ( app, Cmd (Msg session app) )
    , view : app -> Html.Html (Msg session app)
    , subscriptions : app -> Sub (Msg session app)
    }


toAnonymousPage :
    { init : session -> Maybe model -> args -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html.Html msg
    , subscriptions : model -> Sub msg
    , session : model -> session -> session
    , save : args -> model -> app -> app
    , load : args -> app -> Maybe model
    }
    -> args
    -> AnonymousPage session app
toAnonymousPage config args =
    let
        init_ : session -> app -> ( app, Cmd (Msg session app) )
        init_ session app =
            config.init session (config.load args app) args
                |> Tuple.mapFirst (\model -> config.save args model app)
                |> Tuple.mapSecond (Cmd.map anonymizeMsg)

        view_ : app -> Html.Html (Msg session app)
        view_ app =
            case config.load args app of
                Just model ->
                    Html.map anonymizeMsg (config.view model)

                Nothing ->
                    Html.text ""

        subscriptions_ : app -> Sub (Msg session app)
        subscriptions_ app =
            case config.load args app of
                Just model ->
                    Sub.map anonymizeMsg (config.subscriptions model)

                Nothing ->
                    Sub.none

        anonymizeMsg : msg -> Msg session app
        anonymizeMsg msg =
            PageMsg <|
                \inner ->
                    case config.load args inner.app of
                        Just model ->
                            let
                                ( model_, cmd ) =
                                    config.update msg model
                            in
                            ( { inner
                                | app = config.save args model_ inner.app
                                , session = config.session model_ inner.session
                              }
                            , Cmd.map anonymizeMsg cmd
                            )

                        Nothing ->
                            ( inner, Cmd.none )
    in
    AnonymousPage init_ view_ subscriptions_


findPageForRoute : Page Location session app -> List (Route session app) -> Json.Value -> Location -> AnonymousPage session app
findPageForRoute (Page fallback) routes json url =
    let
        search (Route toPage) result =
            case result of
                Just done ->
                    Just done

                Nothing ->
                    toPage json url
    in
    List.foldl search Nothing routes
        |> Maybe.withDefault (fallback json url)



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
