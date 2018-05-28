module Spa
    exposing
        ( Application
        , Page
        , Route
        , application
        , button
        , link
        , page
        , route
        )

{-|

@docs application, Application, Page, page, Route, route, button, link

-}

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Navigation exposing (Location)
import UrlParser as Parser exposing (Parser)


-- API / PROGRAM


{-| -}
type alias Application app =
    Platform.Program Never (Model app) (Msg app)


{-| -}
application : app -> List (Route app) -> Page Location app -> Application app
application init routes fallback =
    Navigation.program ReceiveLocation
        { init = appInit fallback routes init
        , view = appView
        , update = appUpdate fallback routes
        , subscriptions = appSubscriptions
        }


{-| -}
type Page args app
    = Page (args -> AnonymousPage app)


{-| -}
page :
    { init : Maybe model -> args -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html.Html msg
    , subscriptions : model -> Sub msg
    , save : args -> model -> app -> app
    , load : args -> app -> Maybe model
    }
    -> Page args app
page config =
    Page <|
        \args ->
            let
                init_ : app -> ( app, Cmd (Msg app) )
                init_ app =
                    config.init (config.load args app) args
                        |> Tuple.mapFirst (\model -> config.save args model app)
                        |> Tuple.mapSecond (Cmd.map anonymizeMsg)

                view_ : app -> Html.Html (Msg app)
                view_ app =
                    case config.load args app of
                        Just model ->
                            Html.map anonymizeMsg (config.view model)

                        Nothing ->
                            Html.text "Oops, something went wrong. Please refresh."

                subscriptions_ : app -> Sub (Msg app)
                subscriptions_ app =
                    case config.load args app of
                        Just model ->
                            Sub.map anonymizeMsg (config.subscriptions model)

                        Nothing ->
                            Sub.none

                anonymizeMsg : msg -> Msg app
                anonymizeMsg msg =
                    PageMsg <|
                        \app ->
                            case config.load args app of
                                Just model ->
                                    config.update msg model
                                        |> Tuple.mapFirst (\model_ -> config.save args model_ app)
                                        |> Tuple.mapSecond (Cmd.map anonymizeMsg)

                                Nothing ->
                                    ( app, Cmd.none )
            in
            AnonymousPage init_ view_ subscriptions_


{-| -}
type Route app
    = Route (Location -> Maybe (AnonymousPage app))


{-| -}
route : Page args app -> Parser (args -> args) args -> Route app
route (Page toPage) parser =
    Route <|
        \url ->
            case Parser.parsePath parser url of
                Just args ->
                    Just (toPage args)

                Nothing ->
                    Nothing



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



-- APPLICATION / INTERNAL / MODEL


type alias Model app =
    { page : AnonymousPage app
    , app : app
    }



-- APPLICATION / INTERNAL / INIT


appInit : Page Location app -> List (Route app) -> app -> Location -> ( Model app, Cmd (Msg app) )
appInit fallback routes app url =
    let
        page_ =
            findPageForRoute fallback routes url
    in
    Tuple.mapFirst (Model page_) (page_.init app)



-- APPLICATION / INTERNAL / UPDATE


type Msg app
    = ReceiveLocation Location
    | PageMsg (app -> ( app, Cmd (Msg app) ))


appUpdate : Page Location app -> List (Route app) -> Msg app -> Model app -> ( Model app, Cmd (Msg app) )
appUpdate fallback routes msg model =
    case msg of
        ReceiveLocation location ->
            appInit fallback routes model.app location

        PageMsg updatePage ->
            Tuple.mapFirst (Model model.page) (updatePage model.app)



-- APPLICATION / INTERNAL / VIEW


appView : Model app -> Html.Html (Msg app)
appView model =
    model.page.view model.app



-- APPLICATION / INTERNAL / SUBS


appSubscriptions : Model app -> Sub (Msg app)
appSubscriptions model =
    model.page.subscriptions model.app



-- APPLICATION / INTERNAL / PAGE


type alias AnonymousPage app =
    { init : app -> ( app, Cmd (Msg app) )
    , view : app -> Html.Html (Msg app)
    , subscriptions : app -> Sub (Msg app)
    }


findPageForRoute : Page Location app -> List (Route app) -> Location -> AnonymousPage app
findPageForRoute (Page fallback) routes url =
    let
        search (Route toPage) result =
            case result of
                Just done ->
                    Just done

                Nothing ->
                    toPage url
    in
    List.foldl search Nothing routes
        |> Maybe.withDefault (fallback url)
