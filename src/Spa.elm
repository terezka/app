module Spa exposing (Config, Default, Page, Program, Route, button, default, link, program, route)

{-|

@docs program, Program, Config

@docs route, Route, Page

@docs Default, default

@docs link, button

-}

import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Navigation
import UrlParser as Parser


-- API / PROGRAM


{-| -}
type alias Program app =
  Platform.Program Never (Model app) (Msg app)


{-| -}
type alias Config app =
  { pages : List (Route app)
  , default : Default app
  , init : app
  }


{-| -}
program : Config app -> Program app
program config =
  Navigation.program ReceiveLocation
    { init = init config config.init
    , update = update config
    , view = view config
    , subscriptions = subscriptions config
    }



-- API / PAGE CONFIG


{-| -}
type alias Page app args model msg =
  { get : app -> Maybe model
  , set : model -> app -> app
  , init : Maybe model -> args -> ( model, Cmd msg )
  , update : msg -> model -> ( model, Cmd msg )
  , view : model -> Html.Html msg
  , subscriptions : model -> Sub msg
  }



-- API / ROUTE


{-| -}
type Route app
  = Route (Navigation.Location -> Maybe (InternalPage app))


{-| -}
route : Page app args model msg -> List (Parser.Parser (args -> args) args) -> Route app
route config routes =
  Route <| \location ->
    Parser.parsePath (Parser.oneOf routes) location
      |> Maybe.map (internalPage config)



-- API / DEFAULT PAGE


{-| -}
type Default app
  = Default (Navigation.Location -> InternalPage app)


{-| -}
default : Page app Navigation.Location model msg -> Default app
default config =
  Default (internalPage config)



-- API / LINKS


{-| -}
link : (route -> String) -> (route -> msg) -> route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
link toString toMsg route attributes =
  Html.a <| attributes ++
    [ onClick (toMsg route)
    , Html.Attributes.target "_blank"
    , Html.Attributes.href (toString route)
    ]


{-| -}
button : (route -> String) -> (route -> msg) -> route -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
button toString toMsg route attributes =
  Html.a <| attributes ++
    [ onClick (toMsg route)
    , Html.Attributes.target "_blank"
    , Html.Attributes.href (toString route)
    ]



-- INTERNAL / MODEL


type alias Model app =
  { page : InternalPage app
  , app : app
  }



-- INTERNAL / INIT


init : Config app -> app -> Navigation.Location -> ( Model app, Cmd (Msg app) )
init config app location =
  let
      page =
          find config.default config.pages location
  in
  Tuple.mapFirst (Model page) (page.init app)



-- INTERNAL / UPDATE


type Msg app
  = ReceiveLocation Navigation.Location
  | PageMsg (app -> ( app, Cmd (Msg app) ))


update : Config app -> Msg app -> Model app -> ( Model app, Cmd (Msg app) )
update config msg model =
  case msg of
    ReceiveLocation location ->
      init config model.app location

    PageMsg update ->
      Tuple.mapFirst (Model model.page) (update model.app)



-- INTERNAL / VIEW


view : Config app -> Model app -> Html.Html (Msg app)
view config model =
  model.page.view model.app



-- INTERNAL / SUBS


subscriptions : Config app -> Model app -> Sub (Msg app)
subscriptions config model =
  model.page.subscriptions model.app



-- INTERNAL / PAGE


type alias InternalPage app =
  { init : app -> ( app, Cmd (Msg app) )
  , view : app -> Html.Html (Msg app)
  , subscriptions : app -> Sub (Msg app)
  }


find : Default app -> List (Route app) -> Navigation.Location -> InternalPage app
find (Default default) pages location =
  let
    fold (Route route) final =
      case final of
        Just found ->
          Just found

        Nothing ->
          route location
  in
  List.foldl fold Nothing pages
      |> Maybe.withDefault (default location)


internalPage : Page app args model msg -> args -> InternalPage app
internalPage config args =
  let
    init app =
      config.init (config.get app) args
        |> Tuple.mapFirst (\model -> config.set model app)
        |> Tuple.mapSecond (Cmd.map (PageMsg << anonymize))

    view app =
      case config.get app of
        Just model ->
          Html.map (PageMsg << anonymize) (config.view model)

        Nothing ->
          Html.text "Oops, something went terrible wrong. Please refresh."

    subscriptions app =
      case config.get app of
        Just model ->
          Sub.map (PageMsg << anonymize) (config.subscriptions model)

        Nothing ->
          Sub.none

    anonymize msg app =
      case config.get app of
        Just model ->
          config.update msg model
            |> Tuple.mapFirst (\model -> config.set model app)
            |> Tuple.mapSecond (Cmd.map (PageMsg << anonymize))

        Nothing ->
          ( app, Cmd.none )
  in
  { init = init
  , view = view
  , subscriptions = subscriptions
  }



-- INTERNAL / LINKS


onClick : msg -> Html.Attribute msg
onClick msg =
  let
    isSpecialClick =
      Json.Decode.map2 (\isCtrl isMeta -> isCtrl || isMeta)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)

    succeedIfNotSpecial msg preventDefault =
      if preventDefault then
        Json.Decode.fail "Click was a special click"
      else
        Json.Decode.succeed msg

    decoder =
      Json.Decode.andThen (succeedIfNotSpecial msg) isSpecialClick
  in
  Html.Events.onWithOptions "click" { stopPropagation = False, preventDefault = True } decoder
