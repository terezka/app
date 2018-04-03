module Main exposing (main)


import Html
import Navigation
import Global
import UrlParser as Url
import Page.Home as Home
import Page.Profile as Profile


-- PROGRAM


main : Program Never Model Msg
main =
    Navigation.program ChangeUrl
        { init = init Nothing
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


-- MODEL


type alias Model =
    { global : Global.Global
    , page : Page
    }


type Page
    = Home Home.Model
    | Profile Profile.Model
    | NotFound



-- INIT


init : Maybe Global.Global -> Navigation.Location -> ( Model, Cmd Msg )
init global location =
    let
        thunkPage page msg init_ () =
            updatePage page msg (init_ global)

        parser =
            Url.oneOf
                [ Url.map (thunkPage Home HomeMsg Home.init) Url.top
                , Url.map (thunkPage Profile ProfileMsg Profile.init) (Url.s "profile")
                ]
    in
    case Url.parsePath parser location of
        Just promise ->
            promise ()

        Nothing ->
            ( { global = Global.init, page = NotFound }
            , Cmd.none
            )



-- UPDATE


type Msg
    = ChangeUrl Navigation.Location
    | HomeMsg Home.Msg
    | ProfileMsg Profile.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, ChangeUrl location ) ->
            init (Just model.global) location

        ( Home homeModel, HomeMsg msg ) ->
            updatePage Home HomeMsg <|
                Home.update msg model.global homeModel

        ( Profile profileModel, ProfileMsg msg ) ->
            updatePage Profile ProfileMsg <|
                Profile.update msg model.global profileModel

        ( _, _ ) ->
            ( model, Cmd.none )


updatePage : (page -> Page) -> (msg -> Msg) -> ( Global.Global, page, Cmd msg ) -> ( Model, Cmd Msg )
updatePage toPage toMsg ( global, page, cmd ) =
    ( { global = global
      , page = toPage page
      }
    , Cmd.map toMsg cmd
    )


view : Model -> Html.Html Msg
view model =
    case model.page of
        Home homeModel ->
            Home.view model.global homeModel
                |> Html.map HomeMsg

        Profile profileModel ->
            Profile.view model.global profileModel
                |> Html.map ProfileMsg

        NotFound ->
            Html.text "not found"


