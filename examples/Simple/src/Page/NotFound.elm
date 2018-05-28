module Page.NotFound exposing (Model, page)

import Html
import Html.Events
import Navigation
import Route
import Spa


-- PAGE


{-| -}
page : Spa.Page Navigation.Location { app | notFound : Maybe Model }
page =
    Spa.page
        { load = \_ -> .notFound
        , save = \_ notFound app -> { app | notFound = Just notFound }
        , init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


{-| -}
type alias Model =
    ()



-- INIT


init : Maybe Model -> Navigation.Location -> ( Model, Cmd Msg )
init _ _ =
    ( ()
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetLocation Route.Route


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLocation route ->
            ( model, Route.navigate route )



-- VIEW


view : Model -> Html.Html Msg
view _ =
    Html.div []
        [ Html.text "NOT FOUND"
        , Route.link SetLocation Route.Home [] [ Html.text "to home" ]
        ]
