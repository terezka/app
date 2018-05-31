module Page.NotFound exposing (Model, Msg, init, update, view)

import App
import Html
import Html.Events
import Navigation
import Route


-- MODEL


{-| -}
type alias Model =
    ()



-- INIT


init : Maybe Model -> ( Model, Cmd Msg )
init _ =
    -- TODO Navigation.Location ->
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
