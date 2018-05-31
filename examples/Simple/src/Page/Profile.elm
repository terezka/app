module Page.Profile exposing (Model, Msg, init, update, view)

import App
import Data.Status as Status
import Data.User as User
import Dict
import Html
import Html.Attributes
import Html.Events
import Http
import Navigation
import Route
import UrlParser as P exposing ((</>))


-- MODEL


{-| -}
type alias Model =
    { id : User.Id
    , user : Status.Status Http.Error User.User
    , tab : Route.Tab
    }



-- INIT


init : Maybe Model -> User.Id -> Route.Tab -> ( Model, Cmd Msg )
init cached id tab =
    case cached of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( { id = id, user = Status.loading, tab = tab }
            , Http.send ReceiveUser (User.request id)
            )



-- UPDATE


type Msg
    = SetLocation Route.Route
    | SetLocationFaulty
    | ReceiveUser (Result Http.Error User.User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLocation route ->
            ( model, Route.navigate route )

        SetLocationFaulty ->
            ( model, Navigation.newUrl "not-found-lala" )

        ReceiveUser (Ok user) ->
            ( { model | user = Status.success user }, Cmd.none )

        ReceiveUser (Err err) ->
            ( { model | user = Status.failure err }, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text "PROFILE"
        , Html.div [] [ Html.text (toString model.tab) ]
        , Html.div [] [ viewUser model.user ]
        , Route.link SetLocation Route.Home [] [ Html.text "to home" ]
        , Html.button
            [ Html.Events.onClick SetLocationFaulty ]
            [ Html.text "to not found" ]
        ]


viewUser : Status.Status Http.Error User.User -> Html.Html Msg
viewUser user =
    case user of
        Status.Loading _ ->
            Html.text "Loading..."

        Status.Finished (Status.Success user) ->
            Html.text <| "belonging to: " ++ user.name

        Status.Finished _ ->
            Html.text "Could not load user!"
