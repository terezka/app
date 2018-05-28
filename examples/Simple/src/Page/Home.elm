module Page.Home exposing (Model, page)

import Data.Post as Post
import Data.Status as Status
import Html
import Html.Attributes
import Html.Events
import Http
import Navigation
import Route
import Spa


-- PAGE


{-| -}
page : Spa.Page () { app | home : Maybe Model }
page =
    Spa.page
        { load = \_ -> .home
        , save = \_ home app -> { app | home = Just home }
        , init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


{-| -}
type alias Model =
    { posts : Status.Status Http.Error (List Post.Post)
    }



-- INIT


init : Maybe Model -> () -> ( Model, Cmd Msg )
init cached _ =
    case cached of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( { posts = Status.loading }
            , Http.send ReceivePosts Post.request
            )



-- UPDATE


type Msg
    = SetLocation Route.Route
    | ReceivePosts (Result Http.Error (List Post.Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLocation route ->
            ( model, Route.navigate route )

        ReceivePosts (Ok posts) ->
            ( { model | posts = Status.success posts }, Cmd.none )

        ReceivePosts (Err err) ->
            ( { model | posts = Status.failure err }, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text "HOME for real"
        , Route.button SetLocation (Route.Profile 1 Route.Submissions) [] [ Html.text "to profile 1" ]
        , Route.button SetLocation (Route.Profile 2 Route.Progress) [] [ Html.text "to profile 2" ]
        , viewPosts model.posts
        ]


viewPosts : Status.Status Http.Error (List Post.Post) -> Html.Html msg
viewPosts posts =
    case posts of
        Status.Loading _ ->
            Html.text "Loading..."

        Status.Finished (Status.Success posts) ->
            Html.div [] (List.map viewPost posts)

        Status.Finished _ ->
            Html.text "Could not load posts!"


viewPost : Post.Post -> Html.Html msg
viewPost post =
    Html.div [] [ Html.text post.title ]
