module Page.Home exposing (Model, init, Msg, update, view)


import Html
import Html.Events
import Global
import Navigation



-- MODEL


{-| -}
type alias Model =
    { books : List Int
    }



-- INIT


{-| -}
init : Maybe Global.Global -> ( Global.Global, Model, Cmd Msg )
init global =
    case global of
        Just global ->
            ( global
            , { books = [] }
            , Cmd.none
            )

        Nothing ->
            ( Global.init
            , { books = [] }
            , Cmd.none
            )



-- UPDATE


{-| -}
type Msg
    = ChangeUrl String


{-| -}
update : Msg -> Global.Global -> Model -> ( Global.Global, Model, Cmd Msg )
update msg global model =
    case msg of
        ChangeUrl url ->
            ( global, model, Navigation.newUrl url )



-- VIEW


{-| -}
view : Global.Global -> Model -> Html.Html Msg
view global model =
    Html.div []
        [ Html.text "HOME"
        , Html.button
            [ Html.Events.onClick (ChangeUrl "/profile") ]
            [ Html.text "to profile" ]
        ]

