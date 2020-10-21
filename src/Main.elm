module Main exposing (main)

import Browser
import CbusState
import Html
import Html.Attributes as HtmlA
import Http
import LayoutJson
import Model
import Tile



-- Main code


view : Model.Model -> Html.Html Model.Msg
view model =
    Html.div [ HtmlA.class "content" ]
        [ Html.h1 [] [ Html.text "CanWeb" ]
        , Tile.view model
        , CbusState.view model
        ]


main : Program () Model.Model Model.Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model.Model, Cmd Model.Msg )
init () =
    ( Model.initialModel, LayoutJson.fetchLayout )


update : Model.Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    case msg of
        Model.LoadLayout (Ok layout) ->
            ( { model | layout = Just layout, status = Model.Loaded }
            , Cmd.none
            )

        Model.LoadLayout (Err _) ->
            ( { model | status = Model.Failure "Failed to load" }, Cmd.none )


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    Sub.none
