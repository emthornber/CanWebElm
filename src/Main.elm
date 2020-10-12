module Main exposing (main)

import Html
import Model exposing (..)
import Tile exposing (view)



-- temporary main code


view : Model -> Html.Html msg
view model =
    Tile.view model


main =
    view initialModel
