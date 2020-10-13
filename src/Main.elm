module Main exposing (main)

import Html
import Html.Attributes as HtmlA
import Model
import Tile



-- temporary main code


view : Model.Model -> Html.Html msg
view model =
    Html.div [ HtmlA.class "content" ]
        [ Html.h1 [] [ Html.text "CanWeb" ]
        , Tile.view model
        ]


main =
    view Model.initialModel
