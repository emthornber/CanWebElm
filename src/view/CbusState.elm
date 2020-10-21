{-
   Display contents of the CBUS dictionary

      19 October, 2020 - E M Thornber
      Created
-}


module CbusState exposing (view)

import Dict
import Html
import Html.Attributes as HtmlA
import Model


view : Model.Model -> Html.Html Model.Msg
view model =
    case model.layout of
        Just layout ->
            viewCbusDict layout.cbus

        Nothing ->
            viewNoLayout


viewCbusDict : Model.CBUSStateDict -> Html.Html Model.Msg
viewCbusDict dict =
    Html.div [ HtmlA.class "cbus" ]
        [ Html.h2 [] [ Html.text "CBUS States" ]
        , Html.table []
            ([ Html.thead []
                [ Html.th [] [ Html.text "Name" ]
                , Html.th [] [ Html.text "Event" ]
                , Html.th [] [ Html.text "State" ]
                ]
             ]
                ++ List.map viewCbusDictEntry (Dict.toList dict)
                ++ [ Html.tr []
                        [ Html.td [] [ Html.text "-----" ]
                        , Html.td [] [ Html.text "-----" ]
                        , Html.td [] [ Html.text "-----" ]
                        ]
                   ]
            )
        ]


viewNoLayout : Html.Html Model.Msg
viewNoLayout =
    Html.div [ HtmlA.class "cbus" ]
        [ Html.h2 [] [ Html.text "No layout loaded" ]
        ]


viewCbusDictEntry : ( String, Model.CBUSState ) -> Html.Html Model.Msg
viewCbusDictEntry ( name, state ) =
    let
        event2Text : Maybe String -> String
        event2Text event =
            case event of
                Just ev ->
                    ev

                Nothing ->
                    "-"

        oneBit2Text : Model.OneBit -> String
        oneBit2Text onebit =
            case onebit of
                Model.UNKN ->
                    "UNKN"

                Model.ZERO ->
                    "ZERO"

                Model.ONE ->
                    "ONE"
    in
    Html.tr []
        [ Html.td [] [ Html.text name ]
        , Html.td [] [ Html.text (event2Text state.event) ]
        , Html.td [] [ Html.text (oneBit2Text state.state) ]
        ]
