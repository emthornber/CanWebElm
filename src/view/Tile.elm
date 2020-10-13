{-
   Testbed for embedded Svg usage creating Signal Panel tiles

   05 October, 2020 - E M Thornber
   Created
-}


module Tile exposing (view)

import Dict exposing (Dict)
import Html
import Html.Attributes as HtmlA
import Model
import Panel exposing (Layout)
import Svg
import Svg.Attributes as SvgA


view : Model.Model -> Html.Html msg
view model =
    Html.div [ HtmlA.class "panel" ]
        [ Html.h2 [] [ Html.text Panel.layout.name ]
        , Svg.svg
            [ SvgA.id "tiles"
            , SvgA.width (Tuple.first Panel.window)
            , SvgA.height (Tuple.second Panel.window)
            ]
            (List.concat
                [ viewBackground model.panel
                , viewTracks
                , viewTrackCircuits
                , viewSpots
                , viewTurnouts
                , viewLevers
                , viewControls
                ]
            )
        ]



-- Background Tiles


viewBackground : Layout -> List (Svg.Svg msg)
viewBackground layout =
    let
        start =
            String.fromInt layout.margins

        xStop =
            String.fromInt (layout.margins + Tuple.first layout.size * layout.tiles)

        yStop =
            String.fromInt (layout.margins + Tuple.second layout.size * layout.tiles)

        xGrid : Int -> List (Svg.Svg msg)
        xGrid linenum =
            let
                yBoth =
                    String.fromInt (layout.margins + linenum * layout.tiles)
            in
            [ Svg.line
                [ SvgA.x1 start, SvgA.y1 yBoth, SvgA.x2 xStop, SvgA.y2 yBoth ]
                []
            ]

        yGrid : Int -> List (Svg.Svg msg)
        yGrid linenum =
            let
                xBoth =
                    String.fromInt (layout.margins + linenum * layout.tiles)
            in
            [ Svg.line
                [ SvgA.x1 xBoth, SvgA.y1 start, SvgA.x2 xBoth, SvgA.y2 yStop ]
                []
            ]
    in
    [ Svg.rect
        [ SvgA.x Panel.border
        , SvgA.y Panel.border
        , SvgA.width (Tuple.first Panel.edge)
        , SvgA.height (Tuple.second Panel.edge)
        , SvgA.rx "0"
        , SvgA.fill "black"
        ]
        []
    , Svg.rect
        [ SvgA.x Panel.margin
        , SvgA.y Panel.margin
        , SvgA.width (Tuple.first Panel.panel)
        , SvgA.height (Tuple.second Panel.panel)
        , SvgA.rx "0"
        , SvgA.fill Panel.layout.bkgFill
        ]
        []
    , Svg.g
        [ SvgA.stroke "black"
        ]
        (List.concat
            [ List.concatMap xGrid <| List.range 1 (Tuple.second layout.size - 1)
            , List.concatMap yGrid <| List.range 1 (Tuple.first layout.size - 1)
            ]
        )
    ]



-- Track Layout definitions


trackFill : Model.Track -> String
trackFill track =
    case track.state of
        Just _ ->
            "black"

        Nothing ->
            "none"


viewTracks : List (Svg.Svg msg)
viewTracks =
    List.concat <| List.map viewTrack Model.tracks


viewTrack : Model.Track -> List (Svg.Svg msg)
viewTrack track =
    case track.direction of
        Model.NS ->
            viewTrackOrthog track

        Model.EW ->
            viewTrackOrthog track

        _ ->
            viewTrackDiag track


viewTrackOrthog : Model.Track -> List (Svg.Svg msg)
viewTrackOrthog track =
    let
        rotate =
            case track.direction of
                Model.NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.polyline
        [ SvgA.fill (trackFill track)
        , SvgA.stroke "black"
        , SvgA.points "-30,5 30,5 30,-5 -30,-5 -30,5"
        , SvgA.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]


viewTrackDiag : Model.Track -> List (Svg.Svg msg)
viewTrackDiag track =
    let
        rotate =
            case track.direction of
                Model.SE ->
                    "rotate(90)"

                Model.SW ->
                    "rotate(180)"

                Model.NW ->
                    "rotate(-90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.polyline
        [ SvgA.fill (trackFill track)
        , SvgA.stroke "black"
        , SvgA.points "5,-30 30,-5 30,5 -5,-30 5,-30"
        , SvgA.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]



-- Track Circuit State definitions


tcFill : Model.OneBit -> String
tcFill status =
    case status of
        Model.UNKN ->
            "grey"

        Model.ZERO ->
            "white"

        Model.ONE ->
            "cyan"


viewTrackCircuits : List (Svg.Svg msg)
viewTrackCircuits =
    List.concat <|
        List.map viewTC <|
            List.filter
                (\track ->
                    case track.state of
                        Just state ->
                            True

                        Nothing ->
                            False
                )
                Model.tracks


viewTC : Model.Track -> List (Svg.Svg msg)
viewTC track =
    case track.direction of
        Model.NS ->
            viewTCOrtho track <| tcFill <| Model.getOBState track.state

        Model.EW ->
            viewTCOrtho track <| tcFill <| Model.getOBState track.state

        _ ->
            viewTCDiag track <| tcFill <| Model.getOBState track.state


viewTCOrtho : Model.Track -> String -> List (Svg.Svg msg)
viewTCOrtho track status =
    let
        rotate =
            case track.direction of
                Model.NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.g
        [ SvgA.fill status
        , SvgA.stroke status
        , SvgA.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        [ Svg.rect
            [ SvgA.x "-23"
            , SvgA.y "-2"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            ]
            []
        , Svg.rect
            [ SvgA.x "7"
            , SvgA.y "-2"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            ]
            []
        ]
    ]


viewTCDiag : Model.Track -> String -> List (Svg.Svg msg)
viewTCDiag track status =
    let
        rotate =
            case track.direction of
                Model.SE ->
                    "rotate(135)"

                Model.SW ->
                    "rotate(-135)"

                Model.NW ->
                    "rotate(-45)"

                _ ->
                    "rotate(45)"
    in
    [ Svg.rect
        [ SvgA.x "-8"
        , SvgA.y "-23.25"
        , SvgA.width "16"
        , SvgA.height "4"
        , SvgA.rx "2"
        , SvgA.fill status
        , SvgA.stroke status
        , SvgA.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]



-- Indicator State definitions (Spot detectors) e.g. Stop boards, Limits of Shunt, Platform stops


spotFill : Model.OneBit -> String
spotFill spot =
    case spot of
        Model.UNKN ->
            "grey"

        Model.ZERO ->
            "white"

        Model.ONE ->
            "green"


viewSpots : List (Svg.Svg msg)
viewSpots =
    List.concat <|
        List.map viewSpot <|
            List.filter
                (\track ->
                    case track.spot of
                        Just spot ->
                            True

                        Nothing ->
                            False
                )
                Model.tracks


viewSpot : Model.Track -> List (Svg.Svg msg)
viewSpot track =
    case track.direction of
        Model.NS ->
            viewSpotOrtho track <| spotFill <| Model.getOBState track.spot

        Model.EW ->
            viewSpotOrtho track <| spotFill <| Model.getOBState track.spot

        _ ->
            viewSpotDiag track <| spotFill <| Model.getOBState track.spot


viewSpotOrtho : Model.Track -> String -> List (Svg.Svg msg)
viewSpotOrtho track spot =
    let
        rotate =
            case track.direction of
                Model.NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.g
        [ SvgA.fill spot
        , SvgA.stroke spot
        , SvgA.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        [ Svg.rect
            [ SvgA.x "-10"
            , SvgA.y "-12"
            , SvgA.width "20"
            , SvgA.height "3"
            , SvgA.rx "2"
            ]
            []
        ]
    ]


viewSpotDiag : Model.Track -> String -> List (Svg.Svg msg)
viewSpotDiag track spot =
    let
        rotate =
            case track.direction of
                Model.SE ->
                    "rotate(135)"

                Model.SW ->
                    "rotate(-135)"

                Model.NW ->
                    "rotate(-45)"

                _ ->
                    "rotate(45)"
    in
    [ Svg.rect
        [ SvgA.x "-7"
        , SvgA.y "-31"
        , SvgA.width "14"
        , SvgA.height "3"
        , SvgA.rx "2"
        , SvgA.fill spot
        , SvgA.stroke spot
        , SvgA.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]



-- Turnout Settings definitions
---- Convert double bit state into colours


textAttr : Model.Turnout -> ( Int, Int ) -> List (Svg.Attribute msg)
textAttr turnout offset =
    let
        translation =
            "translate(" ++ String.fromInt (Tuple.first offset) ++ ", " ++ String.fromInt (Tuple.second offset) ++ ")"

        rotation =
            case turnout.orientation of
                Model.TONorth ->
                    "rotate(180)"

                Model.TOEast ->
                    "rotate(180)"

                Model.TOSouth ->
                    "rotate(0)"

                Model.TOWest ->
                    "rotate(0)"
    in
    [ SvgA.fontFamily "monospace"
    , SvgA.fontSize "small"
    , SvgA.fill "black"
    , SvgA.stroke "none"
    , SvgA.x "0"
    , SvgA.y "4"
    , SvgA.textAnchor "middle"
    , SvgA.transform (String.join " " [ translation, rotation ])
    ]


turnoutFill : Model.Turnout -> String
turnoutFill turnout =
    case turnout.state of
        Just _ ->
            "black"

        Nothing ->
            "none"


turnoutRotation : Model.Turnout -> String
turnoutRotation turnout =
    case turnout.orientation of
        Model.TONorth ->
            "rotate(90)"

        Model.TOEast ->
            "rotate(180)"

        Model.TOSouth ->
            "rotate(-90)"

        Model.TOWest ->
            "rotate(0)"


viewTurnouts : List (Svg.Svg msg)
viewTurnouts =
    List.concat <| List.map viewTurnout Model.turnouts


viewTurnout : Model.Turnout -> List (Svg.Svg msg)
viewTurnout turnout =
    case turnout.hand of
        Model.TOLeft ->
            viewTOLeft turnout

        Model.TORight ->
            viewTORight turnout

        Model.TOWye ->
            viewTOWye turnout


viewTOLeft : Model.Turnout -> List (Svg.Svg msg)
viewTOLeft turnout =
    [ Svg.g
        [ SvgA.fill (turnoutFill turnout)
        , SvgA.stroke "black"
        , SvgA.transform (String.join " " [ Panel.translateTile turnout.coords, turnoutRotation turnout ])
        ]
        [ Svg.polyline
            [ SvgA.points "-30,5 30,5 30,-5 -30,-5 -30,5"
            ]
            []
        , Svg.polyline
            [ SvgA.points "-5,30 5,30 28,7 18,7 -5,30"
            ]
            []
        , Svg.text_
            (textAttr turnout ( 0, -18 ))
            [ Svg.text turnout.name ]
        ]
    ]


viewTORight : Model.Turnout -> List (Svg.Svg msg)
viewTORight turnout =
    [ Svg.g
        [ SvgA.fill (turnoutFill turnout)
        , SvgA.stroke "black"
        , SvgA.transform (String.join " " [ Panel.translateTile turnout.coords, turnoutRotation turnout ])
        ]
        [ Svg.polyline
            [ SvgA.points "-30,5 30,5 30,-5 -30,-5 -30,5"
            ]
            []
        , Svg.polyline
            [ SvgA.points "-5,-30 5,-30 28,-7 18,-7 -5,-30"
            ]
            []
        , Svg.text_
            (textAttr turnout ( 0, 18 ))
            [ Svg.text turnout.name ]
        ]
    ]


viewTOWye : Model.Turnout -> List (Svg.Svg msg)
viewTOWye turnout =
    [ Svg.g
        [ SvgA.fill (turnoutFill turnout)
        , SvgA.stroke "black"
        , SvgA.transform (String.join " " [ Panel.translateTile turnout.coords, turnoutRotation turnout ])
        ]
        [ Svg.polyline
            [ SvgA.points "0,5 30,5 30,-5 0,-5 0,5"
            ]
            []
        , Svg.polyline
            [ SvgA.points "-5,30 5,30 28,7 18,7 -5,30"
            ]
            []
        , Svg.polyline
            [ SvgA.points "-5,-30 5,-30 28,-7 18,-7 -5,-30"
            ]
            []
        , Svg.text_
            (textAttr turnout ( -15, 0 ))
            [ Svg.text turnout.name ]
        ]
    ]



-- Lever State definitions


leverFill : Model.TwoBit -> ( String, String )
leverFill double =
    case double of
        ( Model.UNKN, _ ) ->
            ( "grey", "grey" )

        ( _, Model.UNKN ) ->
            ( "grey", "grey" )

        ( Model.ZERO, Model.ZERO ) ->
            ( "none", "none" )

        ( Model.ONE, Model.ZERO ) ->
            ( "white", "none" )

        ( Model.ZERO, Model.ONE ) ->
            ( "none", "white" )

        _ ->
            ( "red", "red" )


viewLevers : List (Svg.Svg msg)
viewLevers =
    List.concat <|
        List.map viewLever <|
            List.filter
                (\turnout ->
                    case turnout.state of
                        Just spot ->
                            True

                        Nothing ->
                            False
                )
                Model.turnouts


viewLever : Model.Turnout -> List (Svg.Svg msg)
viewLever turnout =
    let
        getTON : Maybe ( String, String ) -> Maybe String
        getTON state =
            case state of
                Just value ->
                    Just (Tuple.first value)

                Nothing ->
                    Nothing

        getTOR : Maybe ( String, String ) -> Maybe String
        getTOR state =
            case state of
                Just value ->
                    Just (Tuple.second value)

                Nothing ->
                    Nothing

        status =
            ( Model.getOBState <| getTON turnout.state, Model.getOBState <| getTOR turnout.state )
    in
    case turnout.hand of
        Model.TOLeft ->
            viewLeverLeft turnout (leverFill status) (turnoutRotation turnout)

        Model.TORight ->
            viewLeverRight turnout (leverFill status) (turnoutRotation turnout)

        Model.TOWye ->
            viewLeverWye turnout (leverFill status) (turnoutRotation turnout)


viewLeverLeft : Model.Turnout -> ( String, String ) -> String -> List (Svg.Svg msg)
viewLeverLeft turnout status rotation =
    [ Svg.g
        [ SvgA.stroke "white"
        , SvgA.transform (String.join " " [ Panel.translateTile turnout.coords, rotation ])
        ]
        [ Svg.rect
            [ SvgA.x "-23"
            , SvgA.y "-2"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ SvgA.x "7"
            , SvgA.y "-2"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ SvgA.x "-4"
            , SvgA.y "-23.25"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.second status)
            , SvgA.transform "rotate( 135 )"
            ]
            []
        ]
    ]


viewLeverRight : Model.Turnout -> ( String, String ) -> String -> List (Svg.Svg msg)
viewLeverRight turnout status rotation =
    [ Svg.g
        [ SvgA.stroke "white"
        , SvgA.transform (String.join " " [ Panel.translateTile turnout.coords, rotation ])
        ]
        [ Svg.rect
            [ SvgA.x "-23"
            , SvgA.y "-2"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ SvgA.x "7"
            , SvgA.y "-2"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ SvgA.x "-12"
            , SvgA.y "-23.25"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.second status)
            , SvgA.transform "rotate( 45 )"
            ]
            []
        ]
    ]


viewLeverWye : Model.Turnout -> ( String, String ) -> String -> List (Svg.Svg msg)
viewLeverWye turnout status rotation =
    [ Svg.g
        [ SvgA.stroke "white"
        , SvgA.transform (String.join " " [ Panel.translateTile turnout.coords, rotation ])
        ]
        [ Svg.rect
            [ SvgA.x "-12"
            , SvgA.y "-23.25"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.first status)
            , SvgA.transform "rotate( 45 )"
            ]
            []
        , Svg.rect
            [ SvgA.x "15"
            , SvgA.y "-2"
            , SvgA.width "8"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill "white"
            ]
            []
        , Svg.rect
            [ SvgA.x "-4"
            , SvgA.y "-23.25"
            , SvgA.width "16"
            , SvgA.height "4"
            , SvgA.rx "2"
            , SvgA.fill (Tuple.second status)
            , SvgA.transform "rotate( 135 )"
            ]
            []
        ]
    ]



-- Controls


viewControls : List (Svg.Svg msg)
viewControls =
    List.concat <| List.map viewControl Model.controls


viewControl : Model.Control -> List (Svg.Svg msg)
viewControl control =
    case control.switch of
        Model.Toggle ->
            List.concat [ viewSwBkgd control, viewSwState control ]

        _ ->
            []


viewSwBkgd : Model.Control -> List (Svg.Svg msg)
viewSwBkgd switch =
    [ Svg.g
        [ SvgA.transform (Panel.translateTile switch.coords)
        ]
        [ Svg.circle
            [ SvgA.cx "0"
            , SvgA.cy "15"
            , SvgA.r "10"
            , SvgA.stroke "black"
            , SvgA.fill "black"
            ]
            []
        , Svg.circle
            [ SvgA.cx "-17"
            , SvgA.cy "-20"
            , SvgA.r "5"
            , SvgA.stroke "black"
            , SvgA.fill "none"
            ]
            []
        , Svg.circle
            [ SvgA.cx "17"
            , SvgA.cy "-20"
            , SvgA.r "5"
            , SvgA.stroke "black"
            , SvgA.fill "none"
            ]
            []
        , Svg.g
            [ SvgA.fontFamily "monospace"
            , SvgA.fontSize "small"
            ]
            [ Svg.text_
                [ SvgA.x "-17"
                , SvgA.y "5"
                , SvgA.textAnchor "middle"
                ]
                [ Svg.text "N" ]
            , Svg.text_
                [ SvgA.x "17"
                , SvgA.y "5"
                , SvgA.textAnchor "middle"
                ]
                [ Svg.text "R" ]
            , Svg.text_
                [ SvgA.x "0"
                , SvgA.y "-5"
                , SvgA.textAnchor "middle"
                ]
                [ Svg.text switch.name ]
            ]
        ]
    ]


viewSwState : Model.Control -> List (Svg.Svg msg)
viewSwState switch =
    List.concat [ viewKnob switch, viewLamps switch ]


viewKnob : Model.Control -> List (Svg.Svg msg)
viewKnob switch =
    let
        knobRotate : Model.OneBit -> String
        knobRotate action =
            case action of
                Model.UNKN ->
                    "rotate(180)"

                Model.ZERO ->
                    "rotate(-45)"

                Model.ONE ->
                    "rotate(45)"
    in
    [ Svg.g
        [ SvgA.transform (Panel.translateTile switch.coords)
        ]
        [ Svg.polyline
            [ SvgA.fill "none"
            , SvgA.stroke "white"
            , SvgA.strokeLinecap "round"
            , SvgA.strokeWidth "0.75"
            , SvgA.points "0,-9 5,-2 2,-2 2,8 -2,8 -2,-2 -5,-2 0,-9"
            , SvgA.transform (String.join " " [ "translate(0 15)", knobRotate <| Model.getOBState switch.action ])
            ]
            []
        ]
    ]


viewLamps : Model.Control -> List (Svg.Svg msg)
viewLamps switch =
    let
        getLampN : Maybe ( String, String ) -> Maybe String
        getLampN state =
            case state of
                Just value ->
                    Just (Tuple.first value)

                Nothing ->
                    Nothing

        getLampR : Maybe ( String, String ) -> Maybe String
        getLampR state =
            case state of
                Just value ->
                    Just (Tuple.second value)

                Nothing ->
                    Nothing

        status =
            leverFill ( Model.getOBState <| getLampN switch.state, Model.getOBState <| getLampR switch.state )
    in
    [ Svg.g
        [ SvgA.transform (Panel.translateTile switch.coords)
        ]
        [ Svg.circle
            [ SvgA.cx "-17"
            , SvgA.cy "-20"
            , SvgA.r "4.5"
            , SvgA.stroke "white"
            , SvgA.fill (Tuple.first status)
            ]
            []
        , Svg.circle
            [ SvgA.cx "17"
            , SvgA.cy "-20"
            , SvgA.r "4.5"
            , SvgA.stroke "white"
            , SvgA.fill (Tuple.second status)
            ]
            []
        ]
    ]
