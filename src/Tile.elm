{-
   Testbed for embedded Svg usage creating Signal Panel tiles

   05 October, 2020 - E M Thornber
   Created
-}


module Tile exposing (view)

import Dict exposing (Dict)
import Html exposing (div, h1, text)
import Html.Attributes as Attr exposing (..)
import Model exposing (..)
import Panel exposing (Layout, bkgFill, border, edge, margin, panel, translateTile, window)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)


view : Model -> Html.Html msg
view model =
    div [ Attr.class "content" ]
        [ Html.h1 [] [ Html.text "CanWeb CBUS Elm Demo" ]
        , svg
            [ Svg.id "tiles"
            , Svg.width (Tuple.first Panel.window)
            , Svg.height (Tuple.second Panel.window)
            ]
            (List.concat [ viewBackground model.panel, viewTracks, viewTrackCircuits, viewSpots, viewTurnouts, viewLevers, viewControls ])
        ]



-- Background Tiles


viewBackground : Layout -> List (Svg msg)
viewBackground layout =
    let
        start =
            String.fromInt layout.margins

        xStop =
            String.fromInt (layout.margins + Tuple.first layout.size * layout.tiles)

        yStop =
            String.fromInt (layout.margins + Tuple.second layout.size * layout.tiles)

        xGrid : Int -> List (Svg msg)
        xGrid linenum =
            let
                yBoth =
                    String.fromInt (layout.margins + linenum * layout.tiles)
            in
            [ Svg.line
                [ Svg.x1 start, Svg.y1 yBoth, Svg.x2 xStop, Svg.y2 yBoth ]
                []
            ]

        yGrid : Int -> List (Svg msg)
        yGrid linenum =
            let
                xBoth =
                    String.fromInt (layout.margins + linenum * layout.tiles)
            in
            [ Svg.line
                [ Svg.x1 xBoth, Svg.y1 start, Svg.x2 xBoth, Svg.y2 yStop ]
                []
            ]
    in
    [ Svg.rect
        [ Svg.x Panel.border
        , Svg.y Panel.border
        , Svg.width (Tuple.first Panel.edge)
        , Svg.height (Tuple.second Panel.edge)
        , Svg.rx "0"
        , Svg.fill "black"
        ]
        []
    , Svg.rect
        [ Svg.x Panel.margin
        , Svg.y Panel.margin
        , Svg.width (Tuple.first Panel.panel)
        , Svg.height (Tuple.second Panel.panel)
        , Svg.rx "0"
        , Svg.fill Panel.bkgFill
        ]
        []
    , Svg.g
        [ Svg.stroke "black"
        ]
        (List.concat
            [ List.concatMap xGrid <| List.range 1 (Tuple.second layout.size - 1)
            , List.concatMap yGrid <| List.range 1 (Tuple.first layout.size - 1)
            ]
        )
    ]



-- Track Layout definitions


trackFill : Track -> String
trackFill track =
    case track.state of
        Just _ ->
            "black"

        Nothing ->
            "none"


viewTracks : List (Svg msg)
viewTracks =
    List.concat <| List.map viewTrack tracks


viewTrack : Track -> List (Svg msg)
viewTrack track =
    case track.direction of
        NS ->
            viewTrackOrthog track

        EW ->
            viewTrackOrthog track

        _ ->
            viewTrackDiag track


viewTrackOrthog : Track -> List (Svg msg)
viewTrackOrthog track =
    let
        rotate =
            case track.direction of
                NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.polyline
        [ Svg.fill (trackFill track)
        , Svg.stroke "black"
        , Svg.points "-30,5 30,5 30,-5 -30,-5 -30,5"
        , Svg.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]


viewTrackDiag : Track -> List (Svg msg)
viewTrackDiag track =
    let
        rotate =
            case track.direction of
                SE ->
                    "rotate(90)"

                SW ->
                    "rotate(180)"

                NW ->
                    "rotate(-90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.polyline
        [ Svg.fill (trackFill track)
        , Svg.stroke "black"
        , Svg.points "5,-30 30,-5 30,5 -5,-30 5,-30"
        , Svg.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]



-- Track Circuit State definitions


tcFill : OneBit -> String
tcFill status =
    case status of
        UNKN ->
            "grey"

        ZERO ->
            "white"

        ONE ->
            "cyan"


viewTrackCircuits : List (Svg msg)
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
                tracks


viewTC : Track -> List (Svg msg)
viewTC track =
    case track.direction of
        NS ->
            viewTCOrtho track <| tcFill <| getOBState track.state

        EW ->
            viewTCOrtho track <| tcFill <| getOBState track.state

        _ ->
            viewTCDiag track <| tcFill <| getOBState track.state


viewTCOrtho : Track -> String -> List (Svg msg)
viewTCOrtho track status =
    let
        rotate =
            case track.direction of
                NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.g
        [ Svg.fill status
        , Svg.stroke status
        , Svg.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        [ Svg.rect
            [ Svg.x "-23"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            ]
            []
        , Svg.rect
            [ Svg.x "7"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            ]
            []
        ]
    ]


viewTCDiag : Track -> String -> List (Svg msg)
viewTCDiag track status =
    let
        rotate =
            case track.direction of
                SE ->
                    "rotate(135)"

                SW ->
                    "rotate(-135)"

                NW ->
                    "rotate(-45)"

                _ ->
                    "rotate(45)"
    in
    [ Svg.rect
        [ Svg.x "-8"
        , Svg.y "-23.25"
        , Svg.width "16"
        , Svg.height "4"
        , Svg.rx "2"
        , Svg.fill status
        , Svg.stroke status
        , Svg.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]



-- Indicator State definitions (Spot detectors) e.g. Stop boards, Limits of Shunt, Platform stops


spotFill : OneBit -> String
spotFill spot =
    case spot of
        UNKN ->
            "grey"

        ZERO ->
            "white"

        ONE ->
            "green"


viewSpots : List (Svg msg)
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
                tracks


viewSpot : Track -> List (Svg msg)
viewSpot track =
    case track.direction of
        NS ->
            viewSpotOrtho track <| spotFill <| getOBState track.spot

        EW ->
            viewSpotOrtho track <| spotFill <| getOBState track.spot

        _ ->
            viewSpotDiag track <| spotFill <| getOBState track.spot


viewSpotOrtho : Track -> String -> List (Svg msg)
viewSpotOrtho track spot =
    let
        rotate =
            case track.direction of
                NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"
    in
    [ Svg.g
        [ Svg.fill spot
        , Svg.stroke spot
        , Svg.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        [ Svg.rect
            [ Svg.x "-10"
            , Svg.y "-12"
            , Svg.width "20"
            , Svg.height "3"
            , Svg.rx "2"
            ]
            []
        ]
    ]


viewSpotDiag : Track -> String -> List (Svg msg)
viewSpotDiag track spot =
    let
        rotate =
            case track.direction of
                SE ->
                    "rotate(135)"

                SW ->
                    "rotate(-135)"

                NW ->
                    "rotate(-45)"

                _ ->
                    "rotate(45)"
    in
    [ Svg.rect
        [ Svg.x "-7"
        , Svg.y "-31"
        , Svg.width "14"
        , Svg.height "3"
        , Svg.rx "2"
        , Svg.fill spot
        , Svg.stroke spot
        , Svg.transform (String.join " " [ Panel.translateTile track.coords, rotate ])
        ]
        []
    ]



-- Turnout Settings definitions
---- Convert double bit state into colours


textAttr : Turnout -> ( Int, Int ) -> List (Attribute msg)
textAttr turnout offset =
    let
        translation =
            "translate(" ++ String.fromInt (Tuple.first offset) ++ ", " ++ String.fromInt (Tuple.second offset) ++ ")"

        rotation =
            case turnout.orientation of
                TONorth ->
                    "rotate(180)"

                TOEast ->
                    "rotate(180)"

                TOSouth ->
                    "rotate(0)"

                TOWest ->
                    "rotate(0)"
    in
    [ Svg.fontFamily "monospace"
    , Svg.fontSize "small"
    , Svg.fill "black"
    , Svg.stroke "none"
    , Svg.x "0"
    , Svg.y "4"
    , Svg.textAnchor "middle"
    , Svg.transform (String.join " " [ translation, rotation ])
    ]


turnoutFill : Turnout -> String
turnoutFill turnout =
    case turnout.state of
        Just _ ->
            "black"

        Nothing ->
            "none"


turnoutRotation : Turnout -> String
turnoutRotation turnout =
    case turnout.orientation of
        TONorth ->
            "rotate(90)"

        TOEast ->
            "rotate(180)"

        TOSouth ->
            "rotate(-90)"

        TOWest ->
            "rotate(0)"


viewTurnouts : List (Svg msg)
viewTurnouts =
    List.concat <| List.map viewTurnout turnouts


viewTurnout : Turnout -> List (Svg msg)
viewTurnout turnout =
    case turnout.hand of
        TOLeft ->
            viewTOLeft turnout

        TORight ->
            viewTORight turnout

        TOWye ->
            viewTOWye turnout


viewTOLeft : Turnout -> List (Svg msg)
viewTOLeft turnout =
    [ Svg.g
        [ Svg.fill (turnoutFill turnout)
        , Svg.stroke "black"
        , Svg.transform (String.join " " [ Panel.translateTile turnout.coords, turnoutRotation turnout ])
        ]
        [ Svg.polyline
            [ Svg.points "-30,5 30,5 30,-5 -30,-5 -30,5"
            ]
            []
        , Svg.polyline
            [ Svg.points "-5,30 5,30 28,7 18,7 -5,30"
            ]
            []
        , Svg.text_
            (textAttr turnout ( 0, -18 ))
            [ Svg.text turnout.name ]
        ]
    ]


viewTORight : Turnout -> List (Svg msg)
viewTORight turnout =
    [ Svg.g
        [ Svg.fill (turnoutFill turnout)
        , Svg.stroke "black"
        , Svg.transform (String.join " " [ Panel.translateTile turnout.coords, turnoutRotation turnout ])
        ]
        [ Svg.polyline
            [ Svg.points "-30,5 30,5 30,-5 -30,-5 -30,5"
            ]
            []
        , Svg.polyline
            [ Svg.points "-5,-30 5,-30 28,-7 18,-7 -5,-30"
            ]
            []
        , Svg.text_
            (textAttr turnout ( 0, 18 ))
            [ Svg.text turnout.name ]
        ]
    ]


viewTOWye : Turnout -> List (Svg msg)
viewTOWye turnout =
    [ Svg.g
        [ Svg.fill (turnoutFill turnout)
        , Svg.stroke "black"
        , Svg.transform (String.join " " [ Panel.translateTile turnout.coords, turnoutRotation turnout ])
        ]
        [ Svg.polyline
            [ Svg.points "0,5 30,5 30,-5 0,-5 0,5"
            ]
            []
        , Svg.polyline
            [ Svg.points "-5,30 5,30 28,7 18,7 -5,30"
            ]
            []
        , Svg.polyline
            [ Svg.points "-5,-30 5,-30 28,-7 18,-7 -5,-30"
            ]
            []
        , Svg.text_
            (textAttr turnout ( -15, 0 ))
            [ Svg.text turnout.name ]
        ]
    ]



-- Lever State definitions


leverFill : TwoBit -> ( String, String )
leverFill double =
    case double of
        ( UNKN, _ ) ->
            ( "grey", "grey" )

        ( _, UNKN ) ->
            ( "grey", "grey" )

        ( ZERO, ZERO ) ->
            ( "none", "none" )

        ( ONE, ZERO ) ->
            ( "white", "none" )

        ( ZERO, ONE ) ->
            ( "none", "white" )

        _ ->
            ( "red", "red" )


viewLevers : List (Svg msg)
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
                turnouts


viewLever : Turnout -> List (Svg msg)
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
            ( getOBState <| getTON turnout.state, getOBState <| getTOR turnout.state )
    in
    case turnout.hand of
        TOLeft ->
            viewLeverLeft turnout (leverFill status) (turnoutRotation turnout)

        TORight ->
            viewLeverRight turnout (leverFill status) (turnoutRotation turnout)

        TOWye ->
            viewLeverWye turnout (leverFill status) (turnoutRotation turnout)


viewLeverLeft : Turnout -> ( String, String ) -> String -> List (Svg msg)
viewLeverLeft turnout status rotation =
    [ Svg.g
        [ Svg.stroke "white"
        , Svg.transform (String.join " " [ Panel.translateTile turnout.coords, rotation ])
        ]
        [ Svg.rect
            [ Svg.x "-23"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ Svg.x "7"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ Svg.x "-4"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.second status)
            , Svg.transform "rotate( 135 )"
            ]
            []
        ]
    ]


viewLeverRight : Turnout -> ( String, String ) -> String -> List (Svg msg)
viewLeverRight turnout status rotation =
    [ Svg.g
        [ Svg.stroke "white"
        , Svg.transform (String.join " " [ Panel.translateTile turnout.coords, rotation ])
        ]
        [ Svg.rect
            [ Svg.x "-23"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ Svg.x "7"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.first status)
            ]
            []
        , Svg.rect
            [ Svg.x "-12"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.second status)
            , Svg.transform "rotate( 45 )"
            ]
            []
        ]
    ]


viewLeverWye : Turnout -> ( String, String ) -> String -> List (Svg msg)
viewLeverWye turnout status rotation =
    [ Svg.g
        [ Svg.stroke "white"
        , Svg.transform (String.join " " [ Panel.translateTile turnout.coords, rotation ])
        ]
        [ Svg.rect
            [ Svg.x "-12"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.first status)
            , Svg.transform "rotate( 45 )"
            ]
            []
        , rect
            [ Svg.x "15"
            , Svg.y "-2"
            , Svg.width "8"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill "white"
            ]
            []
        , rect
            [ Svg.x "-4"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , Svg.rx "2"
            , Svg.fill (Tuple.second status)
            , Svg.transform "rotate( 135 )"
            ]
            []
        ]
    ]



-- Controls


viewControls : List (Svg msg)
viewControls =
    List.concat <| List.map viewControl Model.controls


viewControl : Control -> List (Svg msg)
viewControl control =
    case control.switch of
        Toggle ->
            List.concat [ viewSwBkgd control, viewSwState control ]

        _ ->
            []


viewSwBkgd : Control -> List (Svg msg)
viewSwBkgd switch =
    [ Svg.g
        [ Svg.transform (Panel.translateTile switch.coords)
        ]
        [ Svg.circle
            [ Svg.cx "0"
            , Svg.cy "15"
            , Svg.r "10"
            , Svg.stroke "black"
            , Svg.fill "black"
            ]
            []
        , Svg.circle
            [ Svg.cx "-17"
            , Svg.cy "-20"
            , Svg.r "5"
            , Svg.stroke "black"
            , Svg.fill "none"
            ]
            []
        , Svg.circle
            [ Svg.cx "17"
            , Svg.cy "-20"
            , Svg.r "5"
            , Svg.stroke "black"
            , Svg.fill "none"
            ]
            []
        , Svg.g
            [ Svg.fontFamily "monospace"
            , Svg.fontSize "small"
            ]
            [ Svg.text_
                [ Svg.x "-17"
                , Svg.y "5"
                , Svg.textAnchor "middle"
                ]
                [ Svg.text "N" ]
            , Svg.text_
                [ Svg.x "17"
                , Svg.y "5"
                , Svg.textAnchor "middle"
                ]
                [ Svg.text "R" ]
            , Svg.text_
                [ Svg.x "0"
                , Svg.y "-5"
                , Svg.textAnchor "middle"
                ]
                [ Svg.text switch.name ]
            ]
        ]
    ]


viewSwState : Control -> List (Svg msg)
viewSwState switch =
    List.concat [ viewKnob switch, viewLamps switch ]


viewKnob : Control -> List (Svg msg)
viewKnob switch =
    let
        knobRotate : OneBit -> String
        knobRotate action =
            case action of
                UNKN ->
                    "rotate(180)"

                ZERO ->
                    "rotate(-45)"

                ONE ->
                    "rotate(45)"
    in
    [ Svg.g
        [ Svg.transform (Panel.translateTile switch.coords)
        ]
        [ Svg.polyline
            [ Svg.fill "none"
            , Svg.stroke "white"
            , Svg.strokeLinecap "round"
            , Svg.strokeWidth "0.75"
            , Svg.points "0,-9 5,-2 2,-2 2,8 -2,8 -2,-2 -5,-2 0,-9"
            , Svg.transform (String.join " " [ "translate(0 15)", knobRotate <| getOBState switch.action ])
            ]
            []
        ]
    ]


viewLamps : Control -> List (Svg msg)
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
            leverFill ( getOBState <| getLampN switch.state, getOBState <| getLampR switch.state )
    in
    [ Svg.g
        [ Svg.transform (Panel.translateTile switch.coords)
        ]
        [ Svg.circle
            [ Svg.cx "-17"
            , Svg.cy "-20"
            , Svg.r "4.5"
            , Svg.stroke "white"
            , Svg.fill (Tuple.first status)
            ]
            []
        , Svg.circle
            [ Svg.cx "17"
            , Svg.cy "-20"
            , Svg.r "4.5"
            , Svg.stroke "white"
            , Svg.fill (Tuple.second status)
            ]
            []
        ]
    ]
