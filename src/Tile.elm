{-
   Testbed for embedded Svg usage creating Signal Panel tiles

   05 October, 2020 - E M Thornber
   Created
-}


module Tile exposing (main)

import Dict exposing (Dict)
import Html exposing (div, h1, text)
import Html.Attributes as Attr exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)


view model =
    div [ Attr.class "content" ]
        [ h1 [] [ Html.text "CanWeb CBUS Elm Demo" ]
        , svg
            [ Svg.id "tiles"
            , Svg.width "800"
            , Svg.height "440"
            ]
            (List.concat [ viewBackground, viewTracks, viewTrackCircuits, viewSpots, viewTurnouts, viewLevers, viewControls ])
        ]



-- Background Tiles


viewBackground : List (Svg msg)
viewBackground =
    [ rect
        [ x "8"
        , y "8"
        , Svg.width "784"
        , Svg.height "424"
        , rx "0"
        , fill "#000000"
        ]
        []
    , rect
        [ x "10"
        , y "10"
        , Svg.width "780"
        , Svg.height "420"
        , rx "0"
        , fill "#a4b887"
        ]
        []
    , line
        [ x1 "10", y1 "70", x2 "790", y2 "70", stroke "black" ]
        []
    , line
        [ x1 "10", y1 "130", x2 "790", y2 "130", stroke "black" ]
        []
    , line
        [ x1 "10", y1 "190", x2 "790", y2 "190", stroke "black" ]
        []
    , line
        [ x1 "10", y1 "250", x2 "790", y2 "250", stroke "black" ]
        []
    , line
        [ x1 "10", y1 "310", x2 "790", y2 "310", stroke "black" ]
        []
    , line
        [ x1 "10", y1 "370", x2 "790", y2 "370", stroke "black" ]
        []
    , line
        [ x1 "70", y1 "10", x2 "70", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "130", y1 "10", x2 "130", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "190", y1 "10", x2 "190", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "250", y1 "10", x2 "250", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "310", y1 "10", x2 "310", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "370", y1 "10", x2 "370", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "430", y1 "10", x2 "430", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "490", y1 "10", x2 "490", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "550", y1 "10", x2 "550", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "610", y1 "10", x2 "610", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "670", y1 "10", x2 "670", y2 "430", stroke "black" ]
        []
    , line
        [ x1 "730", y1 "10", x2 "730", y2 "430", stroke "black" ]
        []
    ]



-- Track Layout definitions


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
        fillText =
            case track.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (track.x - 1) * 60 + 30 + 10

        y =
            (track.y - 1) * 60 + 30 + 10

        rotMatrix =
            case track.direction of
                NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ polyline
        [ fill fillText
        , stroke "black"
        , Svg.points "-30,5 30,5 30,-5 -30,-5 -30,5"
        , transform xform
        ]
        []
    ]


viewTrackDiag : Track -> List (Svg msg)
viewTrackDiag track =
    let
        fillText =
            case track.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (track.x - 1) * 60 + 30 + 10

        y =
            (track.y - 1) * 60 + 30 + 10

        rotMatrix =
            case track.direction of
                SE ->
                    "rotate(90)"

                SW ->
                    "rotate(180)"

                NW ->
                    "rotate(-90)"

                _ ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ polyline
        [ fill fillText
        , stroke "black"
        , Svg.points "5,-30 30,-5 30,5 -5,-30 5,-30"
        , transform xform
        ]
        []
    ]


type TrackDirection
    = EW
    | NE
    | NS
    | NW
    | SE
    | SW


type alias Track =
    { x : Int, y : Int, direction : TrackDirection, state : Maybe String, spot : Maybe String }


tracks : List Track
tracks =
    [ Track 1 5 EW Nothing Nothing
    , Track 2 5 EW (Just "TCAA") (Just "TCAA")
    , Track 3 5 EW (Just "TCBA") (Just "TCBA")
    , Track 4 5 EW (Just "TCCA") (Just "TCCA")
    , Track 1 6 NS Nothing Nothing
    , Track 2 6 NS (Just "TCAA") Nothing
    , Track 3 6 NS (Just "TCBA") Nothing
    , Track 4 6 NS (Just "TCCA") Nothing
    , Track 1 7 NE Nothing Nothing
    , Track 1 7 SE Nothing Nothing
    , Track 1 7 SW Nothing Nothing
    , Track 1 7 NW Nothing (Just "TCBA")
    , Track 2 7 NE (Just "TCAA") (Just "TCAA")
    , Track 2 7 SE (Just "TCAA") Nothing
    , Track 2 7 SW (Just "TCAA") Nothing
    , Track 2 7 NW (Just "TCAA") Nothing
    , Track 3 7 NE (Just "TCBA") Nothing
    , Track 3 7 SE (Just "TCBA") (Just "TCBA")
    , Track 3 7 SW (Just "TCBA") Nothing
    , Track 3 7 NW (Just "TCBA") Nothing
    , Track 4 7 NE (Just "TCCA") Nothing
    , Track 4 7 SE (Just "TCCA") Nothing
    , Track 4 7 SW (Just "TCCA") (Just "TCCA")
    , Track 4 7 NW (Just "TCCA") Nothing
    ]



-- Track Circuit State definitions


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
    let
        getTC : Maybe String -> Maybe CBUSState
        getTC state =
            case state of
                Just value ->
                    case Dict.get value cbusStates of
                        Just tcRecord ->
                            Just tcRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getOB : Maybe CBUSState -> OneBit
        getOB tcState =
            case tcState of
                Just value ->
                    value.state

                Nothing ->
                    UNKN
    in
    case track.direction of
        NS ->
            viewTCOrtho track <| getOB <| getTC track.state

        EW ->
            viewTCOrtho track <| getOB <| getTC track.state

        _ ->
            viewTCDiag track <| getOB <| getTC track.state


viewTCOrtho : Track -> OneBit -> List (Svg msg)
viewTCOrtho track status =
    let
        fillText =
            case status of
                UNKN ->
                    "grey"

                ZERO ->
                    "white"

                ONE ->
                    "cyan"

        x =
            (track.x - 1) * 60 + 30 + 10

        y =
            (track.y - 1) * 60 + 30 + 10

        rotMatrix =
            case track.direction of
                NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ fill fillText
        , stroke fillText
        , transform xform
        ]
        [ rect
            [ Svg.x "-23"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            ]
            []
        , rect
            [ Svg.x "7"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            ]
            []
        ]
    ]


viewTCDiag : Track -> OneBit -> List (Svg msg)
viewTCDiag track status =
    let
        fillText =
            case status of
                UNKN ->
                    "grey"

                ZERO ->
                    "white"

                ONE ->
                    "cyan"

        x =
            (track.x - 1) * 60 + 30 + 10

        y =
            (track.y - 1) * 60 + 30 + 10

        rotMatrix =
            case track.direction of
                SE ->
                    "rotate(135)"

                SW ->
                    "rotate(-135)"

                NW ->
                    "rotate(-45)"

                _ ->
                    "rotate(45)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ rect
        [ Svg.x "-8"
        , Svg.y "-23.25"
        , Svg.width "16"
        , Svg.height "4"
        , rx "2"
        , fill fillText
        , stroke fillText
        , transform xform
        ]
        []
    ]



-- Indicator State definitions (Spot detectors) e.g. Stop boards, Limits of Shunt, Platform stops


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
    let
        getSpot : Maybe String -> Maybe CBUSState
        getSpot state =
            case state of
                Just value ->
                    case Dict.get value cbusStates of
                        Just spRecord ->
                            Just spRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getOB : Maybe CBUSState -> OneBit
        getOB spState =
            case spState of
                Just value ->
                    value.state

                Nothing ->
                    UNKN
    in
    case track.direction of
        NS ->
            viewSpotOrtho track <| getOB <| getSpot track.spot

        EW ->
            viewSpotOrtho track <| getOB <| getSpot track.spot

        _ ->
            viewSpotDiag track <| getOB <| getSpot track.spot


viewSpotOrtho : Track -> OneBit -> List (Svg msg)
viewSpotOrtho track spot =
    let
        fillText =
            case spot of
                UNKN ->
                    "grey"

                ZERO ->
                    "white"

                ONE ->
                    "green"

        x =
            (track.x - 1) * 60 + 30 + 10

        y =
            (track.y - 1) * 60 + 30 + 10

        rotMatrix =
            case track.direction of
                NS ->
                    "rotate(90)"

                _ ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ fill fillText
        , stroke fillText
        , transform xform
        ]
        [ rect
            [ Svg.x "-10"
            , Svg.y "-12"
            , Svg.width "20"
            , Svg.height "3"
            , rx "2"
            ]
            []
        ]
    ]


viewSpotDiag : Track -> OneBit -> List (Svg msg)
viewSpotDiag track spot =
    let
        fillText =
            case spot of
                UNKN ->
                    "grey"

                ZERO ->
                    "white"

                ONE ->
                    "green"

        x =
            (track.x - 1) * 60 + 30 + 10

        y =
            (track.y - 1) * 60 + 30 + 10

        rotMatrix =
            case track.direction of
                SE ->
                    "rotate(135)"

                SW ->
                    "rotate(-135)"

                NW ->
                    "rotate(-45)"

                _ ->
                    "rotate(45)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ rect
        [ Svg.x "-7"
        , Svg.y "-31"
        , Svg.width "14"
        , Svg.height "3"
        , rx "2"
        , fill fillText
        , stroke fillText
        , transform xform
        ]
        []
    ]



-- Turnout Layout definitions


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
    let
        fillText =
            case turnout.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (turnout.x - 1) * 60 + 30 + 10

        y =
            (turnout.y - 1) * 60 + 30 + 10

        rotMatrix =
            case turnout.orientation of
                TONorth ->
                    "rotate(90)"

                TOEast ->
                    "rotate(180)"

                TOSouth ->
                    "rotate(-90)"

                TOWest ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ fill fillText
        , stroke "black"
        , transform xform
        ]
        [ polyline
            [ Svg.points "-30,5 30,5 30,-5 -30,-5 -30,5"
            ]
            []
        , polyline
            [ Svg.points "-5,30 5,30 28,7 18,7 -5,30"
            ]
            []
        ]
    ]


viewTORight : Turnout -> List (Svg msg)
viewTORight turnout =
    let
        fillText =
            case turnout.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (turnout.x - 1) * 60 + 30 + 10

        y =
            (turnout.y - 1) * 60 + 30 + 10

        rotMatrix =
            case turnout.orientation of
                TONorth ->
                    "rotate(90)"

                TOEast ->
                    "rotate(180)"

                TOSouth ->
                    "rotate(-90)"

                TOWest ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ fill fillText
        , stroke "black"
        , transform xform
        ]
        [ polyline
            [ Svg.points "-30,5 30,5 30,-5 -30,-5 -30,5"
            ]
            []
        , polyline
            [ Svg.points "-5,-30 5,-30 28,-7 18,-7 -5,-30"
            ]
            []
        ]
    ]


viewTOWye : Turnout -> List (Svg msg)
viewTOWye turnout =
    let
        fillText =
            case turnout.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (turnout.x - 1) * 60 + 30 + 10

        y =
            (turnout.y - 1) * 60 + 30 + 10

        rotMatrix =
            case turnout.orientation of
                TONorth ->
                    "rotate(90)"

                TOEast ->
                    "rotate(180)"

                TOSouth ->
                    "rotate(-90)"

                TOWest ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ fill fillText
        , stroke "black"
        , transform xform
        ]
        [ polyline
            [ Svg.points "0,5 30,5 30,-5 0,-5 0,5"
            ]
            []
        , polyline
            [ Svg.points "-5,30 5,30 28,7 18,7 -5,30"
            ]
            []
        , polyline
            [ Svg.points "-5,-30 5,-30 28,-7 18,-7 -5,-30"
            ]
            []
        ]
    ]



-- Lever State definitions


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
        getTON : Maybe ( String, String ) -> Maybe CBUSState
        getTON state =
            case state of
                Just value ->
                    case Dict.get (Tuple.first value) cbusStates of
                        Just toRecord ->
                            Just toRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getTOR : Maybe ( String, String ) -> Maybe CBUSState
        getTOR state =
            case state of
                Just value ->
                    case Dict.get (Tuple.second value) cbusStates of
                        Just toRecord ->
                            Just toRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getOB : Maybe CBUSState -> OneBit
        getOB toState =
            case toState of
                Just value ->
                    value.state

                Nothing ->
                    UNKN

        status =
            ( getOB <| getTON turnout.state, getOB <| getTOR turnout.state )
    in
    case turnout.hand of
        TOLeft ->
            viewLeverLeft turnout <| getTOFill status

        TORight ->
            viewLeverRight turnout <| getTOFill status

        TOWye ->
            viewLeverWye turnout <| getTOFill status


getTOFill : TwoBit -> ( String, String )
getTOFill double =
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


viewLeverLeft : Turnout -> ( String, String ) -> List (Svg msg)
viewLeverLeft turnout status =
    let
        x =
            (turnout.x - 1) * 60 + 30 + 10

        y =
            (turnout.y - 1) * 60 + 30 + 10

        rotMatrix =
            case turnout.orientation of
                TONorth ->
                    "rotate(90)"

                TOEast ->
                    "rotate(180)"

                TOSouth ->
                    "rotate(-90)"

                TOWest ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ stroke "white"
        , transform xform
        ]
        [ rect
            [ Svg.x "-23"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.first status)
            ]
            []
        , rect
            [ Svg.x "7"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.first status)
            ]
            []
        , rect
            [ Svg.x "-4"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.second status)
            , transform "rotate( 135 )"
            ]
            []
        ]
    ]


viewLeverRight : Turnout -> ( String, String ) -> List (Svg msg)
viewLeverRight turnout status =
    let
        x =
            (turnout.x - 1) * 60 + 30 + 10

        y =
            (turnout.y - 1) * 60 + 30 + 10

        rotMatrix =
            case turnout.orientation of
                TONorth ->
                    "rotate(90)"

                TOEast ->
                    "rotate(180)"

                TOSouth ->
                    "rotate(-90)"

                TOWest ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ stroke "white"
        , transform xform
        ]
        [ rect
            [ Svg.x "-23"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.first status)
            ]
            []
        , rect
            [ Svg.x "7"
            , Svg.y "-2"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.first status)
            ]
            []
        , rect
            [ Svg.x "-12"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.second status)
            , transform "rotate( 45 )"
            ]
            []
        ]
    ]


viewLeverWye : Turnout -> ( String, String ) -> List (Svg msg)
viewLeverWye turnout status =
    let
        x =
            (turnout.x - 1) * 60 + 30 + 10

        y =
            (turnout.y - 1) * 60 + 30 + 10

        rotMatrix =
            case turnout.orientation of
                TONorth ->
                    "rotate(90)"

                TOEast ->
                    "rotate(180)"

                TOSouth ->
                    "rotate(-90)"

                TOWest ->
                    "rotate(0)"

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")", rotMatrix ]
    in
    [ g
        [ stroke "white"
        , transform xform
        ]
        [ rect
            [ Svg.x "-12"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.first status)
            , transform "rotate( 45 )"
            ]
            []
        , rect
            [ Svg.x "15"
            , Svg.y "-2"
            , Svg.width "8"
            , Svg.height "4"
            , rx "2"
            , fill "white"
            ]
            []
        , rect
            [ Svg.x "-4"
            , Svg.y "-23.25"
            , Svg.width "16"
            , Svg.height "4"
            , rx "2"
            , fill (Tuple.second status)
            , transform "rotate( 135 )"
            ]
            []
        ]
    ]


type TurnoutHand
    = TOLeft
    | TORight
    | TOWye


type TurnoutFacing
    = TONorth
    | TOEast
    | TOSouth
    | TOWest


type alias Turnout =
    { x : Int, y : Int, hand : TurnoutHand, orientation : TurnoutFacing, state : Maybe ( String, String ) }


turnouts : List Turnout
turnouts =
    [ Turnout 1 1 TOLeft TOWest Nothing
    , Turnout 2 1 TOLeft TONorth (Just ( "101N", "101R" ))
    , Turnout 3 1 TOLeft TOEast (Just ( "101N", "102R" ))
    , Turnout 4 1 TOLeft TOSouth (Just ( "101N", "103R" ))
    , Turnout 1 2 TORight TOWest Nothing
    , Turnout 2 2 TORight TONorth (Just ( "102N", "101R" ))
    , Turnout 3 2 TORight TOEast (Just ( "102N", "102R" ))
    , Turnout 4 2 TORight TOSouth (Just ( "102N", "103R" ))
    , Turnout 5 2 TORight TOWest (Just ( "101N", "101R" ))
    , Turnout 6 2 TORight TOWest (Just ( "103N", "102R" ))
    , Turnout 7 2 TORight TOWest (Just ( "102N", "103R" ))
    , Turnout 8 2 TORight TOWest (Just ( "103N", "103R" ))
    , Turnout 1 3 TOWye TOWest Nothing
    , Turnout 2 3 TOWye TONorth (Just ( "103N", "101R" ))
    , Turnout 3 3 TOWye TOEast (Just ( "103N", "102R" ))
    , Turnout 4 3 TOWye TOSouth (Just ( "103N", "103R" ))
    ]



-- Controls


viewControls : List (Svg msg)
viewControls =
    List.concat <| List.map viewControl controls


viewControl : Control -> List (Svg msg)
viewControl control =
    List.concat [ viewSwBkgd control, viewSwState control ]


viewSwBkgd : Control -> List (Svg msg)
viewSwBkgd switch =
    let
        x =
            (switch.x - 1) * 60 + 30 + 10

        y =
            (switch.y - 1) * 60 + 30 + 10

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")" ]
    in
    [ g
        [ transform xform
        ]
        [ circle
            [ cx "0"
            , cy "15"
            , r "10"
            , stroke "black"
            , fill "black"
            ]
            []
        , circle
            [ cx "-17"
            , cy "-20"
            , r "5"
            , stroke "black"
            , fill "none"
            ]
            []
        , circle
            [ cx "17"
            , cy "-20"
            , r "5"
            , stroke "black"
            , fill "none"
            ]
            []
        , g
            [ fontFamily "monospace"
            , fontSize "small"
            ]
            [ Svg.text_
                [ Svg.x "-17"
                , Svg.y "5"
                , textAnchor "middle"
                ]
                [ Svg.text "N" ]
            , Svg.text_
                [ Svg.x "17"
                , Svg.y "5"
                , textAnchor "middle"
                ]
                [ Svg.text "R" ]
            , Svg.text_
                [ Svg.x "0"
                , Svg.y "-5"
                , textAnchor "middle"
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
        getKnob : Maybe String -> Maybe CBUSState
        getKnob action =
            case action of
                Just value ->
                    case Dict.get value cbusStates of
                        Just spRecord ->
                            Just spRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getOB : Maybe CBUSState -> OneBit
        getOB spState =
            case spState of
                Just value ->
                    value.state

                Nothing ->
                    UNKN

        knobMatrix : OneBit -> String
        knobMatrix action =
            case action of
                UNKN ->
                    "rotate(180)"

                ZERO ->
                    "rotate(-45)"

                ONE ->
                    "rotate(45)"

        rotMatrix =
            knobMatrix <| getOB <| getKnob switch.action

        fillText =
            case switch.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (switch.x - 1) * 60 + 30 + 10

        y =
            (switch.y - 1) * 60 + 30 + 10

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")" ]
    in
    [ g
        [ transform xform
        ]
        [ polyline
            [ fill "none"
            , stroke "white"
            , strokeLinecap "round"
            , strokeWidth "0.75"
            , Svg.points "0,-9 5,-2 2,-2 2,8 -2,8 -2,-2 -5,-2 0,-9"
            , transform (String.join " " [ "translate(0 15)", rotMatrix ])
            ]
            []
        ]
    ]


viewLamps : Control -> List (Svg msg)
viewLamps switch =
    let
        getLampN : Maybe ( String, String ) -> Maybe CBUSState
        getLampN state =
            case state of
                Just value ->
                    case Dict.get (Tuple.first value) cbusStates of
                        Just lpRecord ->
                            Just lpRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getLampR : Maybe ( String, String ) -> Maybe CBUSState
        getLampR state =
            case state of
                Just value ->
                    case Dict.get (Tuple.second value) cbusStates of
                        Just lpRecord ->
                            Just lpRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getOB : Maybe CBUSState -> OneBit
        getOB spState =
            case spState of
                Just value ->
                    value.state

                Nothing ->
                    UNKN

        status =
            getTOFill ( getOB <| getLampN switch.state, getOB <| getLampR switch.state )

        x =
            (switch.x - 1) * 60 + 30 + 10

        y =
            (switch.y - 1) * 60 + 30 + 10

        xform =
            String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")" ]
    in
    [ g
        [ transform xform
        ]
        [ polyline
            [ fill "none"
            , stroke "white"
            , strokeLinecap "round"
            , strokeWidth "0.75"
            , Svg.points "0,-9 5,-2 2,-2 2,8 -2,8 -2,-2 -5,-2 0,-9"

            --            , transform (String.join " " [ "matrix(", rotMatrix, "0 15 )" ])
            ]
            []
        ]
    ]


type alias Control =
    { x : Int, y : Int, name : String, action : Maybe String, state : Maybe ( String, String ) }


controls : List Control
controls =
    [ Control 6 5 "102" (Just "102") (Just ( "102N", "102R" ))
    , Control 7 5 "103" (Just "103") (Just ( "103N", "103R" ))
    , Control 8 5 "104" (Just "104") (Just ( "104N", "104R" ))
    ]



-- CBUS states


type OneBit
    = UNKN
    | ZERO
    | ONE


type alias TwoBit =
    ( OneBit, OneBit )


type alias CBUSState =
    { event : String, state : OneBit }


type alias CBUSStateDict =
    Dict String CBUSState


cbusStates : CBUSStateDict
cbusStates =
    Dict.fromList
        [ ( "TCAA", CBUSState "N5E3" UNKN )
        , ( "TCBA", CBUSState "N5E2" ZERO )
        , ( "TCBB", CBUSState "N6E2" ZERO )
        , ( "TCCA", CBUSState "N5E1" ONE )
        , ( "TCCB", CBUSState "N6E2" ONE )
        , ( "TCDA", CBUSState "N7E3" ZERO )
        , ( "101N", CBUSState "N5E6" UNKN )
        , ( "101R", CBUSState "N5E7" UNKN )
        , ( "102", CBUSState "N6E5" UNKN )
        , ( "102N", CBUSState "N6E6" ONE )
        , ( "102R", CBUSState "N6E7" ZERO )
        , ( "103", CBUSState "N6E5" ZERO )
        , ( "103N", CBUSState "N7E6" ZERO )
        , ( "103R", CBUSState "N7E7" ONE )
        , ( "104", CBUSState "N6E5" ONE )
        , ( "104N", CBUSState "N7E6" ZERO )
        , ( "104R", CBUSState "N7E7" ZERO )
        ]



-- temporary main code


main =
    view "no model yet"
