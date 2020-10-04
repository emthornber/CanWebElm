module Panel exposing (main)

import Dict exposing (Dict)
import Html exposing (div, h1, text)
import Html.Attributes as Attr exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)


view model =
    div [ Attr.class "content" ]
        [ h1 [] [ Html.text "CAG CBUS Demo" ]
        , svg
            [ Svg.id "tiles"
            , Svg.width "800"
            , Svg.height "440"

            --            , viewBox "0 0 800 440"
            ]
            (List.concat [ viewBackground, viewTracks, viewTurnouts ])
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



-- Track Layout definition


viewTracks : List (Svg msg)
viewTracks =
    List.concat (List.map viewTrackTile tracks)


viewTrackTile : Track -> List (Svg msg)
viewTrackTile track =
    case track.state of
        Just _ ->
            List.concat [ viewTrack track, viewTC track ]

        Nothing ->
            viewTrack track


viewTrack : Track -> List (Svg msg)
viewTrack track =
    case track.direction of
        NS ->
            viewTrackOrthog track

        EW ->
            viewTrackOrthog track

        NE ->
            viewTrackDiag track

        SE ->
            viewTrackDiag track

        SW ->
            viewTrackDiag track

        NW ->
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

        coords =
            String.join " "
                [ String.join "," [ String.fromInt -30, String.fromInt 5 ]
                , String.join "," [ String.fromInt 30, String.fromInt 5 ]
                , String.join "," [ String.fromInt 30, String.fromInt -5 ]
                , String.join "," [ String.fromInt -30, String.fromInt -5 ]
                , String.join "," [ String.fromInt -30, String.fromInt 5 ]
                ]

        rotMatrix =
            case track.direction of
                NS ->
                    "0 1 -1 0"

                _ ->
                    "1 0 0 1"

        xform =
            String.join " " [ "matrix(", rotMatrix, String.fromInt x, String.fromInt y, ")" ]
    in
    [ polyline
        [ fill fillText
        , stroke "black"
        , Svg.points coords
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

        coords =
            String.join " "
                [ String.join "," [ String.fromInt 5, String.fromInt -30 ]
                , String.join "," [ String.fromInt 30, String.fromInt -5 ]
                , String.join "," [ String.fromInt 30, String.fromInt 5 ]
                , String.join "," [ String.fromInt -5, String.fromInt -30 ]
                , String.join "," [ String.fromInt 5, String.fromInt -30 ]
                ]

        rotMatrix =
            case track.direction of
                SE ->
                    "0 1 -1 0"

                SW ->
                    "-1 0 0 -1"

                NW ->
                    "0 -1 1 0"

                _ ->
                    "1 0 0 1"

        xform =
            String.join " " [ "matrix(", rotMatrix, String.fromInt x, String.fromInt y, ")" ]
    in
    [ polyline
        [ fill fillText
        , stroke "black"
        , Svg.points coords
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
    { x : Int, y : Int, direction : TrackDirection, state : Maybe String }


tracks : List Track
tracks =
    [ Track 1 4 EW Nothing
    , Track 2 4 EW (Just "TCAA")
    , Track 3 4 EW (Just "TCAA")
    , Track 4 2 NS Nothing
    , Track 4 3 NS (Just "TCBA")
    , Track 7 3 EW (Just "TCBB")
    , Track 8 3 EW (Just "TCBB")
    , Track 9 3 SW (Just "TCDA")
    , Track 9 4 NE (Just "TCDA")
    , Track 10 4 SW (Just "TCDA")
    , Track 5 5 EW (Just "TCCA")
    , Track 6 5 EW (Just "TCCA")
    , Track 7 5 EW (Just "TCCB")
    , Track 8 5 EW (Just "TCCB")
    , Track 9 5 EW (Just "TCDA")
    , Track 11 5 EW (Just "TCDA")
    , Track 12 5 EW (Just "TCDA")
    , Track 13 5 EW (Just "TCDA")
    ]



-- Track Circuit State definitions


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
            viewTCOrthog track (getOB (getTC track.state))

        EW ->
            viewTCOrthog track (getOB (getTC track.state))

        NE ->
            viewTCDiag track (getOB (getTC track.state))

        SE ->
            viewTCDiag track (getOB (getTC track.state))

        SW ->
            viewTCDiag track (getOB (getTC track.state))

        NW ->
            viewTCDiag track (getOB (getTC track.state))


viewTCOrthog : Track -> OneBit -> List (Svg msg)
viewTCOrthog track status =
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
                    "0 1 -1 0"

                _ ->
                    "1 0 0 1"

        xform =
            String.join " " [ "matrix(", rotMatrix, String.fromInt x, String.fromInt y, ")" ]
    in
    [ rect
        [ Svg.x "-23"
        , Svg.y "-2"
        , Svg.width "16"
        , Svg.height "4"
        , rx "2"
        , fill fillText
        , stroke fillText
        , transform xform
        ]
        []
    , rect
        [ Svg.x "7"
        , Svg.y "-2"
        , Svg.width "16"
        , Svg.height "4"
        , rx "2"
        , fill fillText
        , stroke fillText
        , transform xform
        ]
        []
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
                    "red"

        x =
            (track.x - 1) * 60 + 30 + 10

        y =
            (track.y - 1) * 60 + 30 + 10

        rotMatrix =
            case track.direction of
                SE ->
                    "-0.707 0.707 -0.707 -0.707"

                SW ->
                    "-0.707 -0.707 0.707 -0.707"

                NW ->
                    "0.707 -0.707 0.707 0.707"

                _ ->
                    "0.707 0.707 -0.707 0.707"

        xform =
            String.join " " [ "matrix(", rotMatrix, String.fromInt x, String.fromInt y, ")" ]
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


viewTcSW : Int -> Int -> OneBit -> List (Svg msg)
viewTcSW x y status =
    let
        fillText =
            case status of
                UNKN ->
                    "grey"

                ZERO ->
                    "white"

                ONE ->
                    "red"

        xPos =
            (x - 1) * 60 + 10 + 11

        yPos =
            (y - 1) * 60 + 10 + 25 + 13

        xform =
            String.join " " [ "matrix(0.7071 0.7071 -0.7071 0.7071", String.fromInt xPos, String.fromInt yPos, ")" ]
    in
    [ rect
        [ Svg.x "0"
        , Svg.y "0"
        , Svg.width "16"
        , Svg.height "4"
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
    List.concat (List.map viewTurnoutTile turnouts)


viewTurnoutTile : Turnout -> List (Svg msg)
viewTurnoutTile turnout =
    case turnout.state of
        Just _ ->
            List.concat [ viewTurnout turnout, viewLever turnout ]

        Nothing ->
            viewTurnout turnout


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
                    "0 1 -1 0"

                TOEast ->
                    "-1 0 0 -1"

                TOSouth ->
                    "0 -1 1 0"

                TOWest ->
                    "1 0 0 1"

        xform =
            String.join " " [ "matrix(", rotMatrix, String.fromInt x, String.fromInt y, ")" ]
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
                    "0 1 -1 0"

                TOEast ->
                    "-1 0 0 -1"

                TOSouth ->
                    "0 -1 1 0"

                TOWest ->
                    "1 0 0 1"

        xform =
            String.join " " [ "matrix(", rotMatrix, String.fromInt x, String.fromInt y, ")" ]
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
                    "0 1 -1 0"

                TOEast ->
                    "-1 0 0 -1"

                TOSouth ->
                    "0 -1 1 0"

                TOWest ->
                    "1 0 0 1"

        xform =
            String.join " " [ "matrix(", rotMatrix, String.fromInt x, String.fromInt y, ")" ]
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


viewLever : Turnout -> List (Svg msg)
viewLever turnout =
    case turnout.hand of
        TOLeft ->
            viewLeverLeft turnout

        TORight ->
            viewLeverRight turnout

        TOWye ->
            viewLeverWye turnout


viewLeverLeft : Turnout -> List (Svg msg)
viewLeverLeft turnout =
    let
        getTO : Maybe String -> Maybe CBUSState
        getTO state =
            case state of
                Just value ->
                    case Dict.get value levers of
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
                    TOUnkn
    in
    case turnout.direction of
        TO ->
            viewTCOrthog track (getOB (getTO track.state))

        EW ->
            viewTCOrthog track (getOB (getTO track.state))

        NE ->
            viewTCDiag track (getOB (getTO track.state))

        SE ->
            viewTCDiag track (getOB (getTO track.state))

        SW ->
            viewTCDiag track (getOB (getTO track.state))

        NW ->
            viewTCDiag track (getOB (getTO track.state))


getTurnoutState : Turnout -> TurnoutState
getTurnoutState turnout =
    TOUnkn


viewLeverRight : Turnout -> List (Svg msg)
viewLeverRight turnout =
    [ svg [] [] ]


viewLeverWye : Turnout -> List (Svg msg)
viewLeverWye turnout =
    [ svg [] [] ]


type TurnoutHand
    = TOLeft
    | TORight
    | TOWye


type TurnoutFacing
    = TONorth
    | TOEast
    | TOSouth
    | TOWest


type TurnoutState
    = TOUnkn
    | TOInTransit
    | TONormal
    | TOReverse
    | TOError


type alias Turnout =
    { x : Int, y : Int, hand : TurnoutHand, orientation : TurnoutFacing, state : Maybe ( String, String ) }


turnouts : List Turnout
turnouts =
    [ Turnout 1 1 TOLeft TONorth (Just ( "101N", "101R" ))
    , Turnout 1 2 TOLeft TOEast (Just ( "101N", "101R" ))
    , Turnout 1 3 TOLeft TOSouth (Just ( "101N", "101R" ))
    , Turnout 1 4 TOLeft TOWest (Just ( "101N", "101R" ))
    , Turnout 2 1 TORight TONorth (Just ( "102N", "102R" ))
    , Turnout 2 2 TORight TOEast (Just ( "102N", "102R" ))
    , Turnout 2 3 TORight TOSouth (Just ( "102N", "102R" ))
    , Turnout 2 4 TORight TOWest (Just ( "102N", "102R" ))
    , Turnout 3 1 TOWye TONorth (Just ( "103N", "103R" ))
    , Turnout 3 2 TOWye TOEast (Just ( "103N", "103R" ))
    , Turnout 3 3 TOWye TOSouth (Just ( "103N", "103R" ))
    , Turnout 3 4 TOWye TOWest (Just ( "103N", "103R" ))
    ]


type alias LeverState =
    { name : String, state : ( OneBit, OneBit ) }


type alias LeverStateDict =
    Dict String LeverState


levers : LeverStateDict
levers =
    Dict.fromList



-- CBUS states


type OneBit
    = UNKN
    | ZERO
    | ONE


type alias CBUSState =
    { name : String, state : OneBit }


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
        , ( "102N", CBUSState "N6E6" ZERO )
        , ( "102R", CBUSState "N6E7" ONE )
        , ( "103N", CBUSState "N7E6" ONE )
        , ( "103R", CBUSState "N7E7" ONE )
        ]



-- temporary main code


main =
    view "no model yet"
