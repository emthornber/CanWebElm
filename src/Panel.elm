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
            , viewBox "0 0 800 440"
            ]
            (List.concat [ viewBackground, viewTracks ])
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
        EW ->
            viewTrackEW track

        NE ->
            viewTrackNE track

        SW ->
            viewTrackSW track

        _ ->
            [ svg [] [] ]


viewTrackEW : Track -> List (Svg msg)
viewTrackEW track =
    let
        fillText =
            case track.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (track.x - 1) * 60 + 10

        y =
            (track.y - 1) * 60 + 10 + 25

        coords =
            String.join " "
                [ String.join "," [ String.fromInt x, String.fromInt y ]
                , String.join "," [ String.fromInt (x + 60), String.fromInt y ]
                , String.join "," [ String.fromInt (x + 60), String.fromInt (y + 10) ]
                , String.join "," [ String.fromInt x, String.fromInt (y + 10) ]
                , String.join "," [ String.fromInt x, String.fromInt y ]
                ]
    in
    [ polyline
        [ fill fillText
        , stroke "black"
        , Svg.points coords
        ]
        []
    ]


viewTrackNE : Track -> List (Svg msg)
viewTrackNE track =
    let
        fillText =
            case track.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (track.x - 1) * 60 + 10

        y =
            (track.y - 1) * 60 + 10 + 25

        coords =
            String.join " "
                [ String.join "," [ String.fromInt (x + 60), String.fromInt y ]
                , String.join "," [ String.fromInt (x + 60), String.fromInt (y + 10) ]
                , String.join "," [ String.fromInt (x + 25), String.fromInt (y - 25) ]
                , String.join "," [ String.fromInt (x + 35), String.fromInt (y - 25) ]
                , String.join "," [ String.fromInt (x + 60), String.fromInt y ]
                ]
    in
    [ polyline
        [ fill fillText
        , stroke "black"
        , Svg.points coords
        ]
        []
    ]


viewTrackSW : Track -> List (Svg msg)
viewTrackSW track =
    let
        fillText =
            case track.state of
                Just _ ->
                    "black"

                Nothing ->
                    "none"

        x =
            (track.x - 1) * 60 + 10

        y =
            (track.y - 1) * 60 + 10 + 25

        coords =
            String.join " "
                [ String.join "," [ String.fromInt x, String.fromInt y ]
                , String.join "," [ String.fromInt (x + 35), String.fromInt (y + 35) ]
                , String.join "," [ String.fromInt (x + 25), String.fromInt (y + 35) ]
                , String.join "," [ String.fromInt x, String.fromInt (y + 10) ]
                , String.join "," [ String.fromInt x, String.fromInt y ]
                ]
    in
    [ polyline
        [ fill fillText
        , stroke "black"
        , Svg.points coords
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
    [ Track 1 4 EW (Just "101")

    --    , Track 2 4 EW (Just "101")
    , Track 2 4 EW Nothing
    , Track 3 4 EW (Just "101")
    , Track 5 3 EW (Just "201")
    , Track 6 3 EW (Just "201")
    , Track 7 3 EW (Just "202")
    , Track 8 3 EW (Just "202")
    , Track 9 3 SW (Just "401")
    , Track 9 4 NE (Just "401")
    , Track 10 4 SW (Just "401")
    , Track 5 5 EW (Just "301")
    , Track 6 5 EW (Just "301")
    , Track 7 5 EW (Just "302")
    , Track 8 5 EW (Just "302")
    , Track 9 5 EW (Just "401")
    , Track 11 5 EW (Just "401")
    , Track 12 5 EW (Just "401")
    , Track 13 5 EW (Just "401")
    ]



-- Track Circuit State definitions


viewTCs : List (Svg msg)
viewTCs =
    let
        needTC : Track -> Bool
        needTC track =
            case track.state of
                Just _ ->
                    True

                Nothing ->
                    False

        tcTiles =
            List.filter needTC tracks
    in
    List.concat (List.map viewTC tcTiles)


viewTC : Track -> List (Svg msg)
viewTC track =
    let
        x =
            track.x

        y =
            track.y

        getTC : Maybe String -> Maybe TCState
        getTC state =
            case state of
                Just value ->
                    case Dict.get value trackCircuits of
                        Just tcRecord ->
                            Just tcRecord

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getOB : Maybe TCState -> OneBit
        getOB tcState =
            case tcState of
                Just value ->
                    value.state

                Nothing ->
                    UNKN
    in
    case track.direction of
        EW ->
            viewTcEW x y (getOB (getTC track.state))

        NE ->
            viewTcNE x y (getOB (getTC track.state))

        SW ->
            viewTcSW x y (getOB (getTC track.state))

        _ ->
            [ svg [] [] ]


viewTcEW : Int -> Int -> OneBit -> List (Svg msg)
viewTcEW x y status =
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
            (x - 1) * 60 + 10 + 7

        yPos =
            (y - 1) * 60 + 10 + 25 + 3
    in
    [ rect
        [ Svg.x (String.fromInt xPos)
        , Svg.y (String.fromInt yPos)
        , Svg.width "16"
        , Svg.height "4"
        , rx "2"
        , fill fillText
        , stroke fillText
        ]
        []
    , rect
        [ Svg.x (String.fromInt (xPos + 30))
        , Svg.y (String.fromInt yPos)
        , Svg.width "16"
        , Svg.height "4"
        , rx "2"
        , fill fillText
        , stroke fillText
        ]
        []
    ]


viewTcNE : Int -> Int -> OneBit -> List (Svg msg)
viewTcNE x y status =
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
            (x - 1) * 60 + 10 + 41

        yPos =
            (y - 1) * 60 + 10 + 8

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


type OneBit
    = UNKN
    | ZERO
    | ONE


type alias TCState =
    { name : String, state : OneBit }


type alias TCStateDict =
    Dict String TCState


trackCircuits : TCStateDict
trackCircuits =
    Dict.fromList
        [ ( "101", TCState "N5E3" UNKN )
        , ( "201", TCState "N5E2" ZERO )
        , ( "202", TCState "N6E2" ZERO )
        , ( "301", TCState "N5E1" ONE )
        , ( "302", TCState "N6E2" ONE )
        , ( "401", TCState "N7E3" ZERO )
        ]



-- temporary main code


main =
    view "no model yet"
