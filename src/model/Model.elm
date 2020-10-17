module Model exposing (..)

import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Panel


type Status
    = Loading
    | Loaded


type alias Model =
    { panel : Panel.Diagram
    , cbus : CBUSStateDict
    , sw : List Control
    , tr : List Track
    , to : List Turnout
    }


initialModel : Model
initialModel =
    { panel = Panel.diagram
    , cbus = cbusStates
    , sw = controls
    , tr = tracks
    , to = turnouts
    }



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
    Dict.Dict String CBUSState


getOBState : Maybe String -> OneBit
getOBState name =
    let
        getState : Maybe String -> Maybe CBUSState
        getState value =
            case value of
                Just key ->
                    case Dict.get key cbusStates of
                        Just record ->
                            Just record

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getOneBit : Maybe CBUSState -> OneBit
        getOneBit state =
            case state of
                Just value ->
                    value.state

                Nothing ->
                    UNKN
    in
    getOneBit <| getState name


cbusStates : CBUSStateDict
cbusStates =
    Dict.fromList
        [ ( "TCAA", CBUSState "N5E3" UNKN )
        , ( "TCBA", CBUSState "N5E2" ZERO )
        , ( "TCBB", CBUSState "N6E2" ZERO )
        , ( "TCCA", CBUSState "N5E1" ONE )
        , ( "TCCB", CBUSState "N6E2" ONE )
        , ( "TCDA", CBUSState "N7E3" ZERO )
        , ( "101", CBUSState "N5E5" UNKN )
        , ( "101N", CBUSState "N5E6" UNKN )
        , ( "101R", CBUSState "N5E7" UNKN )
        , ( "102", CBUSState "N6E5" ZERO )
        , ( "102N", CBUSState "N6E6" ONE )
        , ( "102R", CBUSState "N6E7" ZERO )
        , ( "103", CBUSState "N6E5" ONE )
        , ( "103N", CBUSState "N7E6" ZERO )
        , ( "103R", CBUSState "N7E7" ONE )
        , ( "104", CBUSState "N6E5" ONE )
        , ( "104N", CBUSState "N7E6" ZERO )
        , ( "104R", CBUSState "N7E7" ZERO )
        , ( "105", CBUSState "N6E5" ONE )
        , ( "105N", CBUSState "N7E6" ONE )
        , ( "105R", CBUSState "N7E7" ONE )
        ]



-- Controls


type Actuator
    = Toggle
    | PushButton


type alias Control =
    { coords : ( Int, Int ), name : String, switch : Actuator, action : Maybe String, state : Maybe ( String, String ) }


controls : List Control
controls =
    [ Control ( 6, 5 ) "101" Toggle (Just "101") (Just ( "101N", "101R" ))
    , Control ( 7, 5 ) "102" Toggle (Just "102") (Just ( "102N", "102R" ))
    , Control ( 8, 5 ) "103" Toggle (Just "103") (Just ( "103N", "103R" ))
    , Control ( 9, 5 ) "104" Toggle (Just "104") (Just ( "104N", "104R" ))
    , Control ( 10, 5 ) "105" Toggle (Just "105") (Just ( "105N", "105R" ))
    ]



-- Track


type TrackDirection
    = EW
    | NE
    | NS
    | NW
    | SE
    | SW


type alias Track =
    { coords : ( Int, Int ), direction : TrackDirection, state : Maybe String, spot : Maybe String }


tracks : List Track
tracks =
    [ Track ( 1, 5 ) EW Nothing Nothing
    , Track ( 2, 5 ) EW (Just "TCAA") (Just "TCAA")
    , Track ( 3, 5 ) EW (Just "TCBA") (Just "TCBA")
    , Track ( 4, 5 ) EW (Just "TCCA") (Just "TCCA")
    , Track ( 1, 6 ) NS Nothing Nothing
    , Track ( 2, 6 ) NS (Just "TCAA") Nothing
    , Track ( 3, 6 ) NS (Just "TCBA") Nothing
    , Track ( 4, 6 ) NS (Just "TCCA") Nothing
    , Track ( 1, 7 ) NE Nothing Nothing
    , Track ( 1, 7 ) SE Nothing Nothing
    , Track ( 1, 7 ) SW Nothing Nothing
    , Track ( 1, 7 ) NW Nothing (Just "TCBA")
    , Track ( 2, 7 ) NE (Just "TCAA") (Just "TCAA")
    , Track ( 2, 7 ) SE (Just "TCAA") Nothing
    , Track ( 2, 7 ) SW (Just "TCAA") Nothing
    , Track ( 2, 7 ) NW (Just "TCAA") Nothing
    , Track ( 3, 7 ) NE (Just "TCBA") Nothing
    , Track ( 3, 7 ) SE (Just "TCBA") (Just "TCBA")
    , Track ( 3, 7 ) SW (Just "TCBA") Nothing
    , Track ( 3, 7 ) NW (Just "TCBA") Nothing
    , Track ( 4, 7 ) NE (Just "TCCA") Nothing
    , Track ( 4, 7 ) SE (Just "TCCA") Nothing
    , Track ( 4, 7 ) SW (Just "TCCA") (Just "TCCA")
    , Track ( 4, 7 ) NW (Just "TCCA") Nothing
    ]



-- Turnouts


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
    { coords : ( Int, Int ), name : String, hand : TurnoutHand, orientation : TurnoutFacing, state : Maybe ( String, String ) }


turnouts : List Turnout
turnouts =
    [ Turnout ( 1, 1 ) "111" TOLeft TOWest Nothing
    , Turnout ( 2, 1 ) "121" TOLeft TONorth (Just ( "101N", "101R" ))
    , Turnout ( 3, 1 ) "121" TOLeft TOEast (Just ( "102N", "102R" ))
    , Turnout ( 4, 1 ) "121" TOLeft TOSouth (Just ( "103N", "103R" ))
    , Turnout ( 5, 1 ) "121" TOLeft TOWest (Just ( "105N", "105R" ))
    , Turnout ( 1, 2 ) "131" TORight TOWest Nothing
    , Turnout ( 2, 2 ) "131" TORight TONorth (Just ( "101N", "101R" ))
    , Turnout ( 3, 2 ) "131" TORight TOEast (Just ( "102N", "102R" ))
    , Turnout ( 4, 2 ) "131" TORight TOSouth (Just ( "103N", "103R" ))
    , Turnout ( 6, 4 ) "101" TORight TOWest (Just ( "101N", "101R" ))
    , Turnout ( 7, 4 ) "102" TORight TOWest (Just ( "102N", "102R" ))
    , Turnout ( 8, 4 ) "103" TORight TOWest (Just ( "103N", "103R" ))
    , Turnout ( 9, 4 ) "104" TORight TOWest (Just ( "104N", "104R" ))
    , Turnout ( 10, 4 ) "105" TORight TOWest (Just ( "105N", "105R" ))
    , Turnout ( 1, 3 ) "141" TOWye TOWest Nothing
    , Turnout ( 2, 3 ) "141" TOWye TONorth (Just ( "101N", "101R" ))
    , Turnout ( 3, 3 ) "141" TOWye TOEast (Just ( "102N", "102R" ))
    , Turnout ( 4, 3 ) "141" TOWye TOSouth (Just ( "103N", "103R" ))
    , Turnout ( 5, 3 ) "141" TOWye TOWest (Just ( "105N", "105R" ))
    ]
