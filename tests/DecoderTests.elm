module DecoderTests exposing (..)

-- tests for the decoder functions

import Dict
import Expect exposing (Expectation)
import Json.Decode as Decode
import LayoutJson
import Model
import Panel
import Test exposing (..)


panelTest : Test
panelTest =
    test "Decode Diagram contents from Json" <|
        \() ->
            let
                input =
                    """
                    { "width": 11,
                    "height": 5,
                    "tilesize": 60,
                    "colour": "#a4b887",
                    "margins": 10,
                    "border": 2,
                    "title": "CAG CBUS Demo (Elm)"
                    }
                    """

                decodedOutput : String -> Result Decode.Error Panel.Diagram
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.decodeDiagram
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Panel.Diagram 11 5 60 "#a4b887" 10 2 "CAG CBUS Demo (Elm)")
                )


cbusstateTest1 : Test
cbusstateTest1 =
    test "Decode CBUSState contents from Json" <|
        \() ->
            let
                input =
                    """
                    { 
                        "event": "N5E3",
                        "state": "UNKN"
                    }
                    """

                decodedOutput : String -> Result Decode.Error Model.CBUSState
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.cbusStateDecoder
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Model.CBUSState (Just "N5E3") Model.UNKN)
                )


cbusstateTest2 : Test
cbusstateTest2 =
    test "Decode CBUSState all contents from Json" <|
        \() ->
            let
                input =
                    """
                    { 
                        "state": "UNKN"
                    }
                    """

                decodedOutput : String -> Result Decode.Error Model.CBUSState
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.cbusStateDecoder
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Model.CBUSState Nothing Model.UNKN)
                )


cbusstatedictTest : Test
cbusstatedictTest =
    test "Decode CBUSStateDict just state contents from Json" <|
        \() ->
            let
                input =
                    """
                    [
                        { "name": "TCAA", 
                        "event": "N5E3",
                        "state": "UNKN"
                        },
                        { "name": "TCBA", 
                        "event": "N5E2",
                        "state": "ZERO"
                        }
                    ]
                    """

                decodedOutput : String -> Result Decode.Error Model.CBUSStateDict
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.decodeCbus
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Dict.fromList [ ( "TCAA", Model.CBUSState (Just "N5E3") Model.UNKN ), ( "TCBA", Model.CBUSState (Just "N5E2") Model.ZERO ) ])
                )


controlTest : Test
controlTest =
    test "Decode Control contents from Json" <|
        \() ->
            let
                input =
                    """
                    {
                        "tile": {
                            "x-coord": 3,
                            "y-coord": 1
                        },
                        "name": "101",
                        "switch": "Toggle",
                        "action": "101",
                        "tostate": {
                            "normal": "101N",
                            "reverse": "101R"
                        }
                    }
                    """

                decodedOutput : String -> Result Decode.Error Model.Control
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.decodeControl
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Model.Control ( 3, 1 ) "101" Model.Toggle (Just "101") (Just ( "101N", "101R" )))
                )


trackFullTest : Test
trackFullTest =
    test "Decode full Track contents from Json" <|
        \() ->
            let
                input =
                    """
                    {
                        "tile": {
                            "x-coord": 1,
                            "y-coord": 3
                        },
                        "direction": "EW",
                        "tcstate": "TCAA",
                        "spot": "TCAA"
                    }
                    """

                decodedOutput : String -> Result Decode.Error Model.Track
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.decodeTrack
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Model.Track ( 1, 3 ) Model.EW (Just "TCAA") (Just "TCAA"))
                )


trackPartialTest : Test
trackPartialTest =
    test "Decode partial Track contents from Json" <|
        \() ->
            let
                input =
                    """
                    {
                        "tile": {
                            "x-coord": 1,
                            "y-coord": 3
                        },
                        "direction": "EW"
                    }
                    """

                decodedOutput : String -> Result Decode.Error Model.Track
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.decodeTrack
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Model.Track ( 1, 3 ) Model.EW Nothing Nothing)
                )


turnoutFullTest : Test
turnoutFullTest =
    test "Decode full Turnout contents from Json" <|
        \() ->
            let
                input =
                    """
                    {
                        "tile": {
                            "x-coord": 3,
                            "y-coord": 3
                        },
                        "name": "101",
                        "hand": "TOWye",
                        "orientation": "TOEast",
                        "tostate": {
                            "normal": "101N",
                            "reverse": "101R"
                        }
                    }
                    """

                decodedOutput : String -> Result Decode.Error Model.Turnout
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.decodeTurnout
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Model.Turnout ( 3, 3 ) "101" Model.TOWye Model.TOEast (Just ( "101N", "101R" )))
                )


turnoutPartialTest : Test
turnoutPartialTest =
    test "Decode partial Turnout contents from Json" <|
        \() ->
            let
                input =
                    """
                    {
                        "tile": {
                            "x-coord": 3,
                            "y-coord": 2
                        },
                        "name": "OOU",
                        "hand": "TOWye",
                        "orientation": "TOWest"
                    }
                    """

                decodedOutput : String -> Result Decode.Error Model.Turnout
                decodedOutput json =
                    Decode.decodeString
                        LayoutJson.decodeTurnout
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    (Model.Turnout ( 3, 2 ) "OOU" Model.TOWye Model.TOWest Nothing)
                )


turnoutListTest : Test
turnoutListTest =
    test "Decode list of Turnout contents from Json" <|
        \() ->
            let
                input =
                    """
                    [{
                        "tile": {
                            "x-coord": 3,
                            "y-coord": 3
                        },
                        "name": "101",
                        "hand": "TOWye",
                        "orientation": "TOEast",
                        "tostate": {
                            "normal": "101N",
                            "reverse": "101R"
                        }
                    },
                    {
                        "tile": {
                            "x-coord": 3,
                            "y-coord": 2
                        },
                        "name": "OOU",
                        "hand": "TOWye",
                        "orientation": "TOWest"
                    }]
                    """

                decodedOutput : String -> Result Decode.Error (List Model.Turnout)
                decodedOutput json =
                    Decode.decodeString
                        (Decode.list LayoutJson.decodeTurnout)
                        json
            in
            Expect.equal (decodedOutput input)
                (Ok
                    [ Model.Turnout ( 3, 3 ) "101" Model.TOWye Model.TOEast (Just ( "101N", "101R" ))
                    , Model.Turnout ( 3, 2 ) "OOU" Model.TOWye Model.TOWest Nothing
                    ]
                )
