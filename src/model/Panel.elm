module Panel exposing (Diagram, border, diagram, edge, margin, panel, translateTile, window)


type alias Diagram =
    { width : Int
    , height : Int
    , tiles : Int
    , bkgFill : String
    , margins : Int
    , border : Int
    , name : String
    }


diagram : Diagram
diagram =
    { width = 13
    , height = 7
    , tiles = 60
    , bkgFill = "#a4b887"
    , margins = 10
    , border = 2
    , name = "CBUS Elm Demo"
    }


window : ( String, String )
window =
    let
        calc : Int -> Diagram -> String
        calc num dim =
            String.fromInt ((num * dim.tiles) + (2 * dim.margins))
    in
    ( calc diagram.width diagram, calc diagram.height diagram )


edge : ( String, String )
edge =
    let
        calc : Int -> Diagram -> String
        calc num dim =
            String.fromInt ((num * dim.tiles) + (2 * dim.border))
    in
    ( calc diagram.width diagram, calc diagram.height diagram )


panel : ( String, String )
panel =
    let
        calc : Int -> Diagram -> String
        calc num dim =
            String.fromInt (num * dim.tiles)
    in
    ( calc diagram.width diagram, calc diagram.height diagram )


translateTile : ( Int, Int ) -> String
translateTile coords =
    let
        x =
            ((Tuple.first coords - 1) * diagram.tiles) + (diagram.tiles // 2) + diagram.margins

        y =
            ((Tuple.second coords - 1) * diagram.tiles) + (diagram.tiles // 2) + diagram.margins
    in
    String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")" ]


margin : String
margin =
    String.fromInt diagram.margins


border : String
border =
    String.fromInt (diagram.margins - diagram.border)
