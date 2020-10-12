module Panel exposing (Layout, bkgFill, border, edge, layout, margin, panel, translateTile, window)


type alias Layout =
    { size : ( Int, Int )
    , tiles : Int
    , bkgFill : String
    , margins : Int
    , border : Int
    }


layout : Layout
layout =
    { size = ( 13, 7 )
    , tiles = 60
    , bkgFill = "#a4b887"
    , margins = 10
    , border = 2
    }


window : ( String, String )
window =
    let
        calc : Int -> Layout -> String
        calc num dim =
            String.fromInt ((num * dim.tiles) + (2 * dim.margins))
    in
    ( calc (Tuple.first layout.size) layout, calc (Tuple.second layout.size) layout )


edge : ( String, String )
edge =
    let
        calc : Int -> Layout -> String
        calc num dim =
            String.fromInt ((num * dim.tiles) + (2 * dim.border))
    in
    ( calc (Tuple.first layout.size) layout, calc (Tuple.second layout.size) layout )


panel : ( String, String )
panel =
    let
        calc : Int -> Layout -> String
        calc num dim =
            String.fromInt (num * dim.tiles)
    in
    ( calc (Tuple.first layout.size) layout, calc (Tuple.second layout.size) layout )


translateTile : ( Int, Int ) -> String
translateTile coords =
    let
        x =
            ((Tuple.first coords - 1) * layout.tiles) + (layout.tiles // 2) + layout.margins

        y =
            ((Tuple.second coords - 1) * layout.tiles) + (layout.tiles // 2) + layout.margins
    in
    String.join " " [ "translate(", String.fromInt x, String.fromInt y, ")" ]


bkgFill : String
bkgFill =
    layout.bkgFill


margin : String
margin =
    String.fromInt layout.margins


border : String
border =
    String.fromInt (layout.margins - layout.border)
