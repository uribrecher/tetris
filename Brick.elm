module Brick exposing (..)

import Set exposing (Set)


type alias Brick =
    { form : Set ( Int, Int )
    , tran : ( Int, Int, Int )
    , color : String
    }


rotate_cart : Int -> ( Int, Int ) -> ( Int, Int )
rotate_cart r ( x, y ) =
    if r == 0 then
        ( x, y )
    else
        let
            ( new_x, new_y ) =
                rotate_cart (r - 1) ( x, y )
        in
            ( new_y, negate new_x )


translate_cart : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
translate_cart ( ofx, ofy ) ( x, y ) =
    ( x + ofx, y + ofy )


transform : ( Int, Int, Int ) -> (( Int, Int ) -> ( Int, Int ))
transform ( ofx, ofy, ofr ) =
    rotate_cart ofr >> translate_cart ( ofx, ofy )


translate_x offset ( x, y, r ) =
    ( x + offset, y, r )


translate_y offset ( x, y, r ) =
    ( x, y + offset, r )


rotate offset ( x, y, r ) =
    ( x, y, r + offset )


inBoundsBrick : (( Int, Int ) -> Bool) -> Maybe Brick -> Bool
inBoundsBrick boundsFunc brick =
    case brick of
        Nothing ->
            True

        Just jbrick ->
            Set.foldl (\loc result -> result && (boundsFunc (transform jbrick.tran loc))) True jbrick.form


bricks : List Brick
bricks =
    [ Brick (Set.fromList [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, 1 ) ]) ( 5, 3, 0 ) "#ffff00"
    , Brick (Set.fromList [ ( 0, 0 ), ( -1, 0 ), ( 0, 1 ), ( 1, 1 ) ]) ( 5, 3, 0 ) "#ff00ff"
    , Brick (Set.fromList [ ( 0, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 0 ) ]) ( 5, 3, 0 ) "#00ffff"
    , Brick (Set.fromList [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 1, 1 ) ]) ( 5, 3, 0 ) "#ffffaa"
    , Brick (Set.fromList [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ) ]) ( 5, 3, 0 ) "#ffaaff"
    , Brick (Set.fromList [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]) ( 5, 3, 0 ) "#aaffff"
    , Brick (Set.fromList [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 2, 0 ) ]) ( 5, 3, 0 ) "#aaffaa"
    ]


pickBrick : Int -> Maybe Brick
pickBrick n =
    List.head (List.drop n bricks)


type Direction
    = Up
    | Down
    | Left
    | Right


toDirection : Int -> Maybe Direction
toDirection code =
    case code of
        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

        _ ->
            Nothing


moveBrick : Maybe Direction -> Brick -> Brick
moveBrick dir brick =
    let
        trans_func =
            case dir of
                Just Left ->
                    translate_x -1

                Just Right ->
                    translate_x 1

                Just Down ->
                    translate_y 1

                Just Up ->
                    rotate 1

                Nothing ->
                    identity
    in
        { brick | tran = trans_func brick.tran }
