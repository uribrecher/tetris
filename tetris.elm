module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Set exposing (..)
import Keyboard
import Random
import Brick


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias SparseMatrix =
    Set ( Int, Int, String )


type alias DebuggingData =
    { collide : Bool
    , landed : Bool
    , msg : String
    }


type alias Model =
    { board : SparseMatrix
    , brick : Maybe Brick.Brick
    , gameOver : Bool
    , debug : DebuggingData
    }


addColor : String -> ( Int, Int ) -> ( Int, Int, String )
addColor color ( x, y ) =
    ( x, y, color )


getLocation : ( Int, Int, String ) -> ( Int, Int )
getLocation ( x, y, c ) =
    ( x, y )


brickToMatrix : Maybe Brick.Brick -> SparseMatrix
brickToMatrix brick =
    case brick of
        Nothing ->
            Set.empty

        Just { form, tran, color } ->
            Set.map (Brick.transform tran >> addColor color) form


modelToMatrix : Model -> SparseMatrix
modelToMatrix { board, brick } =
    Set.union board (brickToMatrix brick)


collision : Maybe Brick.Brick -> SparseMatrix -> Bool
collision brick board =
    let
        clear_board =
            Set.map getLocation board

        clear_brick =
            Set.map getLocation (brickToMatrix brick)
    in
        Set.intersect clear_board clear_brick |> Set.isEmpty |> not


type alias Bounds =
    { min_x : Int
    , min_y : Int
    , max_x : Int
    , max_y : Int
    }


inBoundsX : Bounds -> ( Int, Int ) -> Bool
inBoundsX bounds ( x, _ ) =
    x >= bounds.min_x && x < bounds.max_x


inBoundsY : Bounds -> ( Int, Int ) -> Bool
inBoundsY bounds ( _, y ) =
    y >= bounds.min_y && y < bounds.max_y


inBounds : Bounds -> ( Int, Int ) -> Bool
inBounds bounds loc =
    (inBoundsX bounds loc) && (inBoundsY bounds loc)


boardBounds =
    { min_x = 0, min_y = 0, max_x = 50, max_y = 30 }


init : ( Model, Cmd Msg )
init =
    ( Model empty Nothing False (DebuggingData False False "")
    , (Random.generate NewBrick (Random.int 0 ((List.length Brick.bricks) - 1)))
    )



-- UPDATE


type Msg
    = Tick Float
    | KeyDown Int
    | KeyUp Int
    | NewBrick Int


isDownMsg : Msg -> Bool
isDownMsg msg =
    case msg of
        KeyUp _ ->
            False

        NewBrick _ ->
            False

        Tick _ ->
            True

        KeyDown keyCode ->
            Brick.toDirection keyCode == Just Brick.Down


isNewBrick : Msg -> Bool
isNewBrick msg =
    case msg of
        NewBrick _ ->
            True

        _ ->
            False


moveBrickInBoundsX : Maybe Brick.Direction -> Brick.Brick -> Brick.Brick
moveBrickInBoundsX dir brick =
    let
        newBrick =
            Brick.moveBrick dir brick
    in
        if Brick.inBoundsBrick (inBoundsX boardBounds) (Just newBrick) then
            newBrick
        else
            brick


partitionRows : Int -> SparseMatrix -> List SparseMatrix
partitionRows rowCount board =
    if rowCount == 0 then
        [ board ]
    else
        let
            ( head, tail ) =
                Set.partition (\( _, y, _ ) -> y == rowCount) board
        in
            head :: (partitionRows (rowCount - 1) tail)


partitionColumns : Int -> SparseMatrix -> List SparseMatrix
partitionColumns colCount board =
    if colCount == 0 then
        [ board ]
    else
        let
            ( head, tail ) =
                Set.partition (\( x, _, _ ) -> x == colCount) board
        in
            head :: (partitionColumns (colCount - 1) tail)


removeFullRows : SparseMatrix -> List SparseMatrix
removeFullRows board =
    let
        partialRow =
            (\row -> (Set.size row) < boardBounds.max_x)

        partitionedBoard =
            partitionRows boardBounds.max_y board
    in
        List.filter partialRow partitionedBoard


clampRow : Int -> SparseMatrix -> SparseMatrix
clampRow rowNum row =
    Set.map (\( x, _, color ) -> ( x, rowNum, color )) row


gravity : Int -> List SparseMatrix -> List SparseMatrix
gravity numRows rows =
    let
        topRow =
            numRows - List.length rows

        lowIndices =
            List.reverse (List.range topRow numRows)
    in
        List.map2 clampRow lowIndices rows


mergeRows : List SparseMatrix -> SparseMatrix
mergeRows rows =
    List.foldl Set.union Set.empty rows


updateBrick : Msg -> Maybe Brick.Brick -> Maybe Brick.Brick
updateBrick msg oldBrick =
    case oldBrick of
        Just brick ->
            case msg of
                Tick _ ->
                    Just (moveBrickInBoundsX (Just Brick.Down) brick)

                KeyDown key_code ->
                    Just (moveBrickInBoundsX (Brick.toDirection key_code) brick)

                KeyUp _ ->
                    Just brick

                NewBrick _ ->
                    Nothing

        -- this is a bug, Should never create new brick while Old brick exist
        Nothing ->
            case msg of
                NewBrick n ->
                    Brick.pickBrick n

                _ ->
                    Nothing


updateBoard : Bool -> Maybe Brick.Brick -> SparseMatrix -> SparseMatrix
updateBoard landed brick oldBoard =
    if not landed then
        oldBoard
    else
        Set.union oldBoard (brickToMatrix brick)
            |> removeFullRows
            |> (gravity boardBounds.max_y)
            |> mergeRows


detection : Msg -> Maybe Brick.Brick -> SparseMatrix -> ( Bool, Bool )
detection msg brick board =
    let
        collide =
            collision brick board

        isDown =
            isDownMsg msg

        belowFloor =
            not (Brick.inBoundsBrick (inBoundsY boardBounds) brick)
    in
        ( (isDown && collide) || belowFloor, collide )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.gameOver then
        ( model, Cmd.none )
    else
        let
            candidateBrick =
                updateBrick msg model.brick

            ( landed, collided ) =
                detection msg candidateBrick model.board

            newBoard =
                updateBoard landed model.brick model.board

            newGO =
                collided && isNewBrick msg

            newCmd =
                if landed then
                    Random.generate NewBrick (Random.int 0 ((List.length Brick.bricks) - 1))
                else
                    Cmd.none

            newBrick =
                if landed then
                    Nothing
                else if collided then
                    model.brick
                else
                    candidateBrick
        in
            ( Model newBoard newBrick newGO (DebuggingData collided landed (toString candidateBrick)), newCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Time.every second Tick
        ]



-- VIEW


rect_size =
    10


draw_rect : ( Int, Int, String ) -> Html Msg
draw_rect ( xp, yp, color ) =
    let
        posX =
            toString (xp * rect_size)

        posY =
            toString (yp * rect_size)

        rect_size_str =
            toString (rect_size - 2)
    in
        rect [ fill color, x posX, y posY, width rect_size_str, height rect_size_str ] []


view_matrix : SparseMatrix -> List (Html Msg)
view_matrix matrix =
    List.map draw_rect (toList matrix)


view : Model -> Html Msg
view model =
    Html.div []
        [ let
            width_size =
                (toString (boardBounds.max_x * rect_size))

            height_size =
                (toString (boardBounds.max_y * rect_size))
          in
            svg [ viewBox ("0 0 " ++ width_size ++ " " ++ height_size), width "100px" ]
                (List.append
                    [ rect
                        [ fill "#000000"
                        , x "0"
                        , y "0"
                        , width width_size
                        , height height_size
                        ]
                        []
                    ]
                    (view_matrix (modelToMatrix model))
                )
        , Html.text
            (if model.gameOver then
                "game over"
             else
                ""
            )
        , (view_debug model.debug)
        ]


view_debug : DebuggingData -> Html msg
view_debug debug =
    Html.div []
        [ Html.p [] [ Html.text ("collide = " ++ toString debug.collide) ]
        , Html.p [] [ Html.text ("landed  = " ++ toString debug.landed) ]
        , Html.p [] [ Html.text ("msg = " ++ debug.msg) ]
        ]
