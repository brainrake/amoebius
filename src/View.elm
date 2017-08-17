module View exposing (view)

import Color exposing (..)
import Graphics.Render exposing (..)
import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (style, value, type_, attribute)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseDown, onInput)
import Json.Decode as J
import Kintail.InputWidget exposing (comboBox)
import List exposing (range, map, concatMap, filter, member, length, reverse, concat, indexedMap, concatMap)
import List.Extra exposing ((!!), elemIndex)
import Maybe.Extra exposing ((?), maybeToList, join)
import Algebra exposing (Size, Transform, Lattice, to3x3, transform, transforms, show_transform, floatCoord, ident, moebius, vortex)
import Gomoku exposing (ops2, ops3)
import Model exposing (..)


slot_pixels : Int
slot_pixels =
    20


view : Model -> Html Msg
view model =
    let
        board_pixels =
            model.board.size * slot_pixels

        transform_html ( x, y ) xform form =
            div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "transform", "matrix" ++ toString xform )
                    ]
                ]
                [ svg (toFloat (x * board_pixels)) (toFloat (y * board_pixels)) (toFloat board_pixels) (toFloat board_pixels) <|
                    form
                ]

        grid3x3 =
            to3x3 model.lattice
                |> indexedMap
                    (\x ->
                        indexedMap
                            (\y xform ->
                                group
                                    ([ view_board model ]
                                        ++ (if model.show_hints then
                                                [ view_hints model ]
                                            else
                                                []
                                           )
                                    )
                                    |> transform_html ( x, y ) xform
                            )
                    )

        style_ =
            style
                [ ( "line-height", "0" )
                , ( "width", toString (3 * board_pixels) ++ "px" )
                ]
    in
        div []
            [ view_config model
            , div [ onMouseLeave (Select Nothing), style_ ] <|
                concat grid3x3
            ]


view_hints : Model -> Form msg
view_hints model =
    let
        end =
            toFloat (model.board.size * slot_pixels)

        mid =
            end / 2
    in
        group
            [ segment ( 0, 0 ) ( end, 0 ) |> solidLine 4 (solid red)
            , segment ( mid, 0 ) ( mid - 10, 10 ) |> solidLine 4 (solid red)
            , segment ( 0, 0 ) ( 0, end ) |> solidLine 4 (solid blue)
            , segment ( 0, mid ) ( 10, mid + 10 ) |> solidLine 4 (solid blue)
            , segment ( 0, end ) ( end, end ) |> solidLine 4 (solid brown)
            , segment ( mid, end ) ( mid + 10, end - 10 ) |> solidLine 4 (solid brown)
            , segment ( end, 0 ) ( end, end ) |> solidLine 4 (solid purple)
            , segment ( end, mid ) ( end - 10, mid - 10 ) |> solidLine 4 (solid purple)
            ]


box : List (Html msg) -> Html msg
box =
    div [ style [ ( "display", "inline-block" ), ( "width", "60px" ) ] ]


view_transform : (Transform -> Lattice) -> Transform -> Html Msg
view_transform f t =
    box
        [ comboBox [] show_transform transforms t
            |> Html.map (\t -> SetLattice (f t))
        ]


view_config : Model -> Html Msg
view_config { lattice, board } =
    case lattice of
        { right, down, left, up } ->
            div []
                [ Html.node "style" [] [ Html.text "html {font-family : sans;}" ]
                , div []
                    [ Html.text "players "
                    , button [ Html.Events.onClick (ChangeNumPlayers TwoPlayers) ] [ Html.text "2" ]
                    , button [ Html.Events.onClick (ChangeNumPlayers ThreePlayers) ] [ Html.text "3" ]
                    ]
                , Html.br [] []
                , div []
                    [ Html.text "lattice "
                    , button [ Html.Events.onClick (SetLattice moebius) ] [ Html.text "moebius" ]
                    , button [ Html.Events.onClick (SetLattice vortex) ] [ Html.text "vortex" ]
                    , button [ Html.Events.onClick Randomize ] [ Html.text "random" ]
                    ]
                , div []
                    [ box []
                    , up |> view_transform (\t -> { lattice | up = t })
                    , box []
                    ]
                , div []
                    [ left |> view_transform (\t -> { lattice | left = t })
                    , box []
                    , right |> view_transform (\t -> { lattice | right = t })
                    ]
                , div []
                    [ box []
                    , down |> view_transform (\t -> { lattice | down = t })
                    , box []
                    ]
                , Html.br [] []
                , div []
                    [ Html.text "size "
                    , Html.input
                        [ onInput (\s -> SetSize (String.toInt s |> Result.withDefault board.size))
                        , value (toString board.size)
                        , type_ "number"
                        , attribute "min" "1"
                        , style [ ( "width", "40px" ) ]
                        ]
                        []
                    ]
                , Html.br [] []
                , div []
                    [ button [ Html.Events.onClick Fill ] [ Html.text "fill" ]
                    , button [ Html.Events.onClick Clear ] [ Html.text "clear" ]
                    , button [ Html.Events.onClick ToggleHints ] [ Html.text "hints" ]
                    ]
                , Html.br [] []
                ]


view_board : Model -> Form Msg
view_board model =
    group <|
        concat <|
            (range 0 (model.board.size - 1)
                |> map
                    (\x ->
                        range 0 (model.board.size - 1)
                            |> map
                                (\y ->
                                    let
                                        v ops =
                                            view_cell
                                                (model.selection == Just ( x, y ))
                                                ops
                                                (ops.whose_turn (length model.board.moves))
                                                (model.board.moves |> reverse |> elemIndex ( x, y ) |> Maybe.map ops.whose_turn)
                                                |> position ( toFloat (x * slot_pixels), toFloat (y * slot_pixels) )
                                                |> on "mouseenter" (hover x y model)
                                                |> on "mousedown" (click x y model)
                                    in
                                        case model.num_players of
                                            TwoPlayers ->
                                                v ops2

                                            ThreePlayers ->
                                                v ops3
                                )
                    )
            )


hover : Int -> Int -> Model -> J.Decoder Msg
hover x y model =
    if model.board.moves |> not << member ( x, y ) then
        J.succeed (Select (Just ( x, y )))
    else
        (model.selection |> Maybe.map (always (J.succeed (Select Nothing)))) ? J.fail "occupied"


click : Int -> Int -> Model -> J.Decoder Msg
click x y model =
    if model.board.moves |> not << member ( x, y ) then
        J.succeed (Move ( x, y ))
    else
        J.fail "occupied"


view_cell : Bool -> Gomoku.Ops side -> side -> Maybe side -> Form Msg
view_cell selected ops sideToPlay cell =
    let
        end =
            toFloat slot_pixels

        mid =
            end / 2

        background =
            rectangle end end
                |> filled (solid (rgb 253 226 119))
                |> position ( mid, mid )

        cross =
            [ ( ( mid, 0 ), ( mid, end ) )
            , ( ( 0, mid ), ( end, mid ) )
            ]
                |> map (uncurry segment >> solidLine 1 (solid black))

        dot color =
            circle mid
                |> filled (solid color)
                |> opacity
                    (if selected then
                        (if color == black then
                            0.5
                         else
                            0.7
                        )
                     else
                        1
                    )
                |> position ( mid, mid )
    in
        group <|
            [ background ]
                ++ cross
                ++ (if selected then
                        [ dot (ops.color_of sideToPlay) ]
                    else
                        []
                   )
                ++ (cell |> Maybe.map (\c -> dot (ops.color_of c)) |> maybeToList)
