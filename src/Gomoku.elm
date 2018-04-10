module Gomoku exposing (..)

import Color exposing (..)
import List exposing (member)


type alias Board =
    { sides : Int
    , size : Int
    , moves : List ( Int, Int )
    }


type alias Side =
    Int


move : ( Int, Int ) -> Board -> Maybe Board
move ( x, y ) { sides, size, moves } =
    if
        not (moves |> member ( x, y ))
            && (x >= 0)
            && (x < size)
            && (y >= 0)
            && (y < size)
    then
        Just { sides = sides, size = size, moves = ( x, y ) :: moves }
    else
        Nothing


whose_turn : Side -> Int -> Side
whose_turn sides moves =
    moves % sides


other_side : Side -> Board -> Side
other_side side board =
    side % board.sides


color_of : Side -> Color
color_of side =
    case side % 8 of
        0 ->
            black

        1 ->
            white

        2 ->
            blue

        3 ->
            red

        4 ->
            purple

        5 ->
            orange

        6 ->
            green

        _ ->
            brown
