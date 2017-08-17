module Gomoku exposing (..)

import Color exposing (Color, black, white, blue)
import List exposing (member)


type alias Board =
    { size : Int
    , moves : List ( Int, Int )
    }


type Side2
    = Black2
    | White2


type Side3
    = Black3
    | White3
    | Blue3


move : ( Int, Int ) -> Board -> Maybe Board
move ( x, y ) { size, moves } =
    if
        (not (moves |> member ( x, y )))
            && (x >= 0)
            && (x < size)
            && (y >= 0)
            && (y < size)
    then
        Just { size = size, moves = ( x, y ) :: moves }
    else
        Nothing


whose_turn2 : Int -> Side2
whose_turn2 n =
    if n % 2 == 0 then
        Black2
    else
        White2


whose_turn3 : Int -> Side3
whose_turn3 n =
    if n % 3 == 0 then
        Black3
    else if n % 3 == 1 then
        White3
    else
        Blue3


other2 : Side2 -> Side2
other2 side =
    case side of
        Black2 ->
            White2

        White2 ->
            Black2


other3 : Side3 -> Side3
other3 side =
    case side of
        Black3 ->
            White3

        White3 ->
            Blue3

        Blue3 ->
            Black3


color_of2 : Side2 -> Color
color_of2 side =
    case side of
        Black2 ->
            black

        White2 ->
            white


color_of3 : Side3 -> Color
color_of3 side =
    case side of
        Black3 ->
            black

        White3 ->
            white

        Blue3 ->
            blue


type alias Ops a =
    { whose_turn : Int -> a
    , other : a -> a
    , color_of : a -> Color
    }


ops2 : Ops Side2
ops2 =
    { whose_turn = whose_turn2
    , other = other2
    , color_of = color_of2
    }


ops3 : Ops Side3
ops3 =
    { whose_turn = whose_turn3
    , other = other3
    , color_of = color_of3
    }
