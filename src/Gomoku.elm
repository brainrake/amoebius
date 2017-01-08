module Gomoku exposing (..)

import Color exposing (Color, black, white)
import List exposing (member)


type alias Board =
  { size : Int
  , moves : List (Int, Int)
  }


type Side = Black | White


move : (Int, Int) -> Board -> Maybe Board
move (x, y) { size, moves } =
  if (not (moves |> member (x, y)))
    && (x >= 0) && (x < size)
    && (y >= 0) && (y < size)
  then Just { size = size , moves = (x, y) :: moves }
  else Nothing


whose_turn : Int -> Side
whose_turn n = if n % 2 == 0 then Black else White


other : Side -> Side
other side = case side of
  Black -> White
  White -> Black


colorOf : Side -> Color
colorOf side = case side of
  Black -> black
  White -> white
