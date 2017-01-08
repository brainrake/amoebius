module Model exposing (Model, Msg(..))

import Algebra exposing (Coord, Lattice)
import Gomoku exposing (Board)


type alias Model =
  { selection : Maybe Coord
  , lattice : Lattice
  , board : Board
  }


type Msg =
    Select (Maybe Coord)
  | Move Coord
