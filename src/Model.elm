module Model exposing (..)

import Algebra exposing (Coord, Lattice)
import Gomoku exposing (Board)


type alias Model =
    { selection : Maybe Coord
    , lattice : Lattice
    , board : Board
    , show_hints : Bool
    , num_players : NumPlayers
    }


type NumPlayers
    = TwoPlayers
    | ThreePlayers


type Msg
    = Select (Maybe Coord)
    | Move Coord
    | Clear
    | Fill
    | RandomMoves Coord
    | Randomize
    | RandomLattice Lattice
    | SetLattice Lattice
    | SetSize Int
    | ToggleHints
    | ChangeNumPlayers NumPlayers
