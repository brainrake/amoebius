module Main exposing (..)

import Algebra exposing (..)
import Gomoku
import Html
import Model exposing (Model, Msg(..))
import Random
import View


random_lattice : Cmd Msg
random_lattice =
    Random.generate RandomLattice gen_lattice


random_moves : Int -> Cmd Msg
random_moves size =
    let
        gen_coord =
            Random.int 0 (size - 1)
    in
    Random.generate RandomMoves (Random.pair gen_coord gen_coord)


init : Model
init =
    { selection = Nothing
    , lattice = Lattice rot_r rot_l rot_r rot_l
    , show_hints = False
    , board =
        { sides = 2
        , size = 7
        , moves = []
        }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select s ->
            ( { model | selection = s }, Cmd.none )

        Move ( x, y ) ->
            case Gomoku.move ( x, y ) model.board of
                Just board ->
                    ( { model | selection = Nothing, board = board }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Clear ->
            let
                board =
                    model.board
            in
            ( { model | board = { board | moves = [] } }, Cmd.none )

        Fill ->
            if List.length model.board.moves < model.board.size * model.board.size then
                ( model, random_moves model.board.size )
            else
                let
                    board =
                        model.board
                in
                ( { model | board = { board | moves = [] } }, random_moves model.board.size )

        RandomMoves ( x, y ) ->
            if List.length model.board.moves < model.board.size * model.board.size then
                ( Tuple.first (update (Move ( x, y )) model), random_moves model.board.size )
            else
                ( model, Cmd.none )

        Randomize ->
            ( model, random_lattice )

        RandomLattice { right, down, left, up } ->
            case make_lattice right down left up of
                Just lattice ->
                    ( { model | lattice = lattice }, Cmd.none )

                Nothing ->
                    ( model, random_lattice )

        SetLattice lattice ->
            ( { model | lattice = lattice }, Cmd.none )

        SetSize size ->
            let
                board =
                    model.board
            in
            ( { model | board = { board | size = size } }, Cmd.none )

        SetSides sides ->
            let
                board =
                    model.board
            in
            ( { model | board = { board | sides = sides } }, Cmd.none )

        ToggleHints ->
            ( { model | show_hints = not model.show_hints }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , view = View.view
        , update = update
        , subscriptions = \model -> Sub.none
        }
