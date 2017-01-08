import Html
import Algebra exposing (..)
import Gomoku
import Model exposing (Model, Msg(..))
import View


init : Model
init =
  { selection = Nothing
  , lattice = Lattice rot_r rot_l rot_r rot_l
  --, lattice = Lattice flip_y ident flip_y ident
  , board =
    { size = 7
    , moves = []
    }
  }


update : Msg -> Model -> Model
update msg model = case msg of
  Select s -> { model | selection = s }
  Move (x, y) -> case Gomoku.move (x, y) model.board of
    Just board -> { model | selection = Nothing, board = board }
    Nothing -> model


main : Program Never Model Msg
main = Html.beginnerProgram
  { model = init
  , view = View.view
  , update = update
  }
