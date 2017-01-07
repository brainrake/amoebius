import Graphics.Render exposing (..)
import Html exposing (Html, div, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Color exposing (..)
import List exposing (range, map, concatMap)
import Matrix exposing (..)

type Cell = Black | White | Empty

type alias Model =
  { selection : Maybe (Int, Int)
  , board : Matrix Cell
  }

type Msg =
    Hover (Maybe (Int, Int))


slot_pixels : number
slot_pixels = 20

board_size : number
board_size = 15

board_pixels : number
board_pixels = slot_pixels * board_size


view_isxn : Bool -> Html Msg
view_isxn selected =
  let
    end = slot_pixels
    mid = slot_pixels / 2
    cross =
      [ ((mid, 0), (mid, end))
      , ((0, mid), (end, mid))
      ]
      |> List.map (uncurry segment)
      |> List.map (solidLine 1 (solid black))
    dot = circle (mid)
          |> (filled (solid black))
          |> position (mid, mid)
  in svg 0 0 end end <|
    group (cross ++ if selected then [ dot ] else [])


view_board : Model -> Html Msg
view_board model =
  div
    [ onMouseLeave (Hover Nothing)
    , style
      [ ("line-height", "0")
      , ("width", toString board_pixels ++ "px")
      , ("background-color", "rgb(253, 226, 119)")]
    ]
    (range 1 board_size |> List.map (λx ->
      div []
        (range 1 board_size |> List.map (λy ->
          div
            [ onMouseEnter (Hover (Just (x, y)))
            , style [ ("display", "inline-block") ]
            ]
            [ view_isxn (model.selection == Just (x, y)) ]))))

view : Model -> Html Msg
view model =
  view_board model
  -- div [] [ view_board , view_board, view_board ]


update : Msg -> Model -> Model
update msg model = case msg of
  Hover h -> { model | selection = h }


main : Program Never Model Msg
main = Html.beginnerProgram
  { model = { selection = Nothing, board = Matrix.square board_size (always Empty) }
  , view = view
  , update = update
  }
