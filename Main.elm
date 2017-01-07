import Graphics.Render exposing (..)
import Html exposing (Html, div, span)
import Html.Attributes exposing (style)
import Color exposing (..)
import List exposing (range, map, concatMap)

type alias Model =
  { selection: Maybe (Int, Int) }

type Msg = SelectCell (Maybe (Int, Int))

slot_pixels : number
slot_pixels = 20

board_size : number
board_size = 15

board_pixels : number
board_pixels = slot_pixels * board_size

view_slot : Html Msg
view_slot =
  [ ((slot_pixels/ 2, 0), (slot_pixels/ 2, slot_pixels))
  , ((0, slot_pixels/ 2), (slot_pixels, slot_pixels/ 2))
  ]
  |> map (uncurry segment)
  |> map (solidLine 1 (solid black))
  |> group
  |> svg 0 0 slot_pixels slot_pixels


view_board : Html Msg
view_board =
  div [ style [("width", toString board_pixels ++ "px")] ]
    (range 1 board_size |> map (λx ->
      div [ style [("line-height", "0")] ]
        (range 1 board_size |> map (λy ->
          div [ style [ ("display", "inline-block") ] ]
            [ view_slot ]))))

view : Model -> Html Msg
view model =
  view_board
  -- div [] [ view_board , view_board, view_board ]


update : Msg -> Model -> Model
update msg model = case msg of
  SelectCell s -> { selection = s }


main : Program Never Model Msg
main = Html.beginnerProgram
  { model = { selection = Nothing }
  , view = view
  , update = update
  }
