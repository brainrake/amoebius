import Graphics.Render exposing (..)
import Html exposing (div, span)
import Html.Attributes exposing (style)
import Color exposing (..)
import List exposing (range, map, concatMap)


slot_pixels : number
slot_pixels = 20
board_size : number
board_size = 15
board_pixels : number
board_pixels = slot_pixels * board_size


view_slot : Html.Html msg
view_slot =
  [ ((slot_pixels/ 2, 0), (slot_pixels/ 2, slot_pixels))
  , ((0, slot_pixels/ 2), (slot_pixels, slot_pixels/ 2))
  ]
  |> map (uncurry segment)
  |> map (solidLine 1 (solid black))
  |> group
  |> svg 0 0 slot_pixels slot_pixels


view_board : Html.Html msg
view_board =
  div [ style [("width", toString board_pixels ++ "px")] ]
    (range 1 board_size |> map (λx ->
      div [ style [("line-height", "0")] ] 
        (range 1 board_size |> map (λy ->
          div [ style [ ("display", "inline-block") ] ]
            [ view_slot ]))))

view : Html.Html msg
view =
  view_board



main : Html.Html msg
main = view
