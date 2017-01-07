import Html exposing (text)
import Graphics.Render exposing (..)
import Color exposing (..)
import List exposing (range, map, concatMap)


slot_pixels = 20
board_size = 15
board_pixels = slot_pixels * board_size


view_slot =
  [ ((slot_pixels/ 2, 0), (slot_pixels/ 2, slot_pixels))
  , ((0, slot_pixels/ 2), (slot_pixels, slot_pixels/ 2))
  ]
  |> map (uncurry segment)
  |> map (solidLine 1 (solid black))
  |> group


view_board =
  range 0 board_size |> concatMap (λx -> range 0 board_size |> map (\y -> (x, y)))
  |> map (λ(x, y) -> view_slot
             |> position (toFloat x * slot_pixels, toFloat y * slot_pixels))
  |> group


view =
  svg 0 0 board_pixels board_pixels view_board



main = view