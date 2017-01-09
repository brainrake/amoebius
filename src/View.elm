module View exposing (view)

import Color exposing (..)
import Graphics.Render exposing (..)
import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (style, value, type_, attribute)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseDown, onInput)
import Json.Decode as J
import Kintail.InputWidget exposing (comboBox)
import List exposing (range, map, concatMap, filter, member, length, reverse, concat)
import List.Extra exposing (elemIndex)
import Maybe.Extra exposing ((?), maybeToList, join)

import Algebra exposing (Size, Transform, Lattice, to3x3, transforms, show_transform)
import Gomoku exposing (..)
import Model exposing (Model, Msg(..))


slot_pixels : number
slot_pixels = 20


view : Model -> Html Msg
view model =
  let grid3x3 = model.lattice
                |> to3x3
                |> map transform_html
                |> map ((|>) (view_game model))
      boards = div
        [ onMouseLeave (Select Nothing)
        , style
          [ ("width", toString (model.board.size * slot_pixels * 3) ++ "px")
          , ("line-height", "0")
          ]
        ] grid3x3
      config = view_config model
  in  div [] [ config, boards ]


view_game : Model -> Html Msg
view_game model =
  div [ style [ ("display", "inline-block")
              , ("position", "relative")
              ]
      ]
      [ let board_pixels = toFloat model.board.size * slot_pixels
        in svg 0 0 board_pixels board_pixels (view_board model)
      ]


box : List (Html msg) -> Html msg
box =
  div [ style [("display", "inline-block"), ("width", "60px")] ]

view_transform : (Transform -> Lattice) -> Transform -> Html Msg
view_transform f t =
  box [ comboBox [] show_transform transforms t
        |> Html.map (\t -> SetLattice (f t))
      ]


view_config : Model -> Html Msg
view_config { lattice, board } = case lattice of
  { right, down, left, up } -> div []
    [ div []
      [ box []
      , up |> view_transform (\t -> { lattice | up = t })
      , box []
      ]
    , div []
      [ left |> view_transform (\t -> { lattice | left = t })
      , box []
      , right |> view_transform (\t -> { lattice | right = t })
      ]
    , div []
      [ box []
      , down |> view_transform (\t -> { lattice | down = t })
      , box []
      ]
    , div [] [ button [ Html.Events.onClick Randomize ] [ Html.text "randomize"] ]
    , div []
      [ Html.text "size: "
      , Html.input
        [ onInput (\s -> SetSize (String.toInt s |> Result.withDefault board.size))
        , value (toString board.size)
        , type_ "number"
        , attribute "min" "1"
        , style [ ("width", "40px") ]
        ] []
      , button [ Html.Events.onClick Fill ] [ Html.text "fill"]
      , button [ Html.Events.onClick Clear ] [ Html.text "clear"]
      ]
    ]

view_board : Model -> Form Msg
view_board model =
  group <| concat <| (
    range 0 (model.board.size - 1) |> map (λx ->
      range 0 (model.board.size - 1) |> map (λy ->
        view_cell
          (model.selection == Just (x, y))
          (whose_turn (length model.board.moves))
          (model.board.moves |> reverse |> elemIndex (x, y) |> Maybe.map whose_turn)
        |> position (toFloat x * slot_pixels, toFloat y * slot_pixels)
        |> on "mouseenter" (hover x y model)
        |> on "mousedown" (click x y model)
  ) ) )



hover : Int -> Int -> Model -> J.Decoder Msg
hover x y model =
  if model.board.moves |> not << member (x, y)
  then J.succeed (Select (Just (x, y)))
  else (model.selection |> Maybe.map (always (J.succeed (Select Nothing)))) ? J.fail "occupied"


click : Int -> Int -> Model -> J.Decoder Msg
click x y model =
  if model.board.moves |> not << member (x, y)
  then J.succeed (Move (x, y))
  else J.fail "occupied"

view_cell : Bool -> Side -> Maybe Side -> Form Msg
view_cell selected sideToPlay cell =
  let
    end = slot_pixels
    mid = slot_pixels / 2
    background =
      rectangle end end
      |> filled (solid (rgb 253 226 119))
      |> position (mid, mid)
    cross =
      [ ((mid, 0), (mid, end))
      , ((0, mid), (end, mid))
      ] |> map (uncurry segment >> solidLine 1 (solid black))
    dot color =
      circle mid
      |> filled (solid color)
      |> opacity (if selected then (if color == black then 0.5 else 0.7) else 1)
      |> position (mid, mid)
  in group <|
    [ background ]
    ++ cross
    ++ (if selected then [ dot (colorOf sideToPlay) ] else [])
    ++ (cell |> Maybe.map (\c -> dot (colorOf c)) |> maybeToList)



transform_html : Transform -> Html msg -> Html msg
transform_html xform html =
  div [ style [ ("transform", "matrix" ++ toString xform), ("display", "inline-block") ] ] [ html ]
