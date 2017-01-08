module View exposing (view)

import Color exposing (..)
import Graphics.Render exposing (..)
import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (style, value, type_, attribute)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseDown, onInput)
import Kintail.InputWidget exposing (comboBox)
import List exposing (range, map, concatMap, filter, member, length, reverse)
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
                |> map ((|>) (view_board model))
      boards = div
        [ onMouseLeave (Select Nothing)
        , style
          [ ("width", toString (model.board.size * slot_pixels * 3) ++ "px")
          , ("line-height", "0")
          ]
        ] grid3x3
      config = view_config model
  in  div [] [ config, boards ]


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

view_board : Model -> Html Msg
view_board model =
  div
    [ style [ ("background-color", "rgb(253, 226, 119)")
            , ("display", "inline-block")
            ]
    ]
    (range 0 (model.board.size - 1) |> map (λx ->
      div [] (
        range 0 (model.board.size - 1) |> map (λy ->
          div
            ( [ style [ ("display", "inline-block") ] ]
              ++ (hover x y model |> Maybe.map Html.Events.onMouseEnter|> maybeToList)
              ++ (click x y model |> Maybe.map Html.Events.onMouseDown |> maybeToList)
            )
            [ view_isxn
                (model.selection == Just (x, y))
                (model.board.moves |> length |> whose_turn)
                (model.board.moves |> reverse |> elemIndex (x, y) |> Maybe.map whose_turn)
            ] ))))

hover : Int -> Int -> Model -> Maybe Msg
hover x y model =
  if model.board.moves |> not << member (x, y)
  then Just (Select (Just (x, y)))
  else model.selection |> Maybe.map (always (Select Nothing))


click : Int -> Int -> Model -> Maybe Msg
click x y model =
  if model.board.moves |> not << member (x, y)
  then Just (Move (x, y))
  else Nothing

view_isxn : Bool -> Side -> Maybe Side -> Html Msg
view_isxn selected sideToPlay cell =
  let
    end = slot_pixels
    mid = slot_pixels / 2
    cross =
      [ ((mid, 0), (mid, end))
      , ((0, mid), (end, mid))
      ]
      |> map (uncurry segment >> solidLine 1 (solid black))
    dot color = circle (mid)
          |> filled (solid (color))
          |> position (mid, mid)
  in svg 0 0 end end <|
    group (
      cross
      ++ if selected then [ dot (colorOf sideToPlay) ] else []
      ++ (cell |> Maybe.map (colorOf >> dot) |> maybeToList)
    )


transform_html : Transform -> Html msg -> Html msg
transform_html xform html =
  div [ style [ ("transform", "matrix" ++ toString xform), ("display", "inline-block") ] ] [ html ]
