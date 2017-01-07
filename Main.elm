import Graphics.Render exposing (..)
import Html exposing (Html, div, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseDown)
import Color exposing (..)
import List exposing (range, map, concatMap)
import Matrix exposing (..)
import Maybe.Extra exposing (maybeToList, join)

type Side = Black | White

other : Side -> Side
other side = case side of
  Black -> White
  White -> Black

colorOf : Side -> Color
colorOf side = case side of
  Black -> black
  White -> white

type alias Cell = Maybe Side

type alias Model =
  { selection : Maybe (Int, Int)
  , board : Matrix Cell
  , side: Side
  }

type Msg =
    Select (Maybe (Int, Int))
  | Play (Int, Int)


slot_pixels : number
slot_pixels = 20

board_size : number
board_size = 15

board_pixels : number
board_pixels = slot_pixels * board_size


view_isxn : Bool -> Side -> Cell -> Html Msg
view_isxn selected sideToPlay cell =
  let
    end = slot_pixels
    mid = slot_pixels / 2
    cross =
      [ ((mid, 0), (mid, end))
      , ((0, mid), (end, mid))
      ]
      |> List.map (uncurry segment)
      |> List.map (solidLine 1 (solid black))
    dot color = circle (mid)
          |> (filled (solid (color)))
          |> position (mid, mid)
  in svg 0 0 end end <|
    group (
      cross
      ++ if selected then [ dot (colorOf sideToPlay) ] else []
      ++ maybeToList (cell |> Maybe.map colorOf |> Maybe.map dot)
    )


view_board : Model -> Html Msg
view_board model =
  div
    [ onMouseLeave (Select Nothing)
    , style
      [ ("line-height", "0")
      , ("width", toString board_pixels ++ "px")
      , ("background-color", "rgb(253, 226, 119)")]
    ]
    (range 0 (board_size - 1) |> List.map (λx ->
      div []
        (range 0 (board_size - 1) |> List.map (λy ->
          div
            (
              [ style [ ("display", "inline-block") ] ]
              ++ maybeToList (hover x y model |> Maybe.map onMouseEnter)
              ++ maybeToList (click x y model |> Maybe.map Html.Events.onMouseDown)
            )
            [ view_isxn
                (model.selection == Just (x, y))
                model.side
                (model.board |> get (x, y) |> join) ]))))


hover : Int -> Int -> Model -> Maybe Msg
hover x y model = case model.board |> get (x, y) of
  Just Nothing -> Just <| Select <| Just (x, y)
  _ -> model.selection |> (Maybe.map <| always <| Select Nothing)


click : Int -> Int -> { b | board : Matrix (Maybe a) } -> Maybe Msg
click x y model = case model.board |> get (x, y) of
  Just Nothing -> Just <| Play (x, y)
  _ -> Nothing


view : Model -> Html Msg
view model =
  view_board model
  -- div [] [ view_board , view_board, view_board ]


update : Msg -> Model -> Model
update msg model = case msg of
  Select s -> { model | selection = s }
  Play (x, y) ->
    { model
    | board = model.board |> set (x, y) (Just model.side)
    , side = model.side |> other
    }


main : Program Never Model Msg
main = Html.beginnerProgram
  { model = { selection = Nothing, board = Matrix.square board_size (always Nothing), side = Black }
  , view = view
  , update = update
  }
