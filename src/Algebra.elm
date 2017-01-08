module Algebra exposing (..)

import Random
import List exposing (length, map)
import List.Extra exposing ((!!))
import Maybe.Extra exposing ((?))


type alias Size = Int

type alias Coord = (Int, Int)

type alias Transform = (Int, Int, Int, Int, Int, Int)

type alias Lattice =
  { right : Transform
  , down : Transform
  , left : Transform
  , up : Transform
  }


transform : Transform -> Coord -> Coord
transform (a, b, c, d, e, f) (x, y) =
  (a*x + b*y + c, d*x + e*y + f)

invert : Transform -> Transform
invert (a, b, c, d, e, f) =
  (a, -b, -c, d, -e, -f)

compose : Transform -> Transform -> Transform
compose (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2) =
  (a1*a2 + b1*b2, a1*c2 + b1*d2, c1*a2 + d1*b2, c1*c2 + d1*d2, e1+e2, f1+f2 )


(***) : Transform -> Transform -> Transform
(***) = compose

infixl 5 ***


ident : Transform
ident = (1, 0, 0, 1, 0, 0)

flip_x : Transform
flip_x = (-1, 0, 0, 1, 0, 0)

flip_y : Transform
flip_y = (1, 0, 0, -1, 0, 0)

rot_r : Transform
rot_r = (0, 1, -1, 0, 0, 0)

rot_l : Transform
rot_l  = (0, -1, 1, 0, 0, 0)


show_transform : Transform -> String
show_transform t =
  if t == ident then "ident"
  else if t == flip_x then "flip_x"
  else if t == flip_y then "flip_y"
  else if t == rot_r then "rot_r"
  else if t == rot_l then "rot_l"
  else toString t

make_lattice : Transform -> Transform -> Transform -> Transform -> Maybe Lattice
make_lattice right down left up =
  if right *** up == up *** right
    && left *** up == up *** left
    && right *** down == down *** right
    && left *** down == down *** left
  then Just (Lattice right down left up)
  else Nothing

to3x3 : Lattice -> List Transform
to3x3 { right, down, left, up } =
  [ up *** left   , up    , up *** right
  , left          , ident , right
  , down *** left , down  , down *** right ]


transforms: List Transform
transforms =
  [ ident, rot_r, rot_l, flip_x, flip_y ]


gen_transform : Random.Generator Transform
gen_transform =
  Random.int 0 (length transforms - 1)
  |> Random.map (\i -> transforms !! i ? ident)

gen_lattice : Random.Generator Lattice
gen_lattice =
  let get i xs = (xs !! i ? ident)
  in Random.list 4 gen_transform
     |> Random.map (\xs -> Lattice (get 0 xs) (get 1 xs) (get 2 xs) (get 3 xs))
