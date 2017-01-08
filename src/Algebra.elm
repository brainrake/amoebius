module Algebra exposing (..)


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
rot_r = (0, -1, 1, 0, 0, 0)

rot_l : Transform
rot_l = (0, 1, -1, 0, 0, 0)


--lattice : Transform -> Transform -> Transform -> Transform -> maybe Lattice
--lattice right down left up =


to3x3 : Lattice -> List Transform
to3x3 { right, down, left, up } =
  [ up *** left   , up    , up *** right
  , left          , ident , right
  , down *** left , down  , down *** right ]
