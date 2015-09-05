(* $Id: glMap.ml,v 1.4 2008-01-10 05:50:37 garrigue Exp $ *)

external eval_coord1 : float -> unit = "ml_glEvalCoord1d"
external eval_coord2 : float -> float -> unit = "ml_glEvalCoord2d"
external eval_mesh1 : mode:[`point|`line] -> int -> int -> unit
    = "ml_glEvalMesh1"
let eval_mesh1 ~mode ~range:(u1,u2) = eval_mesh1 ~mode u1 u2
external eval_mesh2 :
    mode:[`point|`line|`fill] -> int -> int -> int -> int -> unit
    = "ml_glEvalMesh2"
let eval_mesh2 ~mode ~range1:(u1,u2) ~range2:(v1,v2) =
  eval_mesh2 ~mode u1 u2 v1 v2
external eval_point1 : int -> unit = "ml_glEvalPoint1"
external eval_point2 : int -> int -> unit = "ml_glEvalPoint2"

type target =
  [ `vertex_3
  | `vertex_4
  | `index
  | `color_4
  | `normal
  | `texture_coord_1
  | `texture_coord_2
  | `texture_coord_3
  | `texture_coord_4 ]
external map1 :
    target:target -> (float*float) -> order:int -> [`double] Raw.t -> unit
    = "ml_glMap1d"
external map2 :
    target:target -> (float*float) -> order:int ->
    (float*float) -> order:int -> [`double] Raw.t -> unit
    = "ml_glMap2d_bc" "ml_glMap2d"
external grid1 : n:int -> range:(float * float) -> unit
    = "ml_glMapGrid1d"
external grid2 :
    n1:int -> range1:(float * float) ->
    n2:int -> range2:(float * float) -> unit
    = "ml_glMapGrid2d"
