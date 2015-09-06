(* $Id: glMap.mli,v 1.3 2000-04-12 07:40:24 garrigue Exp $ *)

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
val map1 :
  target:target -> float * float -> order:int -> [`double] Raw.t -> unit
    (* [map1 :target (u1,u2) :order points] defines a 1-dimensional map.
       [order] is the number of control points in [points] *)
val map2 :
  target:target ->
  float * float ->
  order:int -> float * float -> order:int -> [`double] Raw.t -> unit
    (* [map1 :target (u1,u2) order:uorder (v1,v2) order:vorder points]
       defines a 2-dimensional map.
       The number of control points in [points] is [uorder*vorder] *)

val eval_coord1 : float -> unit
val eval_coord2 : float -> float -> unit
    (* Evaluate the maps at given coordinates *)

val grid1 : n:int -> range:float * float -> unit
val grid2 :
  n1:int -> range1:float * float -> n2:int -> range2:float * float -> unit
    (* Define 1- and 2-dimensional meshes to the maps *)

val eval_mesh1 : mode:[`line|`point] -> range:(int * int) -> unit
val eval_mesh2 :
  mode:[`fill|`line|`point] -> range1:(int * int) -> range2:(int * int) -> unit
val eval_point1 : int -> unit
val eval_point2 : int -> int -> unit
    (* Evaluate meshes at given coordinates *)
