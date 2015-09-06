(** Vertex array manipulation functions *)
(* $Id: glArray.mli,v 1.7 2008-10-25 02:22:58 garrigue Exp $ *)

(** The six different kinds for array *)
type kind =
  [ `color | `edge_flag | `index | `normal | `texture_coord | `vertex ]

(** Tell openGL the address of the edgeFlag array.
    Raw array must be static. *)
val edge_flag : [ `bitmap ] Raw.t -> unit

(** Tell openGL the address of the texCoor array
    Raw array must be static. *)
val tex_coord :
  [< `one | `two | `three | `four] -> 
  [< `double | `float | `int | `short ] Raw.t -> unit

(** Tell openGL the address of the color array
    Raw array must be static. *)
val color :
  [< `three | `four] ->
  [< `byte | `double | `float | `int | `short | `ubyte | `uint | `ushort ]
  Raw.t -> unit

(** Tell openGL the address of the index array
    Raw array must be static. *)
val index : [< `double | `float | `int | `short | `ubyte ] Raw.t -> unit

(** Tell openGL the address of the normal array
    Raw array must be static. *)
val normal : [< `byte | `double | `float | `int | `short ] Raw.t -> unit

(** Tell openGL the address of the vertex array
    Raw array must be static. *)
val vertex :
  [< `two | `three | `four] -> [< `double | `float | `int | `short ] Raw.t 
  -> unit

(** Tell openGL the address of to use the specified array
    Raw array must be static. *)
external enable : kind -> unit = "ml_glEnableClientState"

(** Tell openGL the address of not to use the specified array
    Raw array must be static. *)
external disable : kind -> unit = "ml_glDisableClientState"

(* GlArray.element i
   sends to openGL the element i of all enabled arrays *)
external element : int -> unit = "ml_glArrayElement"

(* GlArray.draw_arrays shape i c
   sends to openGL a GlDraw.begins shape and all the element from i to i+c-1 
   of all enabled arrays and finally do a GlDraw.ends () *)
external draw_arrays : GlDraw.shape -> first:int -> count:int -> unit
  = "ml_glDrawArrays"

(* GlArray.draw_elements shape c tbl
   sends to openGL a GlDraw.begins shape and all the element from tbl[0] to 
   tbl[c-1] of all enabled arrays and finally do a GlDraw.ends () *)
external draw_elements :
  GlDraw.shape -> count:int -> [< `ubyte | `uint | `ushort ] Raw.t -> unit
  = "ml_glDrawElements"
