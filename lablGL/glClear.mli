(* $Id: glClear.mli,v 1.3 1999-11-15 09:55:05 garrigue Exp $ *)

type buffer = [`accum|`color|`depth|`stencil]
val clear : buffer list -> unit
    (* glClear: clear the specified buffers *)

val accum : ?alpha:float -> Gl.rgb -> unit
val color : ?alpha:float -> Gl.rgb -> unit
val depth : Gl.clampf -> unit
val index : float -> unit
val stencil : int -> unit
    (* Set the clear value for each buffer: glClearAccum etc *)
