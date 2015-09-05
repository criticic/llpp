(* $Id: glClear.ml,v 1.5 2000-04-12 07:40:23 garrigue Exp $ *)

open Gl

external accum : float -> float -> float -> float -> unit
    = "ml_glClearAccum"
let accum ?(alpha=1.) (r,g,b : rgb) =
  accum r g b alpha

type buffer = [`color|`depth|`accum|`stencil]
external clear : buffer list -> unit = "ml_glClear"

external color :
    red:float -> green:float -> blue:float -> alpha:float -> unit
    = "ml_glClearColor"
let color ?(alpha=1.) (red, green, blue : rgb) =
  color ~red ~green ~blue ~alpha
external depth : clampf -> unit = "ml_glClearDepth"
external index : float -> unit = "ml_glClearIndex"
external stencil : int -> unit = "ml_glClearStencil"
