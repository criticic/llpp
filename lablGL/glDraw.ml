(* $Id: glDraw.ml,v 1.6 2007-04-13 01:17:50 garrigue Exp $ *)

open Gl

external color :
    red:float -> green:float -> blue:float -> alpha:float -> unit
    = "ml_glColor4d"
let color ?(alpha=1.) (red, green, blue : rgb) =
  color ~red ~green ~blue ~alpha

external index : float -> unit = "ml_glIndexd"

external cull_face : face -> unit = "ml_glCullFace"
external edge_flag : bool -> unit = "ml_glEdgeFlag"
external front_face : [`cw|`ccw] -> unit = "ml_glFrontFace"

external line_width : float -> unit = "ml_glLineWidth"
external line_stipple : factor:int -> pattern:short -> unit
    = "ml_glLineStipple"
let line_stipple ?(factor=1) pattern =
  line_stipple ~factor ~pattern
external point_size : float -> unit = "ml_glPointSize"

external polygon_offset : factor:float -> units:float -> unit
    = "ml_glPolygonOffset"
external polygon_mode : face:face -> [`point|`line|`fill] -> unit
    = "ml_glPolygonMode"
external polygon_stipple : [`bitmap] Raw.t -> unit = "ml_glPolygonStipple"
let polygon_stipple (img : GlPix.bitmap) =
  if GlPix.height img <> 32 || GlPix.width img <> 32
  then invalid_arg "GlDraw.polygon_stipple";
  polygon_stipple (GlPix.to_raw img)

external shade_model : [`flat|`smooth] -> unit = "ml_glShadeModel"

type shape =
    [ `points | `lines | `polygon | `triangles | `quads | `line_strip
    | `line_loop | `triangle_strip | `triangle_fan | `quad_strip ]
external begins : shape -> unit = "ml_glBegin"
external ends : unit -> unit = "ml_glEnd"

external normal : x:float -> y:float -> z:float -> unit
    = "ml_glNormal3d"
let normal ?(x=0.) ?(y=0.) ?(z=0.) () = normal ~x ~y ~z
and normal3 (x,y,z) = normal ~x ~y ~z

external rect : point2 -> point2 -> unit = "ml_glRectd"

external vertex : x:float -> y:float -> ?z:float -> ?w:float -> unit -> unit
    = "ml_glVertex"
let vertex2 (x,y : point2) = vertex ~x ~y ()
and vertex3 (x,y,z : point3) = vertex ~x ~y ~z ()
and vertex4 (x,y,z,w : point4) = vertex ~x ~y ~z ~w ()

external viewport : x:int -> y:int -> w:int -> h:int -> unit
    = "ml_glViewport"
