(* $Id: glMisc.mli,v 1.6 2008-10-25 02:22:58 garrigue Exp $ *)

(* Getting information *)
val get_string : [`vendor|`renderer|`version|`extensions] -> string
val check_extension : string -> bool

(* Clipping planes *)
type equation = float * float * float * float
val clip_plane : plane:int -> equation -> unit

(* Speed hint *)
type hint_target =
    [`fog|`line_smooth|`perspective_correction|`point_smooth|`polygon_smooth]
val hint : hint_target -> [`fastest|`nicest|`dont_care] -> unit

(* Names *)
val init_names : unit -> unit
val load_name : int -> unit
val push_name : int -> unit
val pop_name : unit -> unit

type attrib =
    [ `accum_buffer|`color_buffer|`current|`depth_buffer|`enable|`eval|`fog
    | `hint|`lighting|`line|`list|`pixel_mode|`point|`polygon|`polygon_stipple
    | `scissor|`stencil_buffer|`texture|`transform|`viewport ]
val push_attrib : attrib list -> unit
val pop_attrib : unit -> unit

val render_mode : [`feedback|`render|`select] -> int
val pass_through : float -> unit
val select_buffer : [`uint] Raw.t -> unit
  (* argument must be a static Raw.t *)
type feedback_mode =
    [`_2d |`_3d |`_3d_color |`_3d_color_texture |`_4d_color_texture]
val feedback_buffer : mode:feedback_mode -> [`float] Raw.t -> unit
  (* argument must be a static Raw.t *)

val scissor : x:int -> y:int -> width:int -> height:int -> unit
