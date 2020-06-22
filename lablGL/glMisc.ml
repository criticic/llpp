(* $Id: glMisc.ml,v 1.8 2008-10-25 02:22:58 garrigue Exp $ *)

open StdLabels

external get_string : [`vendor|`renderer|`version|`extensions] -> string
    = "ml_glGetString"

let rec check_substring ~sep ~start ~buf s =
  let len = String.length s in
  if String.length buf < len + start then false else
  if String.sub buf ~pos:start ~len = s &&
    (String.length buf = len + start || buf.[len+start] = sep) then true
  else match
    try Some (String.index_from buf start sep) with Not_found -> None
  with
  | None -> false
  | Some n -> check_substring ~sep ~start:(n+1) ~buf s

let check_extension s =
  check_substring ~sep:' ' ~start:0 ~buf:(get_string `extensions) s

type equation = float * float * float * float
external clip_plane : plane:int -> equation -> unit
    = "ml_glClipPlane"
let clip_plane ~plane equation =
  if plane < 0 || plane > 5 then invalid_arg "Gl.clip_plane";
  clip_plane ~plane equation

type hint_target =
    [`fog|`line_smooth|`perspective_correction|`point_smooth|`polygon_smooth]
external hint : hint_target -> [`fastest|`nicest|`dont_care] -> unit
    = "ml_glHint"

external init_names : unit -> unit = "ml_glInitNames"
external load_name : int -> unit = "ml_glLoadName"
external pop_name : unit -> unit = "ml_glPopName"
external push_name : int -> unit = "ml_glPushName"

external pop_attrib : unit -> unit = "ml_glPopAttrib"
type attrib =
    [ `accum_buffer|`color_buffer|`current|`depth_buffer|`enable|`eval|`fog
    | `hint|`lighting|`line|`list|`pixel_mode|`point|`polygon|`polygon_stipple
    | `scissor|`stencil_buffer|`texture|`transform|`viewport ]
external push_attrib : attrib list -> unit = "ml_glPushAttrib"

external pass_through : float -> unit = "ml_glPassThrough"
external render_mode : [`render|`select|`feedback] -> int = "ml_glRenderMode"
external select_buffer : int -> [`uint] Raw.t -> unit = "ml_glSelectBuffer"
let select_buffer raw =
  if not (Raw.static raw) then
    invalid_arg "GlMisc.select_buffer : buffer must be static";
  select_buffer (Raw.length raw) raw
type feedback_mode =
    [`_2d |`_3d |`_3d_color |`_3d_color_texture |`_4d_color_texture]
external feedback_buffer : int -> feedback_mode -> [`float] Raw.t -> unit
  = "ml_glFeedbackBuffer"
let feedback_buffer ~mode buf =
  if not (Raw.static buf) then
    invalid_arg "GlMisc.feedback_buffer : buffer must be static";
  feedback_buffer (Raw.length buf) mode buf

external scissor : x:int -> y:int -> width:int -> height:int -> unit
  = "ml_glScissor"
