(* $Id: gl.mli,v 1.23 2012-03-06 03:31:02 garrigue Exp $ *)

(* Exceptions *)

exception GLerror of string

(* Types common to all modules *)

type rgb = float * float * float
type rgba = float * float * float * float

type point2 = float * float
type point3 = float * float * float
type point4 = float * float * float * float
type vect3 = float * float *float

type clampf = float
type short = int
type kind = [`bitmap|`byte|`float|`int|`short|`ubyte|`uint|`ushort]
type real_kind = [`byte|`float|`int|`short|`ubyte|`uint|`ushort]

type format =
  [`alpha|`bgr|`bgra|`blue|`color_index|`depth_component|`green|`luminance
  |`luminance_alpha|`red|`rgb|`rgba|`stencil_index]
val format_size : [< format] -> int

type target =
  [`color_4|`index|`normal|`texture_coord_1|`texture_coord_2|`texture_coord_3
  |`texture_coord_4|`trim_2|`trim_3|`vertex_3|`vertex_4]
val target_size : [< target] -> int

type cmp_func =
  [`always|`equal|`gequal|`greater|`lequal|`less|`never|`notequal]
type face = [`back|`both|`front]

(* Basic functions *)

val flush : unit -> unit
val finish : unit -> unit

type cap =
  [`alpha_test|`auto_normal|`blend|`clip_plane0|`clip_plane1|`clip_plane2
  |`clip_plane3|`clip_plane4|`clip_plane5|`color_material|`cull_face
  |`depth_test|`dither|`fog|`light0|`light1|`light2|`light3|`light4|`light5
  |`light6|`light7|`lighting|`line_smooth|`line_stipple
  |`index_logic_op |`color_logic_op
  |`map1_color_4|`map1_index|`map1_normal|`map1_texture_coord_1
  |`map1_texture_coord_2|`map1_texture_coord_3|`map1_texture_coord_4
  |`map1_vertex_3|`map1_vertex_4|`map2_color_4|`map2_index|`map2_normal
  |`map2_texture_coord_1|`map2_texture_coord_2|`map2_texture_coord_3
  |`map2_texture_coord_4|`map2_vertex_3|`map2_vertex_4|`normalize|`point_smooth
  |`polygon_offset_fill|`polygon_offset_line|`polygon_offset_point
  |`polygon_smooth|`polygon_stipple|`scissor_test|`stencil_test|`texture_1d
  |`texture_2d|`texture_gen_q|`texture_gen_r|`texture_gen_s|`texture_gen_t]
val enable : cap -> unit
val disable : cap -> unit
val is_enabled : cap -> bool

type error =
  [`no_error|`invalid_enum|`invalid_value|`invalid_operation
  |`stack_overflow|`stack_underflow|`out_of_memory|`table_too_large]
val get_error : unit -> error
val raise_error : string -> unit
  (* raise GLerror if there is a current error, otherwise do nothing *)
