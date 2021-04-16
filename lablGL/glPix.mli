(* $Id: glPix.mli,v 1.9 2004-12-02 02:01:16 garrigue Exp $ *)

(* An abstract type for pixmaps *)

type (+'a,+'b) t

val create :
  ([< Gl.kind] as 'a) ->
  format:([< Gl.format] as 'b) -> width:int -> height:int -> ('b, 'a) t

val of_raw :
  ([< Gl.kind] as 'a) Raw.t ->
  format:([< Gl.format] as 'b) -> width:int -> height:int -> ('b, 'a) t
val to_raw : ('a, 'b) t -> 'b Raw.t
val format : ('a, 'b) t -> 'a
val width : ('a, 'b) t -> int
val height : ('a, 'b) t -> int
val raw_pos : ([< Gl.format], [< Gl.kind]) t -> x:int -> y:int -> int
    (* [raw_pos image :x :y] partially evaluates on [image] *)

(* openGL functions *)

val read :
  x:int ->
  y:int ->
  width:int ->
  height:int ->
  format:([< Gl.format] as 'a) -> kind:([< Gl.kind] as 'b) -> ('a, 'b) t

type bitmap = ([`color_index], [`bitmap]) t
val bitmap :
  bitmap -> orig:Gl.point2 -> move:Gl.point2 -> unit

val draw : ([< Gl.format], [< Gl.kind]) t -> unit

type map =
  [`a_to_a|`b_to_b|`g_to_g|`i_to_a|`i_to_b
  |`i_to_g|`i_to_i|`i_to_r|`r_to_r|`s_to_s]
val map : map -> [`float] Raw.t -> unit

type store_param = [
    `pack_swap_bytes of bool
  | `pack_lsb_first of bool
  | `pack_row_length of int
  | `pack_skip_pixels of int
  | `pack_skip_rows of int
  | `pack_alignment of int
  | `unpack_swap_bytes of bool
  | `unpack_lsb_first of bool
  | `unpack_row_length of int
  | `unpack_skip_pixels of int
  | `unpack_skip_rows of int
  | `unpack_alignment of int
]
val store : store_param -> unit

type transfer_param = [
    `map_color of bool
  | `map_stencil of bool
  | `index_shift of int
  | `index_offset of int
  | `red_scale of float
  | `red_bias of float
  | `green_scale of float
  | `green_bias of float
  | `blue_scale of float
  | `blue_bias of float
  | `alpha_scale of float
  | `alpha_bias of float
  | `depth_scale of float
  | `depth_bias of float
]
val transfer : transfer_param -> unit

val zoom : x:float -> y:float -> unit
val raster_pos : x:float -> y:float -> ?z:float -> ?w:float -> unit -> unit

val copy :
  x:int ->
  y:int -> width:int -> height:int -> buffer:[`color|`depth|`stencil] -> unit
