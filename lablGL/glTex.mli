(* $Id: glTex.mli,v 1.8 2012-03-06 03:31:02 garrigue Exp $ *)

open Gl

val coord : s:float -> ?t:float -> ?r:float -> ?q:float -> unit -> unit
val coord2 : float * float -> unit
val coord3 : float * float * float -> unit
val coord4 : float * float * float * float -> unit

type env_param = [
    `mode of [`modulate|`decal|`blend|`replace] 
  | `color of rgba]
val env : env_param -> unit

type coord = [`s|`t|`r|`q]
type gen_param = [
    `mode of [`object_linear|`eye_linear|`sphere_map]
  | `object_plane of point4
  | `eye_plane of point4
]
val gen : coord:coord -> gen_param -> unit

type format =
    [`color_index|`red|`green|`blue|`alpha|`rgb|`bgr|`rgba|`bgra
    |`luminance|`luminance_alpha]
val image1d :
  ?proxy:bool -> ?level:int -> ?internal:int -> ?border:bool ->
  ([< format], [< kind]) GlPix.t -> unit
val image2d :
  ?proxy:bool -> ?level:int -> ?internal:int -> ?border:bool ->
  ([< format], [< kind]) GlPix.t -> unit

type filter =
    [`nearest|`linear|`nearest_mipmap_nearest|`linear_mipmap_nearest
    |`nearest_mipmap_linear|`linear_mipmap_linear]
type wrap = [`clamp|`repeat]
type parameter = [
    `min_filter of filter
  | `mag_filter of [`nearest|`linear]
  | `wrap_s of wrap
  | `wrap_t of wrap
  | `border_color of rgba
  | `priority of clampf
  | `generate_mipmap of bool
] 
val parameter : target:[`texture_1d|`texture_2d] -> parameter -> unit

type texture_id
val gen_texture : unit -> texture_id
val gen_textures : len:int -> texture_id array
val bind_texture : target:[`texture_1d|`texture_2d] -> texture_id -> unit
val delete_texture : texture_id -> unit
val delete_textures : texture_id array -> unit
