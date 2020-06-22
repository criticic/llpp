(* $Id: glTex.ml,v 1.14 2012-03-06 03:31:02 garrigue Exp $ *)

open Gl
open GlPix

external coord1 : float -> unit = "ml_glTexCoord1d"
external coord2 : float -> float -> unit = "ml_glTexCoord2d"
external coord3 : float -> float -> float -> unit = "ml_glTexCoord3d"
external coord4 : float -> float -> float -> float -> unit
    = "ml_glTexCoord4d"

(*external multi_coord2 : *)
    
let default x = function Some x -> x | None -> x
let coord ~s ?t ?r ?q () =
  match q with
    Some q -> coord4 s (default 0.0 t) (default 0.0 r) q
  | None -> match r with
      Some r -> coord3 s (default 0.0 t) r
    | None -> match t with
	Some t -> coord2 s t
      |	None -> coord1 s
let coord2 (s,t) = coord2 s t
let coord3 (s,t,r) = coord3 s t r
let coord4 (s,t,r,q) = coord4 s t r q
type env_param = [
    `mode of [`modulate|`decal|`blend|`replace] 
  | `color of rgba
]
external env : env_param -> unit = "ml_glTexEnv"
type coord = [`s|`t|`r|`q]
type gen_param = [
    `mode of [`object_linear|`eye_linear|`sphere_map]
  | `object_plane of point4
  | `eye_plane of point4
]
external gen : coord:coord -> gen_param -> unit = "ml_glTexGen"

let npot = ref None

let check_pow2 n =
  if !npot = None then
    npot := Some (GlMisc.check_extension "GL_ARB_texture_non_power_of_two");
  (!npot = Some true) || (n land (n - 1) = 0)

type format = [
    `color_index
  | `red
  | `green
  | `blue
  | `alpha
  | `rgb
  | `bgr
  | `rgba
  | `bgra
  | `luminance
  | `luminance_alpha
]

external image1d :
    proxy:bool -> level:int -> internal:int ->
    width:int -> border:int -> format:[< format] -> [< kind] Raw.t -> unit
    = "ml_glTexImage1D_bc""ml_glTexImage1D"
let image1d ?(proxy=false) ?(level=0) ?internal:i ?(border=false) img =
  let internal = match i with None -> format_size (format img) | Some i -> i in
  let border = if border then 1 else 0 in
  if not (check_pow2 (width img - 2 * border)) then
    raise (GLerror "Gl.image1d : bad width");
  if height img < 1 then raise (GLerror "Gl.image1d : bad height");
  image1d ~proxy ~level ~internal ~width:(width img) ~border
    ~format:(format img) (to_raw img)
external image2d :
    proxy:bool -> level:int -> internal:int -> width:int ->
    height:int -> border:int -> format:[< format] -> [< kind] Raw.t -> unit
    = "ml_glTexImage2D_bc""ml_glTexImage2D"
let image2d ?(proxy=false) ?(level=0) ?internal:i ?(border=false) img =
  let internal = match i with None -> format_size (format img) | Some i -> i in
  let border = if border then 1 else 0 in
  if not (check_pow2 (width img - 2 * border)) then
    raise (GLerror "Gl.image2d : bad width");
  if not (check_pow2 (height img - 2 * border)) then
    raise (GLerror "Gl.image2d : bad height");
  image2d ~proxy ~level ~internal ~border
    ~width:(width img) ~height:(height img) ~format:(format img) (to_raw img)
type filter = [
    `nearest
  | `linear
  | `nearest_mipmap_nearest
  | `linear_mipmap_nearest
  | `nearest_mipmap_linear
  | `linear_mipmap_linear
]
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
external parameter : target:[`texture_1d|`texture_2d] -> parameter -> unit
    = "ml_glTexParameter"

type texture_id = nativeint
external _gen_textures : int -> [`uint] Raw.t -> unit = "ml_glGenTextures"
let gen_textures ~len =
  let raw = Raw.create `uint ~len in
  _gen_textures len raw;
  let arr = Array.make len Nativeint.zero in
  for i = 0 to len - 1 do
    arr.(i) <- Raw.get_long raw ~pos:i
  done;
  arr
let gen_texture () =  (gen_textures 1).(0)

external bind_texture : target:[`texture_1d|`texture_2d] -> texture_id -> unit
    = "ml_glBindTexture"
external delete_texture : texture_id -> unit = "ml_glDeleteTexture"
let delete_textures a = Array.iter (fun id -> delete_texture id) a
