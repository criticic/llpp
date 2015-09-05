(* $Id: raw.ml,v 1.9 2007-04-13 02:48:43 garrigue Exp $ *)

type addr
type kind =
    [`bitmap|`byte|`double|`float|`int|`long|`short
    |`ubyte|`uint|`ulong|`ushort]
type fkind = [`double|`float]
type ikind = [`bitmap|`byte|`int|`long|`short|`ubyte|`uint|`ulong|`ushort]
type lkind = [`int|`long|`uint|`ulong]
type 'a t =
    { kind: 'a; base: addr; offset: int; size: int; static: bool}

let kind raw = raw.kind
let byte_size raw = raw.size
let static raw = raw.static
let cast raw ~kind =
  { kind = kind; size = raw.size; base = raw.base;
    offset = raw.offset; static = raw.static }

external sizeof : [< kind] -> int = "ml_raw_sizeof"
let length raw = raw.size / sizeof raw.kind
let sub raw ~pos ~len =
  let size = sizeof raw.kind in
  if pos < 0 or (pos+len) * size > raw.size then invalid_arg "Raw.sub";
  { raw with offset = raw.offset + pos * size; size = len * size }

external get : [< ikind] t -> pos:int -> int = "ml_raw_get"
external set : [< ikind] t -> pos:int -> int -> unit = "ml_raw_set"
external get_float : [< fkind] t -> pos:int -> float = "ml_raw_get_float"
external set_float : [< fkind] t -> pos:int -> float -> unit
    = "ml_raw_set_float"
external get_hi : [< lkind] t -> pos:int -> int = "ml_raw_get_hi"
external set_hi : [< lkind] t -> pos:int -> int -> unit = "ml_raw_set_hi"
external get_lo : [< lkind] t -> pos:int -> int = "ml_raw_get_lo"
external set_lo : [< lkind] t -> pos:int -> int -> unit = "ml_raw_set_lo"
external get_long : [< lkind] t -> pos:int -> nativeint = "ml_raw_get_long"
external set_long : [< lkind] t -> pos:int -> nativeint -> unit
    = "ml_raw_set_long"

external gets : [< ikind] t -> pos:int -> len:int -> int array
    = "ml_raw_read"
external gets_string : 'a t -> pos:int -> len:int -> string
    = "ml_raw_read_string"
external gets_float : [< fkind] t -> pos:int -> len:int -> float array
    = "ml_raw_read_float"
external sets : [< ikind] t -> pos:int -> int array -> unit = "ml_raw_write"
external sets_string : 'a t -> pos:int -> string -> unit
    = "ml_raw_write_string"
external sets_float : [< fkind] t -> pos:int -> float array -> unit
    = "ml_raw_write_float"

(*
external fill : [< ikind] t -> pos:int -> len:int -> unit = "ml_raw_fill"
external fill_float : [< fkind] t -> pos:int -> len:int -> unit
    = "ml_raw_fill_float"
*)

external create : ([< kind] as 'a) -> len:int -> 'a t = "ml_raw_alloc"
external create_static : ([< kind] as 'a) -> len:int -> 'a t
    = "ml_raw_alloc_static"
external free_static : 'a t -> unit = "ml_raw_free_static"

let of_array arr ~kind =
  let raw = create kind ~len:(Array.length arr) in
  sets raw ~pos:0 arr;
  raw
let of_float_array arr ~kind =
  let raw = create kind ~len:(Array.length arr) in
  sets_float raw ~pos:0 arr;
  raw
let of_string s ~kind =
  let raw = create kind ~len:(String.length s) in
  sets_string raw ~pos:0 s;
  raw
let of_matrix mat ~kind =
  let h = Array.length mat in
  if h = 0 then invalid_arg "Raw.of_matrix";
  let w = Array.length mat.(0) in
  let raw = create kind ~len:(h*w) in
  for i = 0 to h - 1 do
    if Array.length mat.(i) <> w then invalid_arg "Raw.of_matrix";
    sets_float raw ~pos:(i*w) mat.(i)
  done;
  raw
