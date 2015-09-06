(* $Id: raw.mli,v 1.10 2007-04-13 02:48:43 garrigue Exp $ *)

(* This module provides a direct way to access C arrays of basic types.
   This is particularly useful when one wants to avoid costly
   conversions between ML and C representations. *)

type (+'a) t

type kind =
    [`bitmap|`byte|`double|`float|`int|`long|`short
    |`ubyte|`uint|`ulong|`ushort]
    (* Supported element types. [bitmap] is equivalent to [ubyte] but
       allows user modules to distinguish between them *)
type fkind = [`double|`float]
type ikind = [`bitmap|`byte|`int|`long|`short|`ubyte|`uint|`ulong|`ushort]
type lkind = [`int|`long|`uint|`ulong]

val create : ([< kind] as 'a) -> len:int -> 'a t
    (* [create t :len] returns a new raw array of C type t
       and length len. This array is managed by the GC *)
val create_static : ([< kind] as 'a) -> len:int -> 'a t
    (* [create_static t :len] returns a new raw array of C type t
       and length len. This array is created through malloc.
       You must free it explicitely *)
val free_static : 'a t -> unit
    (* Free a raw array created through create_static *)

val kind : 'a t -> 'a
    (* Returns the type of a free array. Beware of the influence on the
       type system: you probably want to write [(kind raw :> kind)] *)
val byte_size : 'a t -> int
    (* The size of the array in bytes. That is (sizeof t * len)
       where t and len are the parameters to create *)
val static : 'a t -> bool
    (* Wether this array was statically allocated or not *)
val cast : 'a t -> kind:([< kind] as 'b) -> 'b t
    (* Change the type of a raw array *)

external sizeof : [< kind] -> int = "ml_raw_sizeof"
    (* [sizeof t] returns the physical size of t in bytes *)
val length : [< kind] t -> int
    (* [length raw] returns the length of raw array according to
       its contents type *)
val sub : ([< kind] t as 'a) -> pos:int -> len:int -> 'a
    (* returns the slice of length len starting at position pos *)

(* The following functions access raw arrays in the intuitive way.
   They raise [Invalid_argument] when access is attempted out of
   bounds *)

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

(* Simultaneous access versions are much more efficient than individual
   access, the overhead being paid only once *)

val gets : [< ikind] t -> pos:int -> len:int -> int array
val sets : [< ikind] t -> pos:int -> int array -> unit
val gets_float : [< fkind] t -> pos:int -> len:int -> float array
val sets_float : [< fkind] t -> pos:int -> float array -> unit

(* Fastest version: simply copy the contents of the array to and from
   a string *)

val gets_string : 'a t -> pos:int -> len:int -> string
val sets_string : 'a t -> pos:int -> string -> unit

(* Abbreviations to create raw arrays from ML arrays and strings *)

val of_array : int array -> kind:([< ikind] as 'a) -> 'a t
val of_float_array : float array -> kind:([< fkind] as 'a) -> 'a t
val of_string : string -> kind:([< kind] as 'a) -> 'a t
val of_matrix : float array array -> kind:([< fkind] as 'a) -> 'a t
