exception Quit
module E :
  sig
    val s : string
    val b : bytes
    val a : 'a array
    val j : int * float * float
  end
val tempfailureretry : ('a -> 'b) -> 'a -> 'b
external spawn : string -> (Unix.file_descr * int) list -> int = "ml_spawn"
external hasdata : Unix.file_descr -> bool = "ml_hasdata"
val now : unit -> float
val dologf : (string -> unit) ref
val dolog : ('a, unit, string, unit) format4 -> 'a
val dolog1 : ('a, unit, string, unit) format4 -> 'a
val exntos : exn -> string
val onoffs : bool -> string
val error : ('a, unit, string, 'b) format4 -> 'a
module IntSet : Set.S with type elt = int
val emptystr : string -> bool
val nonemptystr : string -> bool
val bound : 'a -> 'a -> 'a -> 'a
module Opaque :
  sig
    type t = private string
    val of_string : string -> t
    val to_string : t -> string
  end
val ( ~< ) : string -> Opaque.t
val ( ~> ) : Opaque.t -> string
val int_of_string_with_suffix : string -> int
val string_with_suffix_of_int : int -> string
val color_of_string : string -> float * float * float
val rgba_of_string : string -> float * float * float * float
val color_to_string : float * float * float -> string
val rgba_to_string : float * float * float * float -> string
val abspath : string -> string
module Ne :
  sig
    val index : string -> char -> int
    val clo : Unix.file_descr -> (string -> unit) -> unit
  end
val getenvdef : string -> string -> string
module Re :
  sig
    val crlf : Str.regexp
    val percent : Str.regexp
    val whitespace : Str.regexp
  end
val addchar : string -> char -> string
val btod : bool -> int
val splitatchar : string -> char -> string * string
val boundastep : int -> int -> int
val withoutlastutf8 : string -> string
val fdcontents : Unix.file_descr -> string
val filecontents : string -> string
val getcmdoutput : (string -> unit) -> string -> string
val geturl : string -> string
val substratis : string -> int -> string -> bool
val w8 : bytes -> int -> int -> unit
val r8 : bytes -> int -> int
val w16 : bytes -> int -> int -> unit
val r16 : bytes -> int -> int
val r16s : bytes -> int -> int
val w32 : bytes -> int -> int -> unit
val r32 : bytes -> int -> int
val r32s : bytes -> int -> int
val vlogf : (string -> unit) ref
val vlog : ('a, unit, string, unit) format4 -> 'a
val pipef :
  ?closew:bool -> string -> (Unix.file_descr -> unit) -> string -> unit
val selstring : string -> string -> unit
