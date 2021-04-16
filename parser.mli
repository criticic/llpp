exception Parse_error of string * string * int
val parse_error : string -> string -> int -> 'a
val enent : string -> int -> int -> string
val unent : Buffer.t -> string -> int -> int -> unit
val subs : string -> int -> string
val ts :
  [< `close | `comment | `doctype | `exclam | `lt | `question | `tag | `text
  ] -> string
type attr = string * string
and attrs = attr list
and vp =
    Vdata
  | Vcdata
  | Vopen of string * attrs * bool
  | Vclose of string
  | Vend
and 'a v = { f : 'a v -> vp -> int -> int -> 'a v; accu : 'a; }
val parse : 'a v -> string -> 'a
