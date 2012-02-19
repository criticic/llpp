type cursor =
    | CURSOR_INHERIT
    | CURSOR_INFO
    | CURSOR_CYCLE
    | CURSOR_CROSSHAIR
    | CURSOR_TEXT
;;

class type t = object
  method display : unit
  method reshape : int -> int -> unit
  method mouse : int -> bool -> int -> int -> int -> unit
  method motion : int -> int -> unit
  method pmotion : int -> int -> unit
  method key : int -> int -> unit
end;;

val setcursor : cursor -> unit;;
val settitle : string -> unit;;
val swapb : unit -> unit;;
val readresp : Unix.file_descr -> unit;;
val init : t -> Unix.file_descr;;
val fullscreen : unit -> unit;;
val reshape : int -> int -> unit;;
val withalt : int -> bool;;
val withctrl : int -> bool;;
val withshift : int -> bool;;
val toutf8 : int -> string;;

exception Quit;;
