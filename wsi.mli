type cursor =
    | CURSOR_INHERIT
    | CURSOR_INFO
    | CURSOR_CYCLE
    | CURSOR_CROSSHAIR
    | CURSOR_TEXT
;;

class type t = object
  method display : unit
  method expose : unit
  method reshape : int -> int -> unit
  method mouse : int -> bool -> int -> int -> int -> unit
  method motion : int -> int -> unit
  method pmotion : int -> int -> unit
  method key : int -> int -> unit
  method enter : int -> int -> unit
  method leave : unit
  method quit : unit
end;;

val setcursor : cursor -> unit;;
val settitle : string -> unit;;
val swapb : unit -> unit;;
val readresp : Unix.file_descr -> unit;;
val init : t -> int -> int -> bool -> Unix.file_descr * int * int;;
val fullscreen : unit -> unit;;
val reshape : int -> int -> unit;;
val activatewin : unit -> unit;;
val withalt : int -> bool;;
val withctrl : int -> bool;;
val withshift : int -> bool;;
val withmeta : int -> bool;;
val withnone : int -> bool;;
val toutf8 : int -> string;;
val metamask : int;;
val altmask : int;;
val shiftmask : int;;
val ctrlmask : int;;
val keyname : int -> string;;
val namekey : string -> int;;
val tempfailureretry : ('a -> 'b) -> 'a -> 'b;;
