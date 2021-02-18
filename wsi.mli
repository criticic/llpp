type cursor =
  | CURSOR_INHERIT
  | CURSOR_INFO
  | CURSOR_CYCLE
  | CURSOR_FLEUR
  | CURSOR_TEXT

type winstate =
  | MaxVert
  | MaxHorz
  | Fullscreen

type visiblestate =
  | Unobscured
  | PartiallyObscured
  | FullyObscured

class type t =
  object
    method display  : unit
    method map      : bool -> unit
    method expose   : unit
    method visible  : visiblestate -> unit
    method reshape  : int -> int -> unit
    method mouse    : int -> bool -> int -> int -> int -> unit
    method motion   : int -> int -> unit
    method pmotion  : int -> int -> unit
    method key      : int -> int -> unit
    method enter    : int -> int -> unit
    method leave    : unit
    method winstate : winstate list -> unit
    method quit     : 'a. 'a
    method scroll   : int -> int -> unit
    method zoom     : float -> int -> int -> unit
    method opendoc  : string -> unit
  end

type keycode = int
val setcursor : cursor -> unit
val settitle : string -> unit
val swapb : unit -> unit
val readresp : Unix.file_descr -> unit
val init : t -> int -> int -> Unix.file_descr * int * int
val fullscreen : unit -> unit
val reshape : int -> int -> unit
val activatewin : unit -> unit
val mapwin : unit -> unit
val withalt : int -> bool
val withctrl : int -> bool
val withshift : int -> bool
val withmeta : int -> bool
val withnone : int -> bool
val metamask : int
val altmask : int
val shiftmask : int
val ctrlmask : int
val keyname : int -> string
val namekey : string -> int
val fontsizescale : int -> int
val ks2kt : int -> Keys.t
val cAp : string
