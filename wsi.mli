type cursor =
  | CURSOR_INHERIT
  | CURSOR_INFO
  | CURSOR_CYCLE
  | CURSOR_FLEUR
  | CURSOR_TEXT
;;

type winstate =
  | MaxVert
  | MaxHorz
  | Fullscreen
;;

type visiblestate =
  | Unobscured
  | PartiallyObscured
  | FullyObscured
;;

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
    method quit     : unit
  end;;

val setcursor : cursor -> unit;;
val settitle : string -> unit;;
val swapb : unit -> unit;;
val readresp : Unix.file_descr -> unit;;
val init : t -> int -> int -> int
           -> Utils.platform -> Unix.file_descr * int * int;;
val fullscreen : unit -> unit;;
val reshape : int -> int -> unit;;
val activatewin : unit -> unit;;
val mapwin : unit -> unit;;
val withalt : int -> bool;;
val withctrl : int -> bool;;
val withshift : int -> bool;;
val withmeta : int -> bool;;
val withnone : int -> bool;;
val metamask : int;;
val altmask : int;;
val shiftmask : int;;
val ctrlmask : int;;
val keyname : int -> string;;
val namekey : string -> int;;
val setwinbgcol : int -> unit;;
val keypadtodigitkey : int -> int;;
val isspecialkey : int -> bool;;
val kc2pv : int -> [> `B
                   | `Code of int
                   | `F
                   | `F1
                   | `F3
                   | `F9
                   | `G
                   | `Gt
                   | `H
                   | `Lb
                   | `Lt
                   | `N
                   | `P
                   | `Q
                   | `Rb
                   | `S
                   | `W
                   | `_0
                   | `_1
                   | `_2
                   | `_3
                   | `_4
                   | `_9
                   | `a
                   | `apos
                   | `b
                   | `backspace
                   | `c
                   | `delete
                   | `down
                   | `e
                   | `enter
                   | `equals
                   | `escape
                   | `f
                   | `g
                   | `h
                   | `home
                   | `i
                   | `insert
                   | `j
                   | `jend
                   | `k
                   | `kpdelete
                   | `kpdown
                   | `kpend
                   | `kpenter
                   | `kphome
                   | `kpleft
                   | `kpminus
                   | `kpnext
                   | `kpplus
                   | `kpprior
                   | `kpright
                   | `kpup
                   | `l
                   | `left
                   | `m
                   | `minus
                   | `n
                   | `next
                   | `o
                   | `p
                   | `pipe
                   | `plus
                   | `prior
                   | `q
                   | `question
                   | `r
                   | `right
                   | `s
                   | `slash
                   | `space
                   | `t
                   | `tab
                   | `tilde
                   | `u
                   | `up
                   | `v
                   | `w
                   | `x
                   | `y
                   | `z ];;
