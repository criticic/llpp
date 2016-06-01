(* let post_key key mask = *)
(*   Marshal.to_channel chan (KEY (key, mask)); *)

(* let readresp fd = *)
(*   let ic = Hashtbl.find fd . in *)
(*   Marshal. *)

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

class type t = object
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
end

type message =
  | KeyDown of int * int

let input_message ic : message =
  Marshal.from_channel ic

let output_message (m : message) oc =
  Marshal.to_channel oc

type state =
  {
    mutable ic: in_channel;
    mutable oc: out_channel;
    mutable t: t;
  }

let onot = object
  method display         = ()
  method map _           = ()
  method expose          = ()
  method visible _       = ()
  method reshape _ _     = ()
  method mouse _ _ _ _ _ = ()
  method motion _ _      = ()
  method pmotion _ _     = ()
  method key _ _         = ()
  method enter _ _       = ()
  method leave           = ()
  method winstate _      = ()
  method quit            = exit 0
end

let state =
  {
    ic = stdin;
    oc = stdout;
    t = onot;
  }

let setcursor _ = ()

let settitle _ = ()

let swapb () = ()

let post_key_down key mask =
  output_message (KeyDown (key, mask)) state.oc

let readresp _ =
  match input_message state.ic with
  | KeyDown (key, mask) -> state.t#key key mask

let init t _ w h platform =
  ()

let fullscreen () = ()

let reshape _ _ = ()

let activatewin () = ()

let mapwin () = ()

let metamask = 1 lsl 19

let altmask = 1 lsl 19

let shiftmask = 1 lsl 17

let ctrlmask = 1 lsl 18

let withalt mask = mask land metamask != 0

let withctrl mask = mask land ctrlmask != 0

let withshift mask = mask land shiftmask != 0

let withmeta mask = mask land metamask != 0

let withnone mask = mask land (altmask + ctrlmask + shiftmask + metamask) = 0

let keyname _ = ""

let namekey _ = 0

let setwinbgcol _ = ()
