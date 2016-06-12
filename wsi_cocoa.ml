open Utils;;

let debug = true

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

let onot = object
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

type state =
  {
    mutable t: t;
    mutable fd: Unix.file_descr;
  }

let state =
  {
    t = onot;
    fd = Unix.stdin;
  }

let readstr sock n = try readstr sock n with End_of_file -> state.t#quit; assert false

let setcursor _ = ()

external settitle: string -> unit = "ml_settitle"

external swapb: unit -> unit = "ml_swapb"

external reshape: int -> int -> unit = "ml_reshape"

(* 0 -> swapb *)

(* 0 -> map
   1 -> expose
   2 -> visible
   3 -> reshape
   4 -> mouse
   5 -> motion
   6 -> pmotion
   7 -> key
   8 -> enter
   9 -> leave
   10 -> winstate
   11 -> quit
   13 -> response *)

let readresp sock =
  let resp = readstr sock 32 in
  let opcode = r8 resp 0 in
  match opcode with
  | 0 ->
    let mapped = r8 resp 16 <> 0 in
    vlog "map %B" mapped;
    state.t#map mapped
  | 1 ->
    vlog "expose";
    state.t#expose
  | 3 ->
    let w = r16 resp 16 in
    let h = r16 resp 18 in
    vlog "reshape width %d height %d" w h;
    state.t#reshape w h
  | 7 ->
    let key = r32 resp 16 in
    let mask = r32 resp 20 in
    vlog "keydown key %d mask %d" key mask;
    state.t#key key mask
  | 8 ->
    let x = r16 resp 16 in
    let y = r16 resp 18 in
    vlog "enter x %d y %d" x y;
    state.t#enter x y
  | 9 ->
    vlog "leave";
    state.t#leave
  | 11 ->
    vlog "quit";
    state.t#quit
  | _ ->
    vlog "unknown server message %d" opcode

external completeinit: int -> int -> unit = "ml_completeinit"

external file_descr_of_int: int -> Unix.file_descr = "%identity"

external getw: unit -> int = "ml_getw"
external geth: unit -> int = "ml_geth"

let init t _ w h platform =
  let fd = int_of_string (Sys.getenv "LLPP_DISPLAY") in
  Printf.eprintf "LLPP_DISPLAY=%d\n%!" fd;
  let fd = file_descr_of_int fd in
  state.t <- t;
  state.fd <- fd;
  completeinit w h;
  fd, getw (), geth ()

external fullscreen: unit -> unit = "ml_fullscreen"

let activatewin () = ()

external mapwin: unit -> unit = "ml_mapwin"

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

external setwinbgcol: int -> unit = "ml_setwinbgcol"
