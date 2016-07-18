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

let classify_key key =
  let open Keys in
  match key with
  | 0xF704 -> F1
  | 0xF706 -> F3
  | 0xF70C -> F9
  | 62 -> Gt
  | 91 -> Lb
  | 60 -> Lt
  | 93 -> Rb
  | 39 -> Apos
  | 8 -> Backspace
  | 0x7f -> Delete
  | 0xF701 -> Down
  | 13 -> Enter
  | 61 -> Equals
  | 27 -> Escape
  | 0xF729 -> Home
  | 0xF727 -> Insert
  | 0xF72B -> Jend
  | 46 -> KPdelete
  | 0xfff01 -> KPdown
  | 0xfff02 -> KPend
  | 0xfff03 -> KPenter
  | 0xfff04 -> KPhome
  | 0xfff05 -> KPleft
  | 0xfff06 -> KPminus
  | 0xfff07 -> KPnext
  | 0xfff08 -> KPplus
  | 0xfff09 -> KPprior
  | 0xfff0a -> KPright
  | 0xfff0b -> KPup
  | 0xF702 -> Left
  | 45 -> Minus
  | 0xF72D -> Next
  | 124 -> Pipe
  | 43 -> Plus
  | 0xF72C -> Prior
  | 63 -> Question
  | 0xF703 -> Right
  | 47 -> Slash
  | 32 -> Space
  | 9 -> Tab
  | 0x7e -> Tilde
  | 0xF700 -> Up
  | 48 -> N0
  | 49 -> N1
  | 50 -> N2
  | 51 -> N3
  | 52 -> N4
  | 57 -> N9
  | 66 -> CB
  | 70 -> CF
  | 71 -> CG
  | 72 -> CH
  | 78 -> CN
  | 80 -> CP
  | 81 -> CQ
  | 87 -> CW
  | 83 -> CS
  | 97 -> Ca
  | 98 -> Cb
  | 99 -> Cc
  | 101 -> Ce
  | 102 -> Cf
  | 103 -> Cg
  | 104 -> Ch
  | 105 -> Ci
  | 106 -> Cj
  | 107 -> Ck
  | 108 -> Cl
  | 109 -> Cm
  | 110 -> Cn
  | 111 -> Co
  | 112 -> Cp
  | 113 -> Cq
  | 114 -> Cr
  | 115 -> Cs
  | 116 -> Ct
  | 117 -> Cu
  | 118 -> Cv
  | 119 -> Cw
  | 120 -> Cx
  | 121 -> Cy
  | 122 -> Cz
  | n -> Code n
;;

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
  method quit     : 'a. 'a
  method scroll   : int -> int -> unit
  method zoom     : float -> unit
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
  method quit: 'a. 'a    = exit 0
  method scroll _ _      = ()
  method zoom _          = ()
end

type state =
  {
    mutable t: t;
    mutable fd: Unix.file_descr;
    buf: bytes;
    mutable off: int;
  }

let state =
  {
    t = onot;
    fd = Unix.stdin;
    buf = Bytes.create 512;
    off = 0;
  }

external setcursor: cursor -> unit = "ml_setcursor"
external settitle: string -> unit = "ml_settitle"
external swapb: unit -> unit = "ml_swapb"
external reshape: int -> int -> unit = "ml_reshape"
external makecurrentcontext: unit -> unit = "ml_makecurrentcontext"
external getw: unit -> int = "ml_getw"
external geth: unit -> int = "ml_geth"
external get_server_fd: unit -> Unix.file_descr = "ml_get_server_fd"
external get_backing_scale_factor: unit -> int = "ml_get_backing_scale_factor"
external fullscreen: unit -> unit = "ml_fullscreen"
external mapwin: unit -> unit = "ml_mapwin"
external setwinbgcol: int -> unit = "ml_setwinbgcol"

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
   12 -> scroll
   13 -> zoom *)

let handleresp resp =
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
  | 4 ->
    let down = r16 resp 10 <> 0 in
    let b = r32 resp 12 in
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    let m = r32 resp 24 in
    vlog "mouse %s b %d x %d y %d m 0x%x" (if down then "down" else "up") b x y m;
    state.t#mouse b down x y m
  | 5 ->
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    let m = r32 resp 24 in
    vlog "motion x %d y %d m 0x%x" x y m;
    state.t#motion x y
  | 6 ->
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    let m = r32 resp 24 in
    vlog "pmotion x %d y %d m 0x%x" x y m;
    state.t#pmotion x y
  | 7 ->
    let key = r32 resp 16 in
    let mask = r32 resp 20 in
    vlog "keydown key %d mask %d" key mask;
    state.t#key key mask
  | 8 ->
    let x = r16s resp 16 in
    let y = r16s resp 20 in
    vlog "enter x %d y %d" x y;
    state.t#enter x y
  | 9 ->
    vlog "leave";
    state.t#leave
  | 10 ->
    let x = r32 resp 16 <> 0 in
    vlog "winstate %B" x;
    state.t#winstate (if x then [Fullscreen] else []);
  | 11 ->
    vlog "quit";
    state.t#quit
  | 12 ->
      let dx = r32s resp 16 in
      let dy = r32s resp 20 in
      vlog "scroll dx %d dy %d" dx dy;
      state.t#scroll dx dy
  | 13 ->
      let z = float (r32s resp 16) /. 1000.0 in
      vlog "zoom z %f" z;
      state.t#zoom z
  | _ ->
    vlog "unknown server message %d" opcode

let readresp sock =
  let len =
    match Unix.read sock state.buf state.off (Bytes.length state.buf - state.off) with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> state.off
    | 0 -> state.t#quit
    | n -> state.off + n
  in
  let rec loop off =
    (* vlog "loop off=%d len=%d\n%!" off len; *)
    if off + 32 <= len then begin
      let resp = Bytes.sub state.buf off 32 in
      handleresp resp;
      loop (off + 32)
    end else if off < len then begin
      Bytes.blit state.buf off state.buf 0 (len - off);
      state.off <- len - state.off
    end else
      state.off <- 0
  in
  loop 0

let fontsizefactor () =
  get_backing_scale_factor ()

let init t _ w h platform =
  let fd = get_server_fd () in
  state.t <- t;
  state.fd <- fd;
  makecurrentcontext ();
  reshape w h;
  fd, getw (), geth ()

let activatewin () = ()

let metamask = 1 lsl 19

let altmask = 1 lsl 19

let shiftmask = 1 lsl 17

let ctrlmask = 1 lsl 18

let withalt mask = mask land metamask != 0

let withctrl mask = mask land ctrlmask != 0

let withshift mask = mask land shiftmask != 0

let withmeta mask = mask land metamask != 0

let withnone mask = mask land (altmask + ctrlmask + shiftmask + metamask) = 0

let xlatt, xlatf =
  let t = Hashtbl.create 20
  and f = Hashtbl.create 20 in
  let add n nl k =
    List.iter (fun s -> Hashtbl.add t s k) (n::nl);
    Hashtbl.add f k n
  in
  let addc c =
    let s = String.make 1 c in
    add s [] (Char.code c)
  in
  let addcr a b =
    let an = Char.code a and bn = Char.code b in
    for i = an to bn do addc (Char.chr i) done;
  in
  addcr '0' '9';
  addcr 'a' 'z';
  addcr 'A' 'Z';
  String.iter addc "`~!@#$%^&*()-_=+\\|[{]};:,./<>?";
  for i = 0 to 29 do add ("f" ^ string_of_int (i+1)) [] (0xf704 + i) done;
  add "space" [] 32;
  add "ret" ["return"; "enter"] 13;
  add "tab" [] 9;
  add "left" [] 0xff51;
  add "right" [] 0xff53;
  add "home" [] 0xf729;
  add "end" [] 0xf72b;
  add "ins" ["insert"] 0xf729;
  add "del" ["delete"] 0x7f;
  add "esc" ["escape"] 27;
  add "pgup" ["pageup"] 0xf72c;
  add "pgdown" ["pagedown"] 0xf72d;
  add "backspace" [] 8;
  add "up" [] 0xf700;
  add "down" [] 0xf701;
  (* add "menu" [] 0xff67; *) (* ? *)
  t, f;
;;

let keyname k =
  try Hashtbl.find xlatf k
  with Not_found -> Printf.sprintf "%#x" k;
;;

let namekey name =
  try Hashtbl.find xlatt name
  with Not_found ->
    if String.length name = 1
    then Char.code name.[0]
    else int_of_string name;
;;

let keypadtodigitkey key = (* FIXME *)
  if key >= 0xffb0 && key <= 0xffb9 (* keypad numbers *)
  then key - 0xffb0 + 48 else key
;;

let isspecialkey key =
  (0x0 <= key && key <= 0x1F) || key = 0x7f || (0x80 <= key && key <= 0x9F) ||
  (key land 0xf700 = 0xf700)
;;
