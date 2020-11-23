open Utils

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
  method quit : 'a. 'a   = exit 0
  method scroll _ _      = ()
  method zoom _ _ _      = ()
  method opendoc _       = ()
end

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
  method quit     : 'a. 'a
  method scroll   : int -> int -> unit
  method zoom     : float -> int -> int -> unit
  method opendoc  : string -> unit
end

type state =
  {
    mutable t: t;
    mutable fd: Unix.file_descr;
    buf: bytes;
    mutable off: int;
    path: Buffer.t;
  }

let state =
  {
    t = onot;
    fd = Unix.stdin;
    buf = Bytes.create 512;
    off = 0;
    path = Buffer.create 0;
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
external nslog: string -> unit = "ml_nslog"

let nslog fmt =
  Printf.ksprintf nslog fmt

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
   13 -> zoom
   20 -> open *)

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
      let x = r16s resp 20 in
      let y = r16s resp 22 in
      vlog "zoom z %f x %d y %d" z x y;
      state.t#zoom z x y
  | 20 ->
      begin match r16 resp 2 with
      | 0 ->
          let path = Buffer.contents state.path in
          Buffer.reset state.path;
          if false then nslog "open %S" path;
          state.t#opendoc path
      | chunk_len ->
          if false then nslog "open-append %S" (Bytes.sub_string resp 4 chunk_len);
          Buffer.add_subbytes state.path resp 4 chunk_len
      end
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

let fontsizescale n =
  n * get_backing_scale_factor ()

let init t w h =
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
  t, f

let keyname k =
  try Hashtbl.find xlatf k
  with Not_found -> Printf.sprintf "%#x" k

let namekey name =
  try Hashtbl.find xlatt name
  with Not_found ->
    if String.length name = 1
    then Char.code name.[0]
    else int_of_string name

let ks2kt =
  let open Keys in
  function
  | 8 -> Backspace
  | 27 -> Escape
  | 13 -> Enter
  | 0xf727 -> Insert
  | 0xf729 | 0xfff04 -> Home
  | 0xf702 | 0xfff05 -> Left
  | 0xfff0b | 0xf700 -> Up
  | 0xfff0a | 0xF703 -> Right
  | 0xfff01 | 0xf701-> Down
  | 0xfff09 | 0xf72c -> Prior
  | 0xf72d | 0xfff07 -> Next
  | 0xfff02 | 0xf72b -> End
  | 0x7f -> Delete
  | 0xfff03 -> Enter
  | 0xfff08 -> Ascii '+'
  | 0xfff06 -> Ascii '-'
  | code when code > 31 && code < 128 -> Ascii (Char.unsafe_chr code)
  | code when code >= 0xffb0 && code <= 0xffb9 ->
     Ascii (Char.unsafe_chr (code - 0xffb0 + 0x30))
  | code when code >= 0xf704 && code <= 0xf70f -> Fn (code - 0xf704 + 1)
  | code when code land 0xff00 = 0xff00 -> Ctrl code
  | code -> Code code

let setmapc = ignore
type keycode = int
