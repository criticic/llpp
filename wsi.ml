type cursor =
    | CURSOR_INHERIT
    | CURSOR_INFO
    | CURSOR_CYCLE
    | CURSOR_CROSSHAIR
    | CURSOR_TEXT
;;

external cloexec : Unix.file_descr -> unit = "ml_cloexec";;
external glx : int -> unit = "ml_glx";;
external swapb : unit -> unit = "ml_swapb";;
external hasdata : Unix.file_descr -> bool = "ml_hasdata";;
external toutf8 : int -> string = "ml_keysymtoutf8";;

let dolog fmt = Format.kprintf prerr_endline fmt;;
let vlog fmt = Format.kprintf ignore fmt;;

let onot = object
  method display         = ()
  method expose          = ()
  method reshape _ _     = ()
  method mouse _ _ _ _ _ = ()
  method motion _ _      = ()
  method pmotion _ _     = ()
  method key _ _         = ()
  method enter _ _       = ()
  method leave           = ()
  method quit            = exit 0
end;;

class type t = object
  method display : unit
  method expose  : unit
  method reshape : int -> int -> unit
  method mouse   : int -> bool -> int -> int -> int -> unit
  method motion  : int -> int -> unit
  method pmotion : int -> int -> unit
  method key     : int -> int -> unit
  method enter   : int -> int -> unit
  method leave   : unit
  method quit    : unit
end;;

type state =
    { mutable mink       : int
    ; mutable maxk       : int
    ; mutable keymap     : int array array
    ; fifo               : (string -> unit) Queue.t
    ; mutable seq        : int
    ; mutable protoatom  : int
    ; mutable deleatom   : int
    ; mutable idbase     : int
    ; mutable fullscreen : (int -> unit)
    ; mutable setwmname  : (string -> unit)
    ; mutable stringatom : int
    ; mutable t          : t
    ; mutable sock       : Unix.file_descr
    ; mutable w          : int
    ; mutable h          : int
    ; mutable fs         : bool
    }
;;

let state =
  { mink       = max_int
  ; maxk       = min_int
  ; keymap     = [||]
  ; fifo       = Queue.create ()
  ; seq        = 0
  ; protoatom  = -1
  ; deleatom   = -1
  ; idbase     = -1
  ; fullscreen = (fun _ -> ())
  ; setwmname  = (fun _ -> ())
  ; sock       = Unix.stdin
  ; t          = onot
  ; w          = -1
  ; h          = -1
  ; fs         = false
  ; stringatom = 31
  }
;;

let w8 s pos i = String.set s pos (Char.chr (i land 0xff));;

let w16 s pos i =
  w8 s pos i;
  w8 s (pos+1) (i lsr 8);
;;

let w32 s pos i =
  w16 s pos i;
  w16 s (pos+2) (i lsr 16);
;;

let r16 s pos =
  let rb pos1 = Char.code (String.get s (pos + pos1)) in
  (rb 0) lor ((rb 1) lsl 8)
;;

let r16s s pos =
  let i = r16 s pos in
  i - ((i land 0x8000) lsl 1);
;;

let r8 s pos = Char.code (String.get s pos);;

let r32 s pos =
  let rb pos1 = Char.code (String.get s (pos + pos1)) in
  let l = (rb 0) lor ((rb 1) lsl 8)
  and u = (rb 2) lor ((rb 3) lsl 8) in
  (u lsl 16) lor l
;;

let error fmt = Printf.kprintf failwith fmt;;

let readstr sock n =
  let s = String.create n in
  let rec loop pos n =
    let m = Unix.read sock s pos n in
    if n != m
    then (
      ignore (Unix.select [sock] [] [] 0.01);
      loop (pos + m) (n - m)
    )
  in
  loop 0 n;
  s;
;;

let sendstr1 s pos len sock =
  vlog "%d => %S" state.seq s;
  state.seq <- state.seq + 1;
  let n = Unix.send sock s pos len [] in
  if n != len
  then error "send %d returned %d" len n;
;;

let updkmap sock resp =
  let syms = r8 resp 1 in
  let len = r32 resp 4 in
  let data =
    if len > 0
    then readstr sock (4*len)
    else ""
  in
  let m = len / syms in
  state.keymap <- Array.make_matrix
    (state.maxk - state.mink) syms 0xffffff;
  let rec loop i = if i = m then () else
      let k = i*4*syms in
      let rec loop2 k l = if l = syms then () else
          let v = r32 data k in
          state.keymap.(i).(l) <- v;
          loop2 (k+4) (l+1)
      in
      loop2 k 0;
      loop (i+1);
  in
  loop 0;
;;

let sendwithrep sock s f =
  Queue.push f state.fifo;
  sendstr1 s 0 (String.length s) sock;
;;

let padcatl ss =
  let b = Buffer.create 16 in
  List.iter (Buffer.add_string b) ss;
  let bl = Buffer.length b in
  let pl = bl land 3 in
  if pl != 0
  then (
    let pad = "123" in
    Buffer.add_substring b pad 0 (4 - pl);
  );
  Buffer.contents b;
;;

let padcat s1 s2 = padcatl [s1; s2];;

let internreq name onlyifexists =
  let s = "\016\000\000\000\000\000\000\000" in
  let s = padcat s name in
  w8 s 1 (if onlyifexists then 1 else 0);
  w16 s 2 (String.length s / 4);
  w16 s 4 (String.length name);
  s;
;;

let sendintern sock s onlyifexists f =
  let s = internreq s onlyifexists in
  sendwithrep sock s f;
;;

let createwindowreq wid parent x y w h bw mask =
  let s = "\001\000\009\000wwwwppppxxyywwhhbwccvvvvmmmmeeee" in
  w32 s 4 wid;
  w32 s 8 parent;
  w16 s 12 x;
  w16 s 14 y;
  w16 s 16 w;
  w16 s 18 h;
  w16 s 20 bw;
  w16 s 22 1;
  w32 s 24 0;
  w32 s 28 0x800;                       (* eventmask *)
  w32 s 32 mask;
  s;
;;

let getgeometryreq wid =
  let s = "\014u\002\000dddd" in
  w32 s 4 wid;
  s;
;;

let mapreq wid =
  let s = "\008u\002\000wwww" in
  w32 s 4 wid;
  s;
;;

let getkeymapreq first count =
  let s = "\101u\002\000fcuu" in
  w8 s 4 first;
  w8 s 5 count;
  s;
;;

let changepropreq wid prop typ format props =
  let s = "\018\000llwwwwppppttttfuuuLLLL" in
  let s = padcat s props in
  w16 s 2 (String.length s / 4);
  w32 s 4 wid;
  w32 s 8 prop;
  w32 s 12 typ;
  w8 s 16 format;
  let ful = String.length props / (match format with
    | 8 -> 1
    | 16 -> 2
    | 32 -> 4
    | n -> error "no idea what %d means" n)
  in
  w32 s 20 ful;
  s;
;;

let openfontreq fid name =
  let s = "\045ullffffnnuu" in
  let s = padcat s name in
  w16 s 2 (String.length s / 4);
  w32 s 4 fid;
  w16 s 8 (String.length name);
  s;
;;

let createglyphcursorreq fid cid cindex =
  let s = "\094u\008\000ccccffffffffssmmrrggbbRRGGBB" in
  w32 s 4 cid;
  w32 s 8 fid;
  w32 s 12 fid;
  w16 s 16 cindex;
  w16 s 18 (cindex+1);
  w16 s 20 0;
  w16 s 22 0;
  w16 s 24 0;
  w16 s 26 0xffff;
  w16 s 28 0xffff;
  w16 s 30 0xffff;
  s;
;;

let mapwindowreq wid =
  let s = "\008u\002\000wwww" in
  w32 s 4 wid;
  s;
;;

let changewindowattributesreq wid mask attrs =
  let s = "\002ullwwwwmmmm" in
  let s = padcat s attrs in
  w16 s 2 (String.length s / 4);
  w32 s 4 wid;
  w32 s 8 mask;
  s;
;;

let configurewindowreq wid mask values =
  let s = "\012ullwwwwmmuu" in
  let s = padcat s values in
  w16 s 2 (String.length s / 4);
  w32 s 4 wid;
  w16 s 8 mask;
  s;
;;

let s32 n =
  let s = "1234" in
  w32 s 0 n;
  s;
;;

let clientmessage format seq wid typ data =
  let s = "\033fsswwwwtttt" in
  let s = padcat s data in
  w8 s 1 format;
  w16 s 2 seq;
  w32 s 4 wid;
  w32 s 8 typ;
  s;
;;

let sendeventreq propagate destwid mask data =
  let s = "\025p\011\000wwwwmmmm" in
  let s = padcat s data in
  w8 s 1 propagate;
  w32 s 4 destwid;
  w32 s 8 mask;
  s;
;;

let getkeysym code mask =
  let index = (mask land 1) lxor ((mask land 2) lsr 1) in
  let keysym = state.keymap.(code-state.mink).(index) in
  if index = 1 && keysym = 0
  then state.keymap.(code-state.mink).(0)
  else keysym
;;

let rec readresp sock =
  let resp = readstr sock 32 in
  let opcode = r8 resp 0 in
  match opcode land lnot 0x80 with
  | 0 ->                                (* error *)
      let s = resp in
      let code = r8 s 1
      and serial = r16 s 2
      and resid = r32 resp 4
      and min = r16 s 8
      and maj = r8 s 10 in
      error "code=%d serial=%d resid=%#x min=%d maj=%d\n%S"
        code serial resid min maj resp;

  | 1 ->                                (* response *)
      let rep = Queue.pop state.fifo in
      rep resp;

  | 2 ->                                (* key press *)
      if Array.length state.keymap > 0
      then
        let code = r8 resp  1 in
        let mask = r16 resp 28 in
        let keysym = getkeysym code mask in
        vlog "keysym = %x %c" keysym (Char.unsafe_chr keysym);
        state.t#key keysym mask;

  | 3 ->                                (* key release *)
      if Array.length state.keymap > 0
      then
        let code = r8 resp 1 in
        let mask = r16 resp 28 in
        let keysym = getkeysym code mask in
        vlog "release keysym = %x %c mask %#x"
          keysym (Char.unsafe_chr keysym) mask

  | 4 ->                                (* buttonpress *)
      let n = r8 resp 1
      and x = r16s resp 24
      and y = r16s resp 26
      and m = r16 resp 28 in
      state.t#mouse n true x y m;
      vlog "press %d" n

  | 5 ->                                (* buttonrelease *)
      let n = r8 resp 1
      and x = r16s resp 24
      and y = r16s resp 26
      and m = r16 resp 28 in
      state.t#mouse n false x y m;
      vlog "release %d %d %d" n x y

  | 6 ->                                (* motion *)
      let x = r16s resp 24 in
      let y = r16s resp 26 in
      let m = r16 resp 28 in
      if m land 0x1f00 = 0
      then state.t#pmotion x y
      else state.t#motion x y;
      vlog "move %dx%d => %d" x y m

  | 7 ->                                (* enter *)
      let x = r16s resp 24
      and y = r16s resp 26 in
      state.t#enter x y;
      vlog "enter %d %d" x y

  | 8 ->                                (* leave *)
      state.t#leave

  | 18 -> vlog "unmap";

  | 19 ->                               (* map *)
      vlog "map";

  | 12 ->                               (* exposure *)
      vlog "exposure";
      state.t#expose

  | 15 ->                               (* visibility *)
      let vis = r8 resp 8 in
      if vis != 2 then state.t#display;
      vlog "visibility %d" vis;

  | 34 ->                               (* mapping *)
      state.keymap <- [||];
      let s = getkeymapreq state.mink (state.maxk-state.mink-1) in
      sendwithrep sock s (updkmap sock);

  | 33 ->                               (* clientmessage *)
      let atom = r32 resp 8 in
      if atom = state.protoatom
      then (
        let atom = r32 resp 12 in
        if atom = state.deleatom
        then state.t#quit;
      );
      vlog "atom %#x" atom

  | 21 ->                               (* reparent *)
      vlog "reparent"

  | 22 ->                               (* configure *)
      let w = r16 resp 20
      and h = r16 resp 22 in
      vlog "configure %d %d %d %d" state.w state.h w h;
      if w != state.w || h != state.h
      then (
        state.w <- w;
        state.h <- h;
        state.t#reshape w h;
      );
      state.t#display

  | n ->
      dolog "event %d %S" n resp
;;

let readresp sock =
  let rec loop () =
    readresp sock;
    if hasdata sock then loop ();
  in
  loop ();
;;

let sendstr s ?(pos=0) ?(len=String.length s) sock =
  sendstr1 s pos len sock;
  if hasdata sock then readresp sock;
;;

let hexstr s =
  let b = Buffer.create 16 in
  String.iter (fun c ->
    Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))) s;
  Buffer.contents b;
;;

let reshape w h =
  if state.fs
  then
    state.fullscreen state.idbase
  ;
  let s = "wwuuhhuu" in
  w32 s 0 w;
  w32 s 4 h;
  let s = configurewindowreq state.idbase 0x000c s in
  vlog "reshape %d %s %d" state.seq (hexstr s) (String.length s);
  sendstr s state.sock;
;;

let setup sock screennum w h =
  let s = readstr sock 2 in
  let n = String.length s in
  if n != 2
  then error "failed to read X connection setup response n=%d" n;
  match s.[0] with
  | '\000' ->
      let reasonlen = r8 s 1 in
      let s = readstr sock 6 in
      let maj = r16 s 0
      and min = r16 s 2
      and add = r16 s 4 in
      let len = add*4 in
      let data = readstr sock len in
      let reason = String.sub data 0 reasonlen in
      error "X connection failed maj=%d min=%d reason=%S"
        maj min reason

  | '\002' -> failwith "X connection setup failed: authentication required";

  | '\001' ->
      let s       = readstr sock 38 in
      let maj     = r16 s 0
      and min     = r16 s 2
      and add     = r16 s 4
      and idbase  = r32 s 10
      and idmask  = r32 s 14
      and vlen    = r16 s 22
      and screens = r8 s 26
      and formats = r8 s 27
      and minkk   = r8 s 32
      and maxkk   = r8 s 33 in
      let data = readstr sock (4*add-32) in
      let vendor = String.sub data 0 vlen in
      let pos = ((vlen+3) land lnot 3) + formats*8 in

      if screennum >= screens
      then error "invalid screen %d, max %d" screennum (screens-1);

      let pos =
        let s = data in
        let rec findscreen n pos = if n = screennum then pos else
            let pos =
              let ndepths = r8 s (pos+39) in
              let rec skipdepths n pos = if n = ndepths then pos else
                  let pos =
                    let nvisiuals = r16 s (pos+2) in
                    pos + nvisiuals*24 + 8
                  in
                  skipdepths (n+1) pos
              in
              skipdepths n (pos+40)
            in
            findscreen (n+1) pos
        in
        findscreen 0 pos
      in
      let root = r32 data pos in
      let rootw = r16 data (pos+20)
      and rooth = r16 data (pos+22) in
      state.mink <- minkk;
      state.maxk <- maxkk;
      state.idbase <- idbase;
      vlog "vendor = %S, maj=%d min=%d" vendor maj min;
      vlog "screens = %d formats = %d" screens formats;
      vlog "minkk = %d maxkk = %d" minkk maxkk;
      vlog "idbase = %#x idmask = %#x" idbase idmask;
      vlog "root=%#x %dx%d" root rootw rooth;

      let mask = 0
        + 0x00000001                    (* KeyPress *)
        (* + 0x00000002 *)              (* KeyRelease *)
        + 0x00000004                    (* ButtonPress *)
        + 0x00000008                    (* ButtonRelease *)
        + 0x00000010                    (* EnterWindow *)
        + 0x00000020                    (* LeaveWindow *)
        + 0x00000040                    (* PointerMotion *)
        (* + 0x00000080 *)              (* PointerMotionHint *)
        (* + 0x00000100 *)              (* Button1Motion *)
        (* + 0x00000200 *)              (* Button2Motion *)
        (* + 0x00000400 *)              (* Button3Motion *)
        (* + 0x00000800 *)              (* Button4Motion *)
        (* + 0x00001000 *)              (* Button5Motion *)
        + 0x00002000                    (* ButtonMotion *)
        (* + 0x00004000 *)              (* KeymapState *)
        + 0x00008000                    (* Exposure *)
        + 0x00010000                    (* VisibilityChange *)
        + 0x00020000                    (* StructureNotify *)
        (* + 0x00040000 *)              (* ResizeRedirect *)
        (* + 0x00080000 *)              (* SubstructureNotify *)
        (* + 0x00100000 *)              (* SubstructureRedirect *)
        (* + 0x00200000 *)              (* FocusChange *)
        (* + 0x00400000 *)              (* PropertyChange *)
        (* + 0x00800000 *)              (* ColormapChange *)
        (* + 0x01000000 *)              (* OwnerGrabButton *)
      in
      let wid = state.idbase in
      let s = createwindowreq wid root 0 0 w h 0 mask in
      sendstr s sock;

      let s = mapreq wid in
      sendstr s sock;

      let s = getkeymapreq state.mink (state.maxk-state.mink) in
      sendwithrep sock s (updkmap sock);

      sendintern sock "WM_PROTOCOLS" false (fun resp ->
        state.protoatom <- r32 resp 8;
        sendintern sock "WM_DELETE_WINDOW" false (fun resp ->
          state.deleatom <- r32 resp 8;
          let s = s32 state.deleatom in
          let s = changepropreq wid state.protoatom 4 32 s in
          sendstr s sock;
        );
      );

      sendintern sock "WM_CLASS" false (fun resp ->
        let atom = r32 resp 8 in
        let llpp = "llpp\000llpp\000" in
        let s = changepropreq wid atom 31 8 llpp in
        sendstr s sock;
      );

      let s = openfontreq (wid+1) "cursor" in
      sendstr s sock;

      Array.iteri (fun i glyphindex ->
        let s = createglyphcursorreq (wid+1) (wid+2+i) glyphindex in
        sendstr s sock;
      ) [|34;48;50;58;128;152|];

      sendintern sock "UTF8_STRING" true (fun resp ->
        let atom = r32 resp 8 in
        if atom != 0
        then state.stringatom <- atom;
      );

      let setwmname s =
        let s = changepropreq state.idbase 39 state.stringatom 8 s in
        sendstr s state.sock;
      in
      state.setwmname <- setwmname;
      sendintern sock "_NET_WM_NAME" true (fun resp ->
        let atom = r32 resp 8 in
        if atom != 0
        then state.setwmname <- (fun s ->
          setwmname s;
          let s = changepropreq state.idbase atom state.stringatom 8 s in
          sendstr s state.sock;
        );
      );

      sendintern sock "_NET_WM_STATE" true (fun resp ->
        let nwmsatom = r32 resp 8 in
        if nwmsatom != 0
        then
          sendintern sock "_NET_WM_STATE_FULLSCREEN" true (fun resp ->
            let fsatom = r32 resp 8 in
            if fsatom != 0
            then
              state.fullscreen <-
                (fun wid ->
                  let data = String.make 20 '\000' in
                  state.fs <- not state.fs;
                  w32 data 0 (if state.fs then 1 else 0);
                  w32 data 4 fsatom;

                  let cm = clientmessage 32 0 wid nwmsatom data in
                  let s = sendeventreq 0 root 0x180000 cm in
                  sendstr s sock;
                );
          );
      );
      let s = getgeometryreq wid in
      let completed = ref false in
      sendwithrep sock s (fun resp ->
        glx wid;
        let w = r16 resp 16
        and h = r16 resp 18 in
        state.w <- w;
        state.h <- h;
        completed := true;
      );
      let now = Unix.gettimeofday in
      let deadline = now () +. 2.0 in
      let rec readtillcompletion () =
        let r, _, _ = Unix.select [sock] [] [] (deadline -. now ()) in
        match r with
        | [] -> readtillcompletion ()
        | _ ->
            readresp sock;
            if not !completed
            then readtillcompletion ()
      in
      readtillcompletion ();

  | c ->
      error "unknown conection setup response %d" (Char.code c)
;;

let getauth haddr dnum =
  let haddr =
    if haddr = "localhost" || String.length haddr = 0
    then
      try Unix.gethostname ()
      with exn ->
        dolog "failed to resolve `%S': %s" haddr (Printexc.to_string exn);
        haddr
    else haddr
  in
  let readauth ic =
    let input_string ic len =
      let s = String.create len in
      really_input ic s 0 len;
      s;
    in
    let r16 s =
      let rb pos = Char.code (String.get s pos) in
      (rb 1) lor ((rb 0) lsl 8)
    in
    let rec find () =
      let rs () =
        let s = input_string ic 2 in
        let n = r16 s in
        input_string ic n
      in
      let family = input_string ic 2 in
      let addr = rs () in
      let nums = rs () in
      let num = int_of_string nums in
      let name = rs () in
      let data = rs () in

      vlog "family %S addr %S(%S) num %d(%d) name %S data %S"
        family addr haddr num dnum name data;
      if addr = haddr && num = dnum
      then name, data
      else find ()
    in
    let name, data =
      try find () with _ -> "", ""
    in
    close_in ic;
    name, data;
  in
  let path =
    try Sys.getenv "XAUTHORITY"
    with Not_found ->
      try Filename.concat (Sys.getenv "HOME") ".Xauthority"
      with Not_found -> ""
  in
  let opt =
    try
      if String.length path = 0
      then None
      else Some (open_in_bin path)
    with exn ->
      dolog "failed to open X authority file `%S' : %s"
        path
        (Printexc.to_string exn);
      None
  in
  match opt with
  | None -> "", ""
  | Some ic -> readauth ic
;;

let init t w h =
  let d =
    try Sys.getenv "DISPLAY"
    with exn ->
      error "Could not get DISPLAY evironment variable: %s"
        (Printexc.to_string exn)
  in
  let colonpos = String.index d ':' in
  let host = String.sub d 0 colonpos in
  let dispnum, screennum =
    try
      let dotpos = String.index_from d (colonpos + 1) '.' in
      let disp = String.sub d (colonpos + 1) (dotpos - colonpos - 1) in
      let screen = String.sub d (dotpos + 1) (String.length d - dotpos - 1) in
      int_of_string disp, int_of_string screen
    with Not_found ->
      let disp = String.sub d (colonpos + 1) (String.length d - colonpos - 1) in
      int_of_string disp, 0
  in
  let aname, adata = getauth host dispnum in
  let fd =
    if String.length host = 0 || host = "unix"
    then
      let addr = Unix.ADDR_UNIX ("/tmp/.X11-unix/X" ^ string_of_int dispnum) in
      let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Unix.connect fd addr;
      fd
    else
      let h = Unix.gethostbyname host in
      let addr = h.Unix.h_addr_list.(0) in
      let port = 6000 + dispnum in
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.connect fd (Unix.ADDR_INET (addr, port));
      fd
  in
  cloexec fd;
  let s = "luMMmmnndduu" in
  let s = padcat s aname in
  let s = padcat s adata in
  w16 s 2 11;
  w16 s 4 0;
  w16 s 6 (String.length aname);
  w16 s 8 (String.length adata);
  sendstr1 s 0 (String.length s) fd;
  state.sock <- fd;
  setup fd screennum w h;
  state.t <- t;
  fd, state.w, state.h;
;;

let settitle s =
  state.setwmname s;
;;

let setcursor cursor =
  let n =
    match cursor with
    | CURSOR_INHERIT -> -1
    | CURSOR_INFO -> 3
    | CURSOR_CYCLE -> 2
    | CURSOR_CROSSHAIR -> 0
    | CURSOR_TEXT -> 5
  in
  let s = s32 (if n = -1 then 0 else state.idbase+2+n) in
  let s = changewindowattributesreq state.idbase (*cursor*)0x4000 s in
  sendstr s state.sock;
;;

let fullscreen () =
  state.fullscreen state.idbase;
;;

let metamask = 0x40;;
let altmask = 8;;
let shiftmask = 1;;
let ctrlmask = 4;;

let withalt mask = mask land altmask != 0;;
let withctrl mask = mask land ctrlmask != 0;;
let withshift mask = mask land shiftmask != 0;;
let withmeta mask = mask land metamask != 0;;
let withnone mask = mask land (altmask + ctrlmask + shiftmask + metamask) = 0;;

let xlatt, xlatf =
  let t = Hashtbl.create 20
  and f = Hashtbl.create 20 in
  let add n nl k =
    List.iter (fun s -> Hashtbl.add t s k) (n::nl);
    Hashtbl.add f k n
  in
  let addc c =
    let s = String.create 1 in
    s.[0] <- c;
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
  for i = 0 to 29 do add ("f" ^ string_of_int (i+1)) [] (0xffbe + i) done;
  add "space" [] 0x20;
  add "return" ["ret"; "enter"] 0xff0d;
  add "tab" [] 0xff09;
  add "left" [] 0xff51;
  add "right" [] 0xff53;
  add "home" [] 0xff50;
  add "end" [] 0xff57;
  add "insert" ["ins"] 0xff63;
  add "delete" ["del"] 0xffff;
  add "escape" ["esc"] 0xff1b;
  add "pgup" ["pageup"] 0xff55;
  add "pgdown" ["pagedown"] 0xff56;
  add "backspace" [] 0xff08;
  add "up" [] 0xff52;
  add "down" [] 0xff54;
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
