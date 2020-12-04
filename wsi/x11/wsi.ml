open Utils

let (~>) = Bytes.unsafe_of_string

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

type wid = int and screenno = int and vid = int and atom = int

external glxinit : string -> wid -> screenno -> vid = "ml_glxinit"
external glxcompleteinit : unit -> unit = "ml_glxcompleteinit"
external swapb : unit -> unit = "ml_swapb"
external setcursor : cursor -> unit = "ml_setcursor"

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

type state =
  { mutable mink       : int
  ; mutable maxk       : int
  ; mutable keymap     : int array array
  ; fifo               : (bytes -> unit) Queue.t
  ; mutable seq        : int
  ; mutable protoatom  : atom
  ; mutable deleatom   : atom
  ; mutable nwmsatom   : atom
  ; mutable maxvatom   : atom
  ; mutable maxhatom   : atom
  ; mutable fulsatom   : atom
  ; mutable idbase     : int
  ; mutable wid        : int
  ; mutable fid        : int
  ; mutable fullscreen : (int -> unit)
  ; mutable setwmname  : (bytes -> unit)
  ; mutable actwin     : (unit -> unit)
  ; mutable stringatom : int
  ; mutable t          : t
  ; mutable sock       : Unix.file_descr
  ; mutable x          : int
  ; mutable y          : int
  ; mutable w          : int
  ; mutable h          : int
  ; mutable fs         : fs
  ; mutable curcurs    : cursor
  ; mutable capslmask  : int
  ; mutable numlmask   : int
  ; mutable levl3mask  : int
  ; mutable levl5mask  : int
  ; mutable xkb        : bool
  ; mutable fscale     : float
  ; mutable mapc       : keycode -> keycode
  }
and fs =
  | NoFs
  | Fs of (int * int * int * int)
and keycode = int

let state =
  { mink       = max_int
  ; maxk       = min_int
  ; keymap     = E.a
  ; fifo       = Queue.create ()
  ; seq        = 0
  ; protoatom  = -1
  ; deleatom   = -1
  ; nwmsatom   = -1
  ; maxvatom   = -1
  ; maxhatom   = -1
  ; fulsatom   = -1
  ; idbase     = -1
  ; wid        = -1
  ; fid        = -1
  ; fullscreen = (fun _ -> ())
  ; setwmname  = (fun _ -> ())
  ; actwin     = (fun _ -> ())
  ; sock       = Unix.stdin
  ; t          = onot
  ; x          = -1
  ; y          = -1
  ; w          = -1
  ; h          = -1
  ; fs         = NoFs
  ; stringatom = 31
  ; curcurs    = CURSOR_TEXT
  ; capslmask  = 0
  ; numlmask   = 0
  ; levl3mask  = 0
  ; levl5mask  = 0
  ; xkb        = false
  ; fscale     = 1.0
  ; mapc       = Fun.id
  }

let settitle s = state.setwmname (~> s)
let fullscreen () = state.fullscreen state.wid
let fontsizescale n = float n *. state.fscale |> truncate
let setmapc f = state.mapc <- f

let ordermagic = 'l'
let metamask = 0x40
let altmask = 8
let shiftmask = 1
let ctrlmask = 4

let withalt mask = mask land altmask != 0
let withctrl mask = mask land ctrlmask != 0
let withshift mask = mask land shiftmask != 0
let withmeta mask = mask land metamask != 0
let withnone mask = mask land (altmask + ctrlmask + shiftmask + metamask) = 0

let makereq opcode len reqlen =
  let s = Bytes.create len in
  w8 s 0 opcode;
  w16 s 2 reqlen;
  s

let readstr sock n =
  let s = Bytes.create n in
  let rec loop pos n =
    let m = tempfailureretry (Unix.read sock s pos) n in
    if m = 0
    then state.t#quit;
    if n != m
    then (
      ignore (tempfailureretry (Unix.select [sock] [] []) 0.01);
      loop (pos + m) (n - m)
    )
  in
  loop 0 n;
  s

let sendstr1 s pos len sock =
  let s = Bytes.unsafe_to_string s in
  vlog "%d <= %S" state.seq s;
  state.seq <- state.seq + 1;
  let n = tempfailureretry (Unix.write_substring sock s pos) len in
  if n != len
  then error "send %d returned %d" len n

let updkmap sock resp =
  let syms = r8 resp 1 in
  let len = r32 resp 4 in
  let data =
    if len > 0
    then readstr sock (4*len)
    else E.b
  in
  let m = len / syms in
  state.keymap <- Array.make_matrix state.maxk syms 0xffffff;
  let rec loop i =
    if i != m
    then
      let k = i*4*syms in
      let rec loop2 k l =
        if l != syms
        then
          let v = r32 data k in
          state.keymap.(i).(l) <- v;
          loop2 (k+4) (l+1)
      in
      loop2 k 0;
      loop (i+1);
  in
  loop 0

let updmodmap sock resp =
  let n = r8 resp 1 in
  let len = r16 resp 4 in
  let data =
    if len > 0
    then readstr sock (len*4)
    else E.b
  in
  if len > 0
  then                      (*???*)
    let modmap = Array.make_matrix 8 n 0xffffff in
    let rec loop l =
      if l != 8
      then
        let p = l*n in
        let rec loop1 m =
          if m != n
          then
            let p = p+m in
            let code = r8 data p in
            modmap.(l).(m) <- code;
            if l = 1
            then (
              let ki = code - state.mink in
              if ki >= 0
              then
                let a = state.keymap.(ki) in
                let rec capsloop i =
                  if not (i = Array.length a || i > 3)
                  then
                    let s = a.(i) in
                    if s = 0xffe5
                    then state.capslmask <- 2
                    else capsloop (i+1)
                in
                capsloop 0;
            )
            else (
              if l > 3
              then
                let ki = code - state.mink in
                if ki >= 0
                then
                  let a = state.keymap.(ki) in
                  let rec lloop i =
                    if not (i = Array.length a || i > 3)
                    then
                      let s = a.(i) in
                      match s with
                      | 0xfe03 -> state.levl3mask <- 1 lsl l
                      | 0xfe11 -> state.levl5mask <- 1 lsl l
                      | 0xff7f -> state.numlmask  <- 1 lsl l
                      | _ -> lloop (i+1)
                  in
                  lloop 0;
            );
            loop1 (m+1)
        in
        loop1 0;
        loop (l+1)
    in
    loop 0

let sendwithrep sock s f =
  Queue.push f state.fifo;
  sendstr1 s 0 (Bytes.length s) sock

let padcat b1 b2 =
  let l1 = Bytes.length b1 and l2 = Bytes.length b2 in
  let l = (l1 + l2) land 3 in
  let pl = if l > 0 then 4 - l else 0 in
  let b = Bytes.create (l1 + l2 + pl) in
  Bytes.blit b1 0 b 0 l1;
  Bytes.blit b2 0 b l1 l2;
  b

let internreq name onlyifexists =
  let s = makereq 16 8 8 in
  let s = padcat s name in
  w8 s 1 (if onlyifexists then 1 else 0);
  w16 s 2 (Bytes.length s / 4);
  w16 s 4 (Bytes.length name);
  s

let sendintern sock s onlyifexists f =
  let s = internreq s onlyifexists in
  sendwithrep sock s f

let createwindowreq wid parent x y w h bw eventmask vid depth mid =
  let s = makereq 1 44 11 in
  w8 s 1 depth;
  w32 s 4 wid;
  w32 s 8 parent;
  w16 s 12 x;
  w16 s 14 y;
  w16 s 16 w;
  w16 s 18 h;
  w16 s 20 bw;
  w16 s 22 0;                           (* copyfromparent *)
  w32 s 24 vid;                         (* visual *)
  w32 s 28 0x2808;                      (* valuemask =
                                         | border pixel
                                         | event mask
                                         | colormap *)
  w32 s 32 0;                           (* border pixel*)
  w32 s 36 eventmask;
  w32 s 40 mid;
  s

let createcolormapreq mid wid vid =
  let s = makereq 78 16 4 in
  w8 s 1 0;
  w32 s 4 mid;
  w32 s 8 wid;
  w32 s 12 vid;
  s

let getgeometryreq wid =
  let s = makereq 14 8 2 in
  w32 s 4 wid;
  s

let mapreq wid =
  let s = makereq 8 8 2 in
  w32 s 4 wid;
  s

let getkeymapreq first count =
  let s = makereq 101 8 2 in
  w8 s 4 first;
  w8 s 5 count;
  s

let changepropreq wid prop typ format props =
  let s = makereq 18 24 0 in
  let s = padcat s props in
  w8 s 1 0;
  w16 s 2 (Bytes.length s / 4);
  w32 s 4 wid;
  w32 s 8 prop;
  w32 s 12 typ;
  w8 s 16 format;
  let ful = Bytes.length props / (match format with
                                  | 8 -> 1
                                  | 16 -> 2
                                  | 32 -> 4
                                  | n -> error "no idea what %d means" n)
  in
  w32 s 20 ful;
  s

let getpropreq delete wid prop typ =
  let s = makereq 20 24 6 in
  w8 s 1 (if delete then 1 else 0);
  w32 s 4 wid;
  w32 s 8 prop;
  w32 s 12 typ;
  w32 s 16 0;
  w32 s 20 2;
  s

let configurewindowreq wid mask values =
  let s = makereq 12 12 0 in
  let s = padcat s values in
  w16 s 2 (Bytes.length s / 4);
  w32 s 4 wid;
  w16 s 8 mask;
  s

let s32 n =
  let s = Bytes.create 4 in
  w32 s 0 n;
  s

let clientmessage format seq wid typ data =
  let s = makereq 33 12 0 in
  let s = padcat s data in
  w8 s 1 format;
  w16 s 2 seq;
  w32 s 4 wid;
  w32 s 8 typ;
  s

let sendeventreq propagate destwid mask data =
  let s = makereq 25 12 11 in
  let s = padcat s data in
  w8 s 1 propagate;
  w16 s 2 11;
  w32 s 4 destwid;
  w32 s 8 mask;
  s

let getmodifiermappingreq () =
  makereq 119 4 1

let queryextensionreq name =
  let s = makereq 98 8 0 in
  let s = padcat s name in
  w16 s 2 (Bytes.length s / 4);
  w16 s 4 (Bytes.length name);
  s

let getkeysym pkpk code mask =
  if (pkpk >= 0xff80 && pkpk <= 0xffbd)
     || (pkpk >= 0x11000000 && pkpk <= 0x1100ffff)
  then (
    if mask land state.numlmask != 0
    then
      let keysym = state.keymap.(code-state.mink).(1) in
      if keysym = 0 then pkpk else keysym
    else pkpk
  )
  else (
    let shift =
      if pkpk land 0xf000 = 0xf000
      then 0
      else (mask land 1) lxor ((mask land state.capslmask) lsr 1)
    in
    let index =
      if state.xkb && mask land 0x2000 != 0
      then shift + 2
      else
        let l3 = (mask land state.levl3mask) != 0 in
        let l4 = (mask land state.levl5mask) != 0 in
        shift +
          if l3 then (if l4 then 8 else 4) else (if l4 then 6 else 0)
    in
    let keysym = state.keymap.(code-state.mink).(index) in
    if index land 1 = 1 && keysym = 0
    then state.keymap.(code-state.mink).(index - 1)
    else keysym
  )

let getkeysym code mask =
  let pkpk = state.keymap.(code-state.mink).(0) in
  if state.xkb && pkpk lsr 8 = 0xfe (* XKB *)
  then 0
  else getkeysym pkpk code mask

let readresp sock =
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
       code serial resid min maj (Bytes.unsafe_to_string resp);

  | 1 ->                                (* response *)
     let rep = Queue.pop state.fifo in
     rep resp;

  | 2 ->                                (* key press *)
     if Array.length state.keymap > 0
     then
       let code = state.mapc @@ r8 resp 1 in
       let mask = r16 resp 28 in
       let keysym = getkeysym code mask in
       vlog "keysym = %x %c mask %#x code %d"
         keysym (Char.unsafe_chr keysym) mask code;
       if keysym != 0
       then state.t#key keysym mask;

  | 3 ->                                (* key release *)
     if Array.length state.keymap > 0
     then
       let code = r8 resp 1 in
       let mask = r16 resp 28 in
       let keysym = getkeysym code mask in
       vlog "release keysym = %x %c mask %#x code %d"
         keysym (Char.unsafe_chr keysym) mask code;

  | 4 ->                                (* buttonpress *)
     let n = r8 resp 1
     and x = r16s resp 24
     and y = r16s resp 26
     and m = r16 resp 28 in
     state.t#mouse n true x y m;
     vlog "press %d" n;

  | 5 ->                                (* buttonrelease *)
     let n = r8 resp 1
     and x = r16s resp 24
     and y = r16s resp 26
     and m = r16 resp 28 in
     state.t#mouse n false x y m;
     vlog "release %d %d %d" n x y;

  | 6 ->                                (* motion *)
     let x = r16s resp 24 in
     let y = r16s resp 26 in
     let m = r16 resp 28 in
     if m land 0x1f00 = 0
     then state.t#pmotion x y
     else state.t#motion x y;
     vlog "move %dx%d => %d" x y m;

  | 7 ->                                (* enter *)
     let x = r16s resp 24
     and y = r16s resp 26 in
     state.t#enter x y;
     vlog "enter %d %d" x y;

  | 8 ->                                (* leave *)
     state.t#leave;

  | 18 ->                               (* unmap *)
     state.t#map false;
     vlog "unmap";

  | 19 ->                               (* map *)
     state.t#map true;
     vlog "map";

  | 12 ->                               (* exposure *)
     vlog "exposure";
     state.t#expose;

  | 15 ->                               (* visibility *)
     let v = r8 resp 8 in
     let vis =
       match v with
       | 0 -> Unobscured
       | 1 -> PartiallyObscured
       | 2 -> FullyObscured
       | _ ->
          dolog "unknown visibility %d" v;
          Unobscured
     in
     state.t#visible vis;
     vlog "visibility %d" v;

  | 11 ->                               (* keymapnotify *)
     state.keymap <- E.a;
     let s = getkeymapreq state.mink (state.maxk-state.mink-1) in
     sendwithrep sock s (updkmap sock);
     state.capslmask <- 0;
     state.levl3mask <- 0;
     state.levl5mask <- 0;
     state.numlmask  <- 0;
     let s = getmodifiermappingreq () in
     sendwithrep sock s (updmodmap sock);

  | 33 ->                               (* clientmessage *)
     let atom = r32 resp 8 in
     if atom = state.protoatom
     then
       let atom = r32 resp 12 in
       if atom = state.deleatom
       then state.t#quit;
     vlog "atom %#x" atom;

  | 21 ->                               (* reparent *)
     vlog "reparent";

  | 22 ->                               (* configure *)
     let x = r16s resp 16
     and y = r16s resp 18
     and w = r16 resp 20
     and h = r16 resp 22 in
     vlog "configure cur [%d %d %d %d] conf [%d %d %d %d]"
       state.x state.y state.w state.h
       x y w h;
     if w != state.w || h != state.h
     then state.t#reshape w h;
     state.w <- w;
     state.h <- h;
     state.x <- x;
     state.y <- y;

  | 24 ->                       (* Gravity notify *)
     ();

  | 28 ->                       (* Property notify *)
     let atom = r32 resp 8 in
     if atom = state.nwmsatom
     then
       let s = getpropreq false state.wid atom 4 in
       sendwithrep sock s (fun resp ->
           state.fs <- NoFs;
           let len = r32 resp 4 in
           let nitems = r32 resp 16 in
           let wsl =
             if len = 0
             then []
             else
               let s = readstr sock (len*4) in
               let rec loop wsl i =
                 if i = nitems
                 then wsl
                 else
                   let atom = r32 s (i*4) in
                   let wsl =
                     if atom = state.maxhatom
                     then MaxHorz::wsl
                     else (
                       if atom = state.maxvatom
                       then MaxVert::wsl
                       else (
                         if atom = state.fulsatom
                         then (
                           state.fs <- Fs (state.x, state.y,
                                           state.w, state.h);
                           Fullscreen::wsl
                         )
                         else wsl
                       )
                     )
                   in loop wsl (i+1)
               in
               loop [] 0
           in
           state.t#winstate (List.sort compare wsl)
         );

  | n -> dolog "event %d %S" n (Bytes.unsafe_to_string resp)

let readresp sock =
  let rec loop () =
    readresp sock;
    if hasdata sock then loop ();
  in
  loop ()

let sendstr s ?(pos=0) ?(len=Bytes.length s) sock =
  sendstr1 s pos len sock;
  if hasdata sock then readresp sock

let reshape w h =
  if state.fs = NoFs
  then
    let s = Bytes.create 8 in
    w32 s 0 w;
    w32 s 4 h;
    let s = configurewindowreq state.wid 0x000c s in
    sendstr s state.sock;
  else state.fullscreen state.wid

let activatewin () =
  state.actwin ()

let syncsendwithrep sock secstowait s f =
  let completed = ref false in
  sendwithrep sock s (fun resp -> f resp; completed := true);
  let now = Unix.gettimeofday in
  let deadline = now () +. secstowait in
  let rec readtillcompletion () =
    let sf deadline =
      let timeout = deadline -. now () in
      if timeout <= 0.0
      then [], [], []
      else Unix.select [sock] [] [] timeout
    in
    let r, _, _ = tempfailureretry sf deadline in
    match r with
    | [] -> error "didn't get X response in %f seconds, aborting" secstowait
    | _ ->
       readresp sock;
       if not !completed
       then readtillcompletion ();
  in
  readtillcompletion ()

let mapwin () =
  let s = mapreq state.wid in
  sendstr s state.sock

let syncsendintern sock secstowait s onlyifexists f =
  let s = internreq s onlyifexists in
  syncsendwithrep sock secstowait s f

let setup disp sock rootwid screennum w h =
  let s = readstr sock 2 in
  let n = Bytes.length s in
  if n != 2
  then error "failed to read X connection setup response n=%d" n;
  match Bytes.get s 0 with
  | '\000' ->
     let reasonlen = r8 s 1 in
     let s = readstr sock 6 in
     let maj = r16 s 0
     and min = r16 s 2
     and add = r16 s 4 in
     let len = add*4 in
     let data = readstr sock len in
     let reason = Bytes.sub data 0 reasonlen in
     error "X connection failed maj=%d min=%d reason=%S"
       maj min (Bytes.unsafe_to_string reason);

  | '\002' -> error "X connection setup failed: authentication required";

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
     let vendor = Bytes.sub data 0 vlen in
     let pos = ((vlen+3) land lnot 3) + formats*8 in

     if screennum >= screens
     then error "invalid screen %d, max %d" screennum (screens-1);

     let pos =
       let rec findscreen n pos =
         if n = screennum
         then pos
         else
           let pos =
             let ndepths = r8 data (pos+39) in
             let rec skipdepths n pos =
               if n = ndepths
               then pos
               else
                 let pos =
                   let nvisiuals = r16 data (pos+2) in
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
     let root = if rootwid = 0 then r32 data pos else rootwid in
     let rootw = r16 data (pos+20)
     and rooth = r16 data (pos+22)
     and rootdepth = r8 data (pos+38)in

     state.fscale <- float rooth /. 1440.0;
     state.mink <- minkk;
     state.maxk <- maxkk;
     state.idbase <- idbase;
     vlog "vendor = %S, maj=%d min=%d" (Bytes.unsafe_to_string vendor) maj min;
     vlog "screens = %d formats = %d" screens formats;
     vlog "minkk = %d maxkk = %d" minkk maxkk;
     vlog "idbase = %#x idmask = %#x" idbase idmask;
     vlog "root=%#x %dx%d" root rootw rooth;
     vlog "wmm = %d, hmm = %d" (r16 data (pos+24)) (r16 data (pos+26));
     vlog "visualid = %#x" (r32 data (pos+32));
     vlog "root depth = %d" rootdepth;

     let wid = state.idbase in
     let mid = wid+1 in
     let fid = mid+1 in

     state.wid <- wid;
     state.fid <- fid;

     let vid = glxinit disp wid screennum in
     let ndepths = r8 data (pos+39) in
     let rec finddepth n' pos =
       if n' = ndepths
       then error "cannot find depth for visual %#x" vid;
       let depth = r8 data pos in
       let nvisuals = r16 data (pos+2) in
       let rec findvisual n pos =
         if n = nvisuals
         then finddepth (n'+1) pos
         else
           let id = r32 data pos in
           if id = vid
           then depth
           else findvisual (n+1) (pos+24)
       in
       findvisual 0 (pos+8)
     in
     let depth = finddepth 0 (pos+40) in

     let s = createcolormapreq mid root vid in
     sendstr s sock;

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
                + 0x00004000                    (* KeymapState *)
                + 0x00008000                    (* Exposure *)
                + 0x00010000                    (* VisibilityChange *)
                + 0x00020000                    (* StructureNotify *)
                (* + 0x00040000 *)              (* ResizeRedirect *)
                (* + 0x00080000 *)              (* SubstructureNotify *)
                (* + 0x00100000 *)              (* SubstructureRedirect *)
                (* + 0x00200000 *)              (* FocusChange *)
                + 0x00400000                    (* PropertyChange *)
                (* + 0x00800000 *)              (* ColormapChange *)
                (* + 0x01000000 *)              (* OwnerGrabButton *)
     in

     let s = createwindowreq wid root 0 0 w h 0 mask vid depth mid in
     sendstr s sock;

     sendintern
       sock (~> "WM_PROTOCOLS") false (fun resp ->
         state.protoatom <- r32 resp 8;
         sendintern
           sock (~> "WM_DELETE_WINDOW") false (fun resp ->
             state.deleatom <- r32 resp 8;
             let s = s32 state.deleatom in
             let s = changepropreq wid state.protoatom 4 32 s in
             sendstr s sock;
           );
       );

     sendintern
       sock (~> "WM_CLIENT_MACHINE") false (fun resp ->
         let atom = r32 resp 8 in
         let empty = E.s in
         let hostname =
           try Unix.gethostname ()
           with exn ->
             dolog "error getting host name: %s" @@ exntos exn;
             empty
         in
         if hostname != empty
         then
           let s = changepropreq wid atom state.stringatom 8
                     (~> hostname) in
           sendstr s sock;
           sendintern
             sock (~> "_NET_WM_PID") false (fun resp ->
               let atom = r32 resp 8 in
               let pid = Unix.getpid () in
               let s = s32 pid in
               let s = changepropreq wid atom 6(*cardinal*) 32 s in
               sendstr s sock;
             )
       );

     state.actwin <- (fun () ->
       let s = Bytes.create 4 in
       let s = configurewindowreq wid 0x40 s in
       sendstr s state.sock;
       let s = mapreq wid in
       sendstr s state.sock;
     );

     sendintern
       sock (~> "_NET_ACTIVE_WINDOW") true (fun resp ->
         let atom = r32 resp 8 in
         state.actwin <- (fun () ->
           let data = Bytes.make 20 '\000' in
           let cm = clientmessage 32 0 wid atom data in
           let s = sendeventreq 0 root 0x180000 cm in
           sendstr s state.sock;
         );
       );

     syncsendintern
       sock 2.0 (~> "WM_CLASS") false (fun resp ->
         let atom = r32 resp 8 in
         let llpp = ~> "llpp\000llpp\000" in
         let s = changepropreq wid atom 31 8 llpp in
         sendstr s sock;
       );

     let s = getkeymapreq state.mink (state.maxk-state.mink) in
     sendwithrep sock s (updkmap sock);

     let s = getmodifiermappingreq () in
     sendwithrep sock s (updmodmap sock);

     sendintern
       sock (~> "UTF8_STRING") true (fun resp ->
         let atom = r32 resp 8 in
         if atom != 0
         then state.stringatom <- atom;
       );

     let setwmname s =
       let s = changepropreq wid 39 state.stringatom 8 s in
       sendstr s state.sock;
     in
     state.setwmname <- setwmname;
     sendintern
       sock (~> "_NET_WM_NAME") true (fun resp ->
         let atom = r32 resp 8 in
         if atom != 0
         then state.setwmname <- (fun s ->
           setwmname s;
           let s = changepropreq wid atom state.stringatom 8 s in
           sendstr s state.sock;
         );
       );

     sendintern
       sock (~> "_NET_WM_STATE") true (fun resp ->
         state.nwmsatom <- r32 resp 8;
         if state.nwmsatom != 0
         then (
           sendintern sock (~> "_NET_WM_STATE_MAXIMIZED_VERT") true (fun resp ->
               state.maxvatom <- r32 resp 8;
             );
           sendintern sock (~> "_NET_WM_STATE_MAXIMIZED_HORZ") true (fun resp ->
               state.maxhatom <- r32 resp 8;
             );
           sendintern
             sock (~> "_NET_WM_STATE_FULLSCREEN") false (fun resp ->
               state.fulsatom <- r32 resp 8;
               if state.fulsatom != 0
               then
                 state.fullscreen <-
                   (fun wid ->
                     let data = Bytes.make 20 '\000' in
                     let fs, f =
                       match state.fs with
                       | NoFs -> Fs (-1, -1, -1, -1), 1
                       | Fs _ -> NoFs, 0
                     in
                     w32 data 0 f;
                     w32 data 4 state.fulsatom;

                     let cm = clientmessage 32 0 wid state.nwmsatom data in
                     let s = sendeventreq 0 root 0x180000 cm in
                     sendstr s sock;
                     state.fs <- fs;
                   );
             );
         );
       );
     let s = queryextensionreq (~> "XKEYBOARD") in
     sendwithrep
       sock s (fun resp ->
         let present = r8 resp 8 in
         if present != 0
         then (
           let maj = r8 resp 9 in
           let s = Bytes.create 8 in
           w8 s 0 maj;   (* XKB *)
           w8 s 1 0;     (* XKBUseExtension *)
           w16 s 2 2;    (* request-length *)
           w16 s 4 1;    (* wantedMajor *)
           w16 s 6 0;    (* watnedMinor *)
           sendwithrep
             sock s
             (fun resp ->
               let supported = r8 resp 1 in
               state.xkb <- supported != 0
             )
         );
       );
     let s = getgeometryreq wid in
     syncsendwithrep sock 2.0 s (fun resp ->
         glxcompleteinit ();
         let w = r16 resp 16
         and h = r16 resp 18 in
         state.w <- w;
         state.h <- h;
       );

  | c -> error "unknown connection setup response %d" (Char.code c)

let getauth haddr dnum =
  let haddr =
    if emptystr haddr || haddr = "localhost"
    then
      try Unix.gethostname ()
      with exn ->
        dolog "failed to resolve `%S': %s" haddr @@ exntos exn;
        haddr
    else haddr
  in
  let path, warn =
    try Sys.getenv "XAUTHORITY", true
    with Not_found ->
      try Filename.concat (Sys.getenv "HOME") ".Xauthority", false
      with Not_found -> E.s, false
  in
  let readauth ic =
    let r16be s =
      let rb pos = Char.code (Bytes.get s pos) in
      (rb 1) lor ((rb 0) lsl 8)
    in
    let rec find () =
      let rs () =
        let s = really_input_string ic 2 in
        let n = r16be (~> s) in
        really_input_string ic n
      in
      let family = really_input_string ic 2 in
      let addr = rs () in
      let nums = rs () in
      let optnum =
        match int_of_string nums with
        | n -> Some n
        | exception exn ->
           if nonemptystr nums
           then
             dolog "display number(%S) is not an integer (corrupt %S?): %s"
               nums path @@ exntos exn
           ;
           None
      in
      let name = rs () in
      let data = rs () in

      vlog "family %S addr %S(%S) num %S(%d) name %S data %S"
        family addr haddr nums dnum name data;
      match optnum with
      | Some num when addr = haddr && num = dnum ->
         name, data
      | _ -> find ()
    in
    let name, data =
      try find ()
      with
      | End_of_file -> E.s, E.s
      | exn ->
         dolog "exception while reading X authority data (%S): %s"
           path @@ exntos exn;
         E.s, E.s
    in
    close_in ic;
    name, data;
  in
  if emptystr path
  then E.s, E.s
  else
    match open_in_bin path with
    | ic -> readauth ic
    | exception exn ->
       if warn
       then
         dolog "failed to open X authority file `%S' : %s" path @@ exntos exn
       ;
       E.s, E.s

let init t w h =
  let d =
    try Sys.getenv "DISPLAY"
    with exn ->
      error "cannot get DISPLAY evironment variable: %s" @@ exntos exn
  in
  let getnum w b e =
    if b = e
    then error "invalid DISPLAY(%s) %S" w d
    else
      let s = String.sub d b (e - b) in
      try int_of_string s
      with exn ->
        error "invalid DISPLAY %S can not parse %s(%S): %s" d w s @@ exntos exn
  in
  let rec phost pos =
    if pos = String.length d
    then error "invalid DISPLAY %S no display number specified" d
    else (
      if d.[pos] = ':'
      then
        let rec pdispnum pos1 =
          if pos1 = String.length d
          then getnum "display number" (pos+1) pos1, 0
          else
            match d.[pos1] with
            | '.' ->
               let dispnum = getnum "display number" (pos+1) pos1 in
               let rec pscreennum pos2 =
                 if pos2 = String.length d
                 then getnum "screen number" (pos1+1) pos2
                 else
                   match d.[pos2] with
                   | '0' .. '9' -> pscreennum (pos2+1)
                   | _ ->
                      error "invalid DISPLAY %S, cannot parse screen number" d
               in
               dispnum, pscreennum (pos1+1)
            | '0' .. '9' -> pdispnum (pos1+1)
            | _ -> error "invalid DISPLAY %S, cannot parse display number" d
        in
        String.sub d 0 pos, pdispnum (pos+1)
      else phost (pos+1)
    )
  in
  let host, (dispnum, screennum) = phost 0 in
  let aname, adata = getauth host dispnum in
  let fd =
    let fd, addr =
      if emptystr host || host.[0] = '/' || host = "unix"
      then
        (Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0,
         Unix.ADDR_UNIX ("\000/tmp/.X11-unix/X" ^ string_of_int dispnum))
      else
        let h =
          try Unix.gethostbyname host
          with exn -> error "cannot resolve %S: %s" host @@ exntos exn
        in
        let addr = h.Unix.h_addr_list.(0) in
        let port = 6000 + dispnum in
        let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        fd, (Unix.ADDR_INET (addr, port))
    in
    try Unix.connect fd addr; fd
    with exn -> error "failed to connect to X: %s" @@ exntos exn
  in
  cloexec fd;
  let s = Bytes.create 12 in
  let s = padcat s (~> aname) in
  let s = padcat s (~> adata) in
  Bytes.set s 0 ordermagic;
  w16 s 2 11;
  w16 s 4 0;
  w16 s 6 (String.length aname);
  w16 s 8 (String.length adata);
  sendstr1 s 0 (Bytes.length s) fd;
  state.sock <- fd;
  setup d fd 0 screennum w h;
  state.t <- t;
  fd, state.w, state.h

let setcursor cursor =
  if cursor != state.curcurs
  then (
    setcursor cursor;
    state.curcurs <- cursor;
  )

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
  for i = 0 to 29 do add ("f" ^ string_of_int (i+1)) [] (0xffbe + i) done;
  add "space" [] 0x20;
  add "ret" ["return"; "enter"] 0xff0d;
  add "tab" [] 0xff09;
  add "left" [] 0xff51;
  add "right" [] 0xff53;
  add "home" [] 0xff50;
  add "end" [] 0xff57;
  add "ins" ["insert"] 0xff63;
  add "del" ["delete"] 0xffff;
  add "esc" ["escape"] 0xff1b;
  add "pgup" ["pageup"] 0xff55;
  add "pgdown" ["pagedown"] 0xff56;
  add "backspace" [] 0xff08;
  add "up" [] 0xff52;
  add "down" [] 0xff54;
  add "menu" [] 0xff67;
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
  | 0xff08 -> Backspace
  | 0xff0d -> Enter
  | 0xff1b -> Escape
  | 0xff50 -> Home
  | 0xff51 -> Left
  | 0xff52 -> Up
  | 0xff53 -> Right
  | 0xff54 -> Down
  | 0xff55 -> Prior
  | 0xff56 -> Next
  | 0xff57 -> End
  | 0xff63 -> Insert
  | 0xff8d -> Enter
  | 0xff95 -> Home
  | 0xff96 -> Left
  | 0xff97 -> Up
  | 0xff98 -> Right
  | 0xff99 -> Down
  | 0xff9a -> Prior
  | 0xff9b -> Next
  | 0xff9c -> End
  | 0xff9f -> Delete
  | 0xffab -> Ascii '+'
  | 0xffad -> Ascii '-'
  | 0xffff -> Delete
  | code when code > 31 && code < 128 -> Ascii (Char.unsafe_chr code)
  | code when code >= 0xffb0 && code <= 0xffb9 ->
     Ascii (Char.unsafe_chr (code - 0xffb0 + 0x30))
  | code when code >= 0xffbe && code <= 0xffc8 -> Fn (code - 0xffbe + 1)
  | code when code land 0xff00 = 0xff00 -> Ctrl code
  | code -> Code code
