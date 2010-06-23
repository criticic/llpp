open Format;;

let log fmt = Printf.kprintf prerr_endline fmt;;
let dolog fmt = Printf.kprintf prerr_endline fmt;;

external init : Unix.file_descr -> unit = "ml_init";;
external draw : int -> int -> int -> int -> string  -> unit = "ml_draw";;
external gettext : string -> (int * int * int * int) -> int -> bool -> unit =
    "ml_gettext";;
external checklink : string -> int -> int -> bool = "ml_checklink";;
external getlink : string -> int -> int -> (int * int) option = "ml_getlink";;
external getpagewh : int -> float array = "ml_getpagewh";;

type mstate = Msel of ((int * int) * (int * int)) | Mnone;;

type te =
    | TEstop
    | TEdone of string
    | TEcont of string
;;

type 'a circbuf =
    { store : 'a array
    ; mutable rc : int
    ; mutable wc : int
    ; mutable len : int
    }
;;

let cbnew n v =
  { store = Array.create n v
  ; rc = 0
  ; wc = 0
  ; len = 0
  }
;;

let cblen b = Array.length b.store;;

let cbput b v =
  let len = cblen b in
  b.store.(b.wc) <- v;
  b.wc <- (b.wc + 1) mod len;
  b.len <- (b.len + 1) mod len;
;;

let cbpeekw b = b.store.(b.wc);;

let cbget b =
  let v = b.store.(b.rc) in
  if b.len = 0
  then
    v
  else (
    let rc = if b.rc = 0 then b.len - 1 else b.rc - 1 in
    b.rc <- rc;
    v
  )
;;

let cbrfollowlen b =
  b.rc <- b.len - 1;
;;

type layout =
    { pageno : int
    ; pagedimno : int
    ; pagew : int
    ; pageh : int
    ; pagedispy : int
    ; pagey : int
    ; pagevh : int
    }
;;

type conf =
    { mutable scrollw : int
    ; mutable scrollh : int
    ; mutable rectsel : bool
    ; mutable icase : bool
    ; mutable preload : bool
    ; mutable titletext : bool
    ; mutable pagebias : int
    ; mutable redispimm : bool
    ; mutable verbose : bool
    ; mutable scrollincr : int
    ; mutable maxhfit : bool
    ; mutable markonquit : bool
    ; mutable crophack : bool
    }
;;

type outline = string * int * int * int;;
type outlines = Oarray of outline array | Olist of outline list;;

type state =
    { mutable csock : Unix.file_descr
    ; mutable ssock : Unix.file_descr
    ; mutable w : int
    ; mutable h : int
    ; mutable y : int
    ; mutable prevy : int
    ; mutable maxy : int
    ; mutable layout : layout list
    ; pagemap : ((int * int), string) Hashtbl.t
    ; mutable pages : (int * int * int) list
    ; mutable pagecount : int
    ; pagecache : string circbuf
    ; navhist : float circbuf
    ; mutable inflight : int
    ; mutable mstate : mstate
    ; mutable searchpattern : string
    ; mutable rects : (int * int * Gl.point2 * Gl.point2) list
    ; mutable rects1 : (int * int * Gl.point2 * Gl.point2) list
    ; mutable text : string
    ; mutable fullscreen : (int * int) option
    ; mutable textentry :
        (char * string * (string -> int -> te) * (string -> unit)) option
    ; mutable outlines : outlines
    ; mutable outline : (bool * int * int * outline array * string) option
    ; mutable bookmarks : outline list
    ; mutable path : string
    }
;;

let conf =
  { scrollw = 5
  ; scrollh = 12
  ; icase = true
  ; rectsel = true
  ; preload = false
  ; titletext = false
  ; pagebias = 0
  ; redispimm = false
  ; verbose = false
  ; scrollincr = 24
  ; maxhfit = true
  ; markonquit = false
  ; crophack = false
  }
;;

let state =
  { csock = Unix.stdin
  ; ssock = Unix.stdin
  ; w = 900
  ; h = 900
  ; y = 0
  ; prevy = 0
  ; layout = []
  ; maxy = max_int
  ; pagemap = Hashtbl.create 10
  ; pagecache = cbnew 10 ""
  ; pages = []
  ; pagecount = 0
  ; inflight = 0
  ; mstate = Mnone
  ; navhist = cbnew 100 0.0
  ; rects = []
  ; rects1 = []
  ; text = ""
  ; fullscreen = None
  ; textentry = None
  ; searchpattern = ""
  ; outlines = Olist []
  ; outline = None
  ; bookmarks = []
  ; path = ""
  }
;;

let vlog fmt =
  if conf.verbose
  then
    Printf.kprintf prerr_endline fmt
  else
    Printf.kprintf ignore fmt
;;

let writecmd fd s =
  let len = String.length s in
  let n = 4 + len in
  let b = Buffer.create n in
  Buffer.add_char b (Char.chr ((len lsr 24) land 0xff));
  Buffer.add_char b (Char.chr ((len lsr 16) land 0xff));
  Buffer.add_char b (Char.chr ((len lsr  8) land 0xff));
  Buffer.add_char b (Char.chr ((len lsr  0) land 0xff));
  Buffer.add_string b s;
  let s' = Buffer.contents b in
  let n' = Unix.write fd s' 0 n in
  if n' != n then failwith "write failed";
;;

let readcmd fd =
  let s = "xxxx" in
  let n = Unix.read fd s 0 4 in
  if n != 4 then failwith "incomplete read(len)";
  let len = 0
    lor (Char.code s.[0] lsl 24)
    lor (Char.code s.[1] lsl 16)
    lor (Char.code s.[2] lsl  8)
    lor (Char.code s.[3] lsl  0)
  in
  let s = String.create len in
  let n = Unix.read fd s 0 len in
  if n != len then failwith "incomplete read(data)";
  s
;;

let yratio y =
  if y = state.maxy then 1.0
  else float y /. float state.maxy
;;

let makecmd s l =
  let b = Buffer.create 10 in
  Buffer.add_string b s;
  let rec combine = function
    | [] -> b
    | x :: xs ->
        Buffer.add_char b ' ';
        let s =
          match x with
          | `b b -> if b then "1" else "0"
          | `s s -> s
          | `i i -> string_of_int i
          | `f f -> string_of_float f
          | `I f -> string_of_int (truncate f)
        in
        Buffer.add_string b s;
        combine xs;
  in
  combine l;
;;

let wcmd s l =
  let cmd = Buffer.contents (makecmd s l) in
  writecmd state.csock cmd;
;;

let calcheight () =
  let rec f pn ph fh l =
    match l with
    | (n, _, h) :: rest ->
        let fh = fh + (n - pn) * ph in
        f n h fh rest

    | [] ->
        let fh = fh + (ph * (state.pagecount - pn)) in
        max 0 fh
  in
  let fh = f 0 0 0 state.pages in
  fh;
;;

let getpagey pageno =
  let rec f pn ph y l =
    match l with
    | (n, _, h) :: rest ->
        if n >= pageno
        then
          y + (pageno - pn) * ph
        else
          let y = y + (n - pn) * ph in
          f n h y rest

    | [] ->
        y + (pageno - pn) * ph
  in
  f 0 0 0 state.pages;
;;

let layout y sh =
  let rec f pageno pdimno prev vy py dy l cacheleft accu =
    if pageno = state.pagecount || cacheleft = 0
    then accu
    else
      let ((_, w, h) as curr), rest, pdimno =
        match l with
        | ((pageno', _, _) as curr) :: rest when pageno' = pageno ->
            curr, rest, pdimno + 1
        | _ ->
            prev, l, pdimno
      in
      let pageno' = pageno + 1 in
      if py + h > vy
      then
        let py' = vy - py in
        let vh = h - py' in
        if dy + vh > sh
        then
          let vh = sh - dy in
          if vh <= 0
          then
            accu
          else
            let e =
              { pageno = pageno
              ; pagedimno = pdimno
              ; pagew = w
              ; pageh = h
              ; pagedispy = dy
              ; pagey = py'
              ; pagevh = vh
              }
            in
            e :: accu
        else
          let e =
            { pageno = pageno
            ; pagedimno = pdimno
            ; pagew = w
            ; pageh = h
            ; pagedispy = dy
            ; pagey = py'
            ; pagevh = vh
            }
          in
          let accu = e :: accu in
          f pageno' pdimno curr
            (vy + vh) (py + h) (dy + vh + 2) rest
            (pred cacheleft) accu
      else
        f pageno' pdimno curr vy (py + h) dy rest cacheleft accu
  in
  let accu = f 0 ~-1 (0,0,0) y 0 0 state.pages (cblen state.pagecache) [] in
  state.maxy <- calcheight ();
  List.rev accu
;;

let clamp incr =
  let y = state.y + incr in
  let y = max 0 y in
  let y = min y (state.maxy - (if conf.maxhfit then state.h else 0)) in
  y;
;;

let gotoy y =
  let y = max 0 y in
  let y = min state.maxy y in
  let pages = layout y state.h in
  state.y <- y;
  state.layout <- pages;
  if conf.redispimm
  then
    Glut.postRedisplay ()
  ;
;;

let addnav () =
  cbput state.navhist (yratio state.y);
  cbrfollowlen state.navhist;
;;

let getnav () =
  let y = cbget state.navhist in
  truncate (y *. float state.maxy)
;;

let gotopage n top =
  let y = getpagey n in
  addnav ();
  state.y <- y + top;
  gotoy state.y;
;;

let reshape ~w ~h =
  let ratio = float w /. float state.w in
  let fixbookmark (s, l, pageno, pagey) =
    let pagey = truncate (float pagey *. ratio) in
    (s, l, pageno, pagey)
  in
  state.bookmarks <- List.map fixbookmark state.bookmarks;
  state.w <- w;
  state.h <- h;
  GlDraw.viewport 0 0 w h;
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.rotate ~x:1.0 ~angle:180.0 ();
  GlMat.translate ~x:~-.1.0 ~y:~-.1.0 ();
  GlMat.scale3 (2.0 /. float w, 2.0 /. float state.h, 1.0);
  GlClear.color (1., 1., 1.);
  GlClear.clear [`color];
  state.layout <- [];
  state.pages <- [];
  state.rects <- [];
  state.text <- "";
  wcmd "geometry" [`i (state.w - conf.scrollw); `i h];
;;

let showtext c s =
  if not conf.titletext
  then (
    GlDraw.color (0.0, 0.0, 0.0);
    GlDraw.rect
      (0.0, float (state.h - 18))
      (float (state.w - conf.scrollw - 1), float state.h)
    ;
    let font = Glut.BITMAP_8_BY_13 in
    GlDraw.color (1.0, 1.0, 1.0);
    GlPix.raster_pos ~x:0.0 ~y:(float (state.h - 5)) ();
    Glut.bitmapCharacter ~font ~c:(Char.code c);
    String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c)) s
  )
  else
    let len = String.length s in
    let dst = String.create (len + 1) in
    dst.[0] <- c;
    StringLabels.blit
      ~src:s
      ~dst
      ~src_pos:0
      ~dst_pos:1
      ~len
    ;
    Glut.setWindowTitle ~title:dst
;;

let enttext () =
  let len = String.length state.text in
  match state.textentry with
  | None ->
      if len > 0 then showtext ' ' state.text

  | Some (c, text, _, _) ->
      let s =
        if len > 0
        then
          text ^ " [" ^ state.text ^ "]"
        else
          text
      in
      showtext c s;
;;

let act cmd =
  match cmd.[0] with
  | 'c' ->
      state.pages <- [];
      state.outlines <- Olist []

  | 'D' ->
      state.rects <- state.rects1;
      Glut.postRedisplay ()

  | 'd' ->
      state.rects <- state.rects1;
      Glut.postRedisplay ()

  | 'C' ->
      let n = Scanf.sscanf cmd "C %d" (fun n -> n) in
      state.pagecount <- n;
      let rely = yratio state.y in
      let maxy = calcheight () in
      state.y <- truncate (float maxy *. rely);
      let pages = layout state.y state.h in
      state.layout <- pages;
      Glut.postRedisplay ();

  | 't' ->
      let s = Scanf.sscanf cmd "t %n"
        (fun n -> String.sub cmd n (String.length cmd - n))
      in
      Glut.setWindowTitle s

  | 'T' ->
      let s = Scanf.sscanf cmd "T %n"
        (fun n -> String.sub cmd n (String.length cmd - n))
      in
      state.text <- s;
      showtext ' ' s;
      Glut.swapBuffers ();
      (* Glut.postRedisplay () *)

  | 'F' ->
      let pageno, c, x0, y0, x1, y1 =
        Scanf.sscanf cmd "F %d %d %f %f %f %f"
          (fun p c x0 y0 x1 y1 -> (p, c, x0, y0, x1, y1))
      in
      let y = (getpagey pageno) + truncate y0 in
      addnav ();
      gotoy y;
      state.rects1 <- [pageno, c, (x0, y0), (x1, y1)]

  | 'R' ->
      let pageno, c, x0, y0, x1, y1 =
        Scanf.sscanf cmd "R %d %d %f %f %f %f"
          (fun pageno c x0 y0 x1 y1 -> (pageno, c, x0, y0, x1, y1))
      in
      state.rects1 <- (pageno, c, (x0, y0), (x1, y1)) :: state.rects1

  | 'r' ->
      let n, w, h, p =
        Scanf.sscanf cmd "r %d %d %d %s"
          (fun n w h p -> (n, w, h, p))
      in
      Hashtbl.replace state.pagemap (n, w) p;
      let evicted = cbpeekw state.pagecache in
      if String.length evicted > 0
      then begin
        wcmd "free" [`s evicted];
        let l = Hashtbl.fold (fun k p a ->
          if evicted = p then k :: a else a) state.pagemap []
        in
        List.iter (fun k -> Hashtbl.remove state.pagemap k) l;
      end;
      cbput state.pagecache p;
      state.inflight <- pred state.inflight;
      Glut.postRedisplay ()

  | 'l' ->
      let (n, w, h) as pagelayout =
        Scanf.sscanf cmd "l %d %d %d" (fun n w h -> n, w, h)
      in
      state.pages <- pagelayout :: state.pages

  | 'o' ->
      let (l, n, t, pos) =
        Scanf.sscanf cmd "o %d %d %d %n" (fun l n t pos -> l, n, t, pos)
      in
      let s = String.sub cmd pos (String.length cmd - pos) in
      let outline = (s, l, n, t) in
      let outlines =
        match state.outlines with
        | Olist outlines -> Olist (outline :: outlines)
        | Oarray _ -> Olist [outline]
      in
      state.outlines <- outlines

  | _ ->
      log "unknown cmd `%S'" cmd
;;

let getopaque pageno =
  try Some (Hashtbl.find state.pagemap (pageno + 1, state.w - conf.scrollw))
  with Not_found -> None
;;

let cache pageno opaque =
  Hashtbl.replace state.pagemap (pageno + 1, state.w - conf.scrollw) opaque
;;

let validopaque opaque = String.length opaque > 0;;

let preload l =
  match getopaque l.pageno with
  | None when state.inflight < 2+0*(cblen state.pagecache) ->
      state.inflight <- succ state.inflight;
      cache l.pageno "";
      wcmd "render" [`i (l.pageno + 1)
                    ;`i l.pagedimno
                    ;`i l.pagew
                    ;`i l.pageh];

  | _ -> ()
;;

let idle () =
  if not conf.redispimm && state.y != state.prevy
  then (
    state.prevy <- state.y;
    Glut.postRedisplay ();
  )
  else
    let r, _, _ = Unix.select [state.csock] [] [] 0.0001 in

    begin match r with
    | [] ->
        if conf.preload then begin
          let h = state.h in
          let y = if state.y < state.h then 0 else state.y - state.h in
          let pages = layout y (h*3) in
          List.iter preload pages;
        end;

    | _ ->
        let cmd = readcmd state.csock in
        act cmd;
    end;
;;

let search pattern forward =
  if String.length pattern > 0
  then
    let pn, py =
      match state.layout with
      | [] -> 0, 0
      | l :: _ ->
          l.pageno, (l.pagey + if forward then 0 else 0*l.pagevh)
    in
    let cmd =
      let b = makecmd "search"
        [`b conf.icase; `i pn; `i py; `i (if forward then 1 else 0)]
      in
      Buffer.add_char b ',';
      Buffer.add_string b pattern;
      Buffer.add_char b '\000';
      Buffer.contents b;
    in
    writecmd state.csock cmd;
;;

let intentry text key =
  let c = Char.unsafe_chr key in
  match c with
  | '0' .. '9' ->
      let s = "x" in s.[0] <- c;
      let text = text ^ s in
      TEcont text

  | _ ->
      state.text <- Printf.sprintf "invalid char (%d, `%c')" key c;
      TEcont text
;;

let addchar s c =
  let b = Buffer.create (String.length s + 1) in
  Buffer.add_string b s;
  Buffer.add_char b c;
  Buffer.contents b;
;;

let textentry text key =
  let c = Char.unsafe_chr key in
  match c with
  | _ when key >= 32 && key < 127 ->
      let text = addchar text c in
      TEcont text

  | _ ->
      log "unhandled key %d char `%c'" key (Char.unsafe_chr key);
      TEcont text
;;

let optentry text key =
  let btos b = if b then "on" else "off" in
  let c = Char.unsafe_chr key in
  match c with
  | 'r' ->
      conf.rectsel <- not conf.rectsel;
      TEdone ("rectsel " ^ (btos conf.rectsel))

  | 'i' ->
      conf.icase <- not conf.icase;
      TEdone ("case insensitive search " ^ (btos conf.icase))

  | 'p' ->
      conf.preload <- not conf.preload;
      TEdone ("preload " ^ (btos conf.preload))

  | 't' ->
      conf.titletext <- not conf.titletext;
      TEdone ("titletext " ^ (btos conf.titletext))

  | 'd' ->
      conf.redispimm <- not conf.redispimm;
      TEdone ("immediate redisplay " ^ (btos conf.redispimm))

  | 'v' ->
      conf.verbose <- not conf.verbose;
      TEdone ("verbose " ^ (btos conf.verbose))

  | 'h' ->
      conf.maxhfit <- not conf.maxhfit;
      state.maxy <- state.maxy + (if conf.maxhfit then -state.h else state.h);
      TEdone ("maxhfit " ^ (btos conf.maxhfit))

  | 'q' ->
      conf.markonquit <- not conf.markonquit;
      TEdone ("bookmark on quit " ^ btos conf.markonquit)

  | 'c' ->
      conf.crophack <- not conf.crophack;
      TEdone ("crophack " ^ btos conf.crophack)

  | _ ->
      state.text <- Printf.sprintf "bad option %d `%c'" key c;
      TEstop
;;

let maxoutlinerows () = (state.h - 31) / 16;;

let enterselector allowdel outlines errmsg =
  if Array.length outlines = 0
  then (
    showtext ' ' errmsg;
    Glut.swapBuffers ()
  )
  else
    let pageno =
      match state.layout with
      | [] -> -1
      | {pageno=pageno} :: rest -> pageno
    in
    let active =
      let rec loop n =
        if n = Array.length outlines
        then 0
        else
          let (_, _, outlinepageno, _) = outlines.(n) in
          if outlinepageno >= pageno then n else loop (n+1)
      in
      loop 0
    in
    state.outline <-
      Some (allowdel, active, max 0 (active - maxoutlinerows ()), outlines, "");
    Glut.postRedisplay ();
;;

let enteroutlinemode () =
  let outlines =
    match state.outlines with
    | Oarray a -> a
    | Olist l ->
        let a = Array.of_list (List.rev l) in
        state.outlines <- Oarray a;
        a
  in
  enterselector false outlines "Documents has no outline";
;;

let enterbookmarkmode () =
  let bookmarks = Array.of_list state.bookmarks in
  enterselector true bookmarks "Documents has no bookmarks (yet)";
;;


let quickbookmark ?title () =
  match state.layout with
  | [] -> ()
  | l :: _ ->
      let title =
        match title with
        | None ->
            let sec = Unix.gettimeofday () in
            let tm = Unix.localtime sec in
            Printf.sprintf "Quick %d visited (%d/%d/%d %d:%d)"
              l.pageno
              tm.Unix.tm_mday
              tm.Unix.tm_mon
              (tm.Unix.tm_year + 1900)
              tm.Unix.tm_hour
              tm.Unix.tm_min
        | Some title -> title
      in
      state.bookmarks <-
        (title, 0, l.pageno, l.pagey) :: state.bookmarks
;;

let viewkeyboard ~key ~x ~y =
  let enttext te =
    state.textentry <- te;
    state.text <- "";
    enttext ();
    Glut.swapBuffers ()
  in
  match state.textentry with
  | None ->
      let c = Char.chr key in
      begin match c with
      | '\027' | 'q' ->
          exit 0

      | '\008' ->
          let y = getnav () in
          gotoy y

      | 'o' ->
          enteroutlinemode ()

      | 'u' ->
          state.rects <- [];
          state.text <- "";
          Glut.postRedisplay ()

      | '/' | '?' ->
          let ondone isforw s =
            state.searchpattern <- s;
            search s isforw
          in
          enttext (Some (c, "", textentry, ondone (c ='/')))

      | '+' ->
          let ondone s =
            let n =
              try int_of_string s with exc ->
                state.text <- Printf.sprintf "bad integer `%s': %s"
                  s (Printexc.to_string exc);
                max_int
            in
            if n != max_int
            then (
              conf.pagebias <- n;
              state.text <- "page bias is now " ^ string_of_int n;
            )
          in
          enttext (Some ('+', "", intentry, ondone))

      | '-' ->
          let ondone msg =
            state.text <- msg;
          in
          enttext (Some ('-', "", optentry, ondone))

      | '0' .. '9' ->
          let ondone s =
            let n =
              try int_of_string s with exc ->
                state.text <- Printf.sprintf "bad integer `%s': %s"
                  s (Printexc.to_string exc);
                -1
            in
            if n >= 0
            then (
              addnav ();
              state.y <- y;
              gotoy (getpagey (n + conf.pagebias - 1))
            )
          in
          let pageentry text key =
            match Char.unsafe_chr key with
            | 'g' -> TEdone text
            | _ -> intentry text key
          in
          let text = "x" in text.[0] <- c;
          enttext (Some (':', text, pageentry, ondone))

      | 'b' ->
          conf.scrollw <- if conf.scrollw > 0 then 0 else 5;
          reshape state.w state.h;

      | 'f' ->
          begin match state.fullscreen with
          | None ->
              state.fullscreen <- Some (state.w, state.h);
              Glut.fullScreen ()
          | Some (w, h) ->
              state.fullscreen <- None;
              Glut.reshapeWindow ~w ~h
          end

      | 'n' ->
          search state.searchpattern true

      | 'p' ->
          search state.searchpattern false

      | 't' ->
          begin match state.layout with
          | [] -> ()
          | l :: _ ->
              gotoy (state.y - l.pagey);
          end

      | ' ' | 'N' ->
          begin match List.rev state.layout with
          | [] -> ()
          | l :: _ ->
              gotoy (clamp (l.pageh - l.pagey))
          end

      | '\127' | 'P' ->
          begin match state.layout with
          | [] -> ()
          | l :: _ ->
              gotoy (clamp (-l.pageh));
          end

      | '=' ->
          let f (fn, ln) l =
            if fn = -1 then l.pageno, l.pageno else fn, l.pageno
          in
          let fn, ln = List.fold_left f (-1, -1) state.layout in
          let s =
            let maxy = state.maxy - (if conf.maxhfit then state.h else 0) in
            let percent = (100. *. (float state.y /. float maxy)) in
            if fn = ln
            then
              Printf.sprintf "Page %d of %d %.2f%%"
                (fn+1) state.pagecount percent
            else
              Printf.sprintf
                "Pages %d-%d of %d %.2f%%"
                (fn+1) (ln+1) state.pagecount percent
          in
          showtext ' ' s;
          Glut.swapBuffers ()

      | 'w' ->
          begin match state.layout with
          | [] -> ()
          | l :: _ ->
              Glut.reshapeWindow (l.pagew + conf.scrollw) l.pageh;
              Glut.postRedisplay ();
          end

      | '\'' ->
          enterbookmarkmode ()

      | 'm' ->
          let ondone s =
            match state.layout with
            | l :: _ ->
                state.bookmarks <- (s, 0, l.pageno, l.pagey) :: state.bookmarks
            | _ -> ()
          in
          enttext (Some ('~', "", textentry, ondone))

      | '~' ->
          quickbookmark ();
          showtext ' ' "Quick bookmark added";
          Glut.swapBuffers ()

      | 'z' ->
          begin match state.layout with
          | l :: _ ->
              let a = getpagewh l.pagedimno in
              let w, h =
                if conf.crophack
                then
                  (truncate (1.8 *. (a.(1) -. a.(0))),
                  truncate (1.4 *. (a.(3) -. a.(0))))
                else
                  (truncate (a.(1) -. a.(0)),
                  truncate (a.(3) -. a.(0)))
              in
              Glut.reshapeWindow (w + conf.scrollw) h;
              Glut.postRedisplay ();

          | [] -> ()
          end

      | _ ->
          vlog "huh? %d %c" key (Char.chr key);
      end

  | Some (c, text, onkey, ondone) when key = 8 ->
      let len = String.length text in
      if len = 0 || len = 1
      then (
        state.textentry <- None;
        Glut.postRedisplay ();
      )
      else (
        let s = String.sub text 0 (len - 1) in
        enttext (Some (c, s, onkey, ondone))
      )

  | Some (c, text, onkey, ondone) ->
      begin match Char.unsafe_chr key with
      | '\r' | '\n' ->
          ondone text;
          state.textentry <- None;
          Glut.postRedisplay ()

      | '\027' ->
          state.textentry <- None;
          Glut.postRedisplay ()

      | _ ->
          begin match onkey text key with
          | TEdone text ->
              state.textentry <- None;
              ondone text;
              Glut.postRedisplay ()

          | TEcont text ->
              enttext (Some (c, text, onkey, ondone));

          | TEstop ->
              state.textentry <- None;
              Glut.postRedisplay ()
          end;
      end;
;;

let outlinekeyboard ~key ~x ~y (allowdel, active, first, outlines, qsearch) =
  let search active pattern incr =
    let re = Str.regexp_case_fold pattern in
    let rec loop n =
      if n = Array.length outlines || n = -1 then None else
        let (s, _, _, _) = outlines.(n) in
        if
          (try ignore (Str.search_forward re s 0); true
            with Not_found -> false)
        then (
          let maxrows = (min (Array.length outlines) (maxoutlinerows ())) / 2 in
          if first > n

          then Some (n, max 0 (n - maxrows))
          else Some (n, max first (n - maxrows))
        )
        else loop (n + incr)
    in
    loop active
  in
  match key with
  | 27 ->
      if String.length qsearch = 0
      then (
        state.text <- "";
        state.outline <- None;
        Glut.postRedisplay ();
      )
      else (
          state.text <- "";
          state.outline <- Some (allowdel, active, first, outlines, "");
          Glut.postRedisplay ();
      )

  | 18 | 19 ->
      let incr = if key = 18 then -1 else 1 in
      let active, first =
        match search (active + incr) qsearch incr with
        | None -> active, first
        | Some af -> af
      in
      state.outline <- Some (allowdel, active, first, outlines, qsearch);
      Glut.postRedisplay ();

  | 8 ->
      let len = String.length qsearch in
      if len = 0
      then ()
      else (
        if len = 1
        then (
          state.text <- "";
          state.outline <- Some (allowdel, active, first, outlines, "");
        )
        else
          let qsearch = String.sub qsearch 0 (len - 1) in
          state.text <- qsearch;
          state.outline <- Some (allowdel, active, first, outlines, qsearch);
      );
      Glut.postRedisplay ()

  | 13 ->
      if active < Array.length outlines
      then (
        let (_, _, n, t) = outlines.(active) in
        gotopage n t;
      );
      state.text <- "";
      if allowdel then state.bookmarks <- Array.to_list outlines;
      state.outline <- None;
      Glut.postRedisplay ();

  | _ when key >= 32 && key < 127 ->
      let pattern = addchar qsearch (Char.chr key) in
      let pattern, active, first =
        match search active pattern 1 with
        | None -> qsearch, active, first
        | Some (active, first) -> (pattern, active, first)
      in
      state.text <- pattern;
      state.outline <- Some (allowdel, active, first, outlines, pattern);
      Glut.postRedisplay ()

  | 127 when allowdel ->
      let len = Array.length outlines - 1 in
      if len = 0
      then (
        state.outline <- None;
        state.bookmarks <- [];
      )
      else (
        let bookmarks = Array.init len
          (fun i ->
            let i = if i >= active then i + 1 else i in
            outlines.(i)
          )
        in
        state.outline <-
          Some (allowdel,
               min active (len-1),
               min first (len-1),
               bookmarks, qsearch)
        ;
      );
      Glut.postRedisplay ()

  | _ -> log "unknown key %d" key
;;

let keyboard ~key ~x ~y =
  match state.outline with
  | None -> viewkeyboard ~key ~x ~y
  | Some outline -> outlinekeyboard ~key ~x ~y outline
;;

let special ~key ~x ~y =
  match state.outline with
  | None ->
      let y =
        match key with
        | Glut.KEY_F3        -> search state.searchpattern true; state.y
        | Glut.KEY_UP        -> clamp (-conf.scrollincr)
        | Glut.KEY_DOWN      -> clamp conf.scrollincr
        | Glut.KEY_PAGE_UP   -> clamp (-state.h)
        | Glut.KEY_PAGE_DOWN -> clamp state.h
        | Glut.KEY_HOME -> addnav (); 0
        | Glut.KEY_END ->
            addnav ();
            state.maxy - (if conf.maxhfit then state.h else 0)
        | _ -> state.y
      in
      state.text <- "";
      gotoy y

  | Some (allowdel, active, first, outlines, qsearch) ->
      let maxrows = maxoutlinerows () in
      let navigate incr =
        let active = active + incr in
        let active = max 0 (min active (Array.length outlines - 1)) in
        let first =
          if active > first
          then
            let rows = active - first in
            if rows > maxrows then first + incr else first
          else active
        in
        state.outline <- Some (allowdel, active, first, outlines, qsearch);
        Glut.postRedisplay ()
      in
      match key with
      | Glut.KEY_UP        -> navigate ~-1
      | Glut.KEY_DOWN      -> navigate   1
      | Glut.KEY_PAGE_UP   -> navigate ~-maxrows
      | Glut.KEY_PAGE_DOWN -> navigate   maxrows

      | Glut.KEY_HOME ->
          state.outline <- Some (allowdel, 0, 0, outlines, qsearch);
          Glut.postRedisplay ()

      | Glut.KEY_END ->
          let active = Array.length outlines - 1 in
          let first = max 0 (active - maxrows) in
          state.outline <- Some (allowdel, active, first, outlines, qsearch);
          Glut.postRedisplay ()

      | _ -> ()
;;

let drawplaceholder l =
  if true
  then (
    GlDraw.color (0.2, 0.2, 0.2);
    GlDraw.color (1.0, 1.0, 1.0);
    GlDraw.rect
      (0.0, float l.pagedispy)
      (float l.pagew, float (l.pagedispy + l.pagevh))
    ;
    let x = 0.0
    and y = float (l.pagedispy + 13) in
    let font = Glut.BITMAP_8_BY_13 in
    GlDraw.color (0.0, 0.0, 0.0);
    GlPix.raster_pos ~x ~y ();
    String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c))
      ("Loading " ^ string_of_int l.pageno);
  )
  else (
    GlDraw.begins `quads;
    GlDraw.vertex2 (0.0, float l.pagedispy);
    GlDraw.vertex2 (float l.pagew, float l.pagedispy);
    GlDraw.vertex2 (float l.pagew, float (l.pagedispy + l.pagevh));
    GlDraw.vertex2 (0.0, float (l.pagedispy + l.pagevh));
    GlDraw.ends ();
  )
;;

let now () = Unix.gettimeofday ();;

let drawpage i l =
  begin match getopaque l.pageno with
  | Some opaque when validopaque opaque ->
      GlDraw.color (1.0, 1.0, 1.0);
      let a = now () in
      draw l.pagedispy l.pagew l.pagevh l.pagey opaque;
      let b = now () in
      let d = b-.a in
      vlog "draw %f sec" d;

  | Some _ ->
      drawplaceholder l

  | None ->
      drawplaceholder l;
      if state.inflight < cblen state.pagecache
      then (
        List.iter preload state.layout;
      )
      else (
        vlog "inflight %d" state.inflight;
      );
  end;
  GlDraw.color (0.5, 0.5, 0.5);
  GlDraw.rect
    (0., float i)
    (float (state.w - conf.scrollw), float (i + (l.pagedispy - i)))
  ;
  l.pagedispy + l.pagevh;
;;

let scrollindicator () =
  let maxy = state.maxy - (if conf.maxhfit then state.h else 0) in
  GlDraw.color (0.64 , 0.64, 0.64);
  GlDraw.rect
    (float (state.w - conf.scrollw), 0.)
    (float state.w, float state.h)
  ;
  GlDraw.color (0.0, 0.0, 0.0);
  let sh = (float (maxy + state.h) /. float state.h)  in
  let sh = float state.h /. sh in
  let sh = max sh (float conf.scrollh) in

  let percent =
    if state.y = state.maxy
    then 1.0
    else float state.y /. float maxy
  in
  let position = (float state.h -. sh) *. percent in

  let position =
    if position +. sh > float state.h
    then
      float state.h -. sh
    else
      position
  in
  GlDraw.rect
    (float (state.w - conf.scrollw), position)
    (float state.w, position +. sh)
  ;
;;

let showsel () =
  match state.mstate with
  | Mnone ->
      ()

  | Msel ((x0, y0), (x1, y1)) ->
      let y0' = min y0 y1
      and y1 = max y0 y1 in
      let y0 = y0' in
      let f l =
        if (y0 >= l.pagedispy && y0 <= (l.pagedispy + l.pagevh))
          || ((y1 >= l.pagedispy))  (* && y1 <= (dy + vh))) *)
        then
          match getopaque l.pageno with
          | Some opaque when validopaque opaque ->
              let oy = -l.pagey + l.pagedispy in
              gettext opaque (min x0 x1, y0, max x1 x0, y1) oy conf.rectsel
          | _ -> ()
      in
      List.iter f state.layout
;;

let showrects () =
  Gl.enable `blend;
  GlDraw.color (0.0, 0.0, 1.0) ~alpha:0.5;
  GlFunc.blend_func `src_alpha `one_minus_src_alpha;
  List.iter
    (fun (pageno, c, (x0, y0), (x1, y1)) ->
      List.iter (fun l ->
        if l.pageno = pageno
        then (
          let d = float (l.pagedispy - l.pagey) in
          GlDraw.color (0.0, 0.0, 1.0 /. float c) ~alpha:0.5;
          GlDraw.rect (x0, y0 +. d) (x1, y1 +. d)
        )
      ) state.layout
    ) state.rects
  ;
  Gl.disable `blend;
;;

let showoutline = function
  | None -> ()
  | Some (allowdel, active, first, outlines, qsearch) ->
      Gl.enable `blend;
      GlFunc.blend_func `src_alpha `one_minus_src_alpha;
      GlDraw.color (0., 0., 0.) ~alpha:0.85;
      GlDraw.rect (0., 0.) (float state.w, float state.h);
      Gl.disable `blend;

      GlDraw.color (1., 1., 1.);
      let font = Glut.BITMAP_9_BY_15 in
      let draw_string x y s =
        GlPix.raster_pos ~x ~y ();
        String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c)) s
      in
      let rec loop row =
        if row = Array.length outlines || (row - first) * 16 > state.h
        then ()
        else (
          let (s, l, _, _) = outlines.(row) in
          let y = (row - first) * 16 in
          let x = 5 + 5*l in
          if row = active
          then (
            Gl.enable `blend;
            GlDraw.polygon_mode `both `line;
            GlFunc.blend_func `src_alpha `one_minus_src_alpha;
            GlDraw.color (1., 1., 1.) ~alpha:0.9;
            GlDraw.rect (0., float (y + 1))
              (float (state.w - conf.scrollw - 1), float (y + 18));
            GlDraw.polygon_mode `both `fill;
            Gl.disable `blend;
            GlDraw.color (1., 1., 1.);
          );
          draw_string (float x) (float (y + 16)) s;
          loop (row+1)
        )
      in
      loop first
;;

let display () =
  let lasty = List.fold_left drawpage 0 (state.layout) in
  GlDraw.color (0.5, 0.5, 0.5);
  GlDraw.rect
    (0., float lasty)
    (float (state.w - conf.scrollw), float state.h)
  ;
  showrects ();
  scrollindicator ();
  showsel ();
  showoutline state.outline;
  enttext ();
  Glut.swapBuffers ();
;;

let getlink x y =
  let rec f = function
    | l :: rest ->
        begin match getopaque l.pageno with
        | Some opaque when validopaque opaque ->
            let y = y - l.pagedispy in
            if y > 0
            then
              let y = l.pagey + y in
              match getlink opaque x y with
              | None -> f rest
              | some -> some
            else
              f rest
        | _ ->
            f rest
        end
    | [] -> None
  in
  f state.layout
;;

let checklink x y =
  let rec f = function
    | l :: rest ->
        begin match getopaque l.pageno with
        | Some opaque when validopaque opaque ->
            let y = y - l.pagedispy in
            if y > 0
            then
              let y = l.pagey + y in
              if checklink opaque x y then true else f rest
            else
              f rest
        | _ ->
            f rest
        end
    | [] -> false
  in
  f state.layout
;;

let mouse ~button ~bstate ~x ~y =
  match button with
  | Glut.OTHER_BUTTON n when n == 3 || n == 4 && bstate = Glut.UP ->
      let incr =
        if n = 3
        then
          -conf.scrollincr
        else
          conf.scrollincr
      in
      let incr = incr * 2 in
      let y = clamp incr in
      gotoy y

  | Glut.LEFT_BUTTON when state.outline = None ->
      let dest = if bstate = Glut.DOWN then getlink x y else None in
      begin match dest with
      | Some (pageno, top) ->
          gotopage pageno top

      | None ->
          if bstate = Glut.DOWN
          then (
            Glut.setCursor Glut.CURSOR_CROSSHAIR;
            state.mstate <- Msel ((x, y), (x, y));
            Glut.postRedisplay ()
          )
          else (
            Glut.setCursor Glut.CURSOR_INHERIT;
            state.mstate <- Mnone;
          )
      end

  | _ ->
      ()
;;
let mouse ~button ~state ~x ~y = mouse button state x y;;

let motion ~x ~y =
  if state.outline = None
  then
    match state.mstate with
    | Mnone -> ()
    | Msel (a, _) ->
        state.mstate <- Msel (a, (x, y));
        Glut.postRedisplay ()
;;

let pmotion ~x ~y =
  if state.outline = None
  then
    match state.mstate with
    | Mnone when (checklink x y) ->
        Glut.setCursor Glut.CURSOR_INFO

    | Mnone ->
        Glut.setCursor Glut.CURSOR_INHERIT

    | Msel (a, _) ->
        ()
;;

let () =
  let statepath = (Sys.getenv "HOME") ^ "/.config/llpp" in
  let pstate =
    try
      let ic = open_in_bin statepath in
      let hash = input_value ic in
      close_in ic;
      hash
    with exn ->
      if false
      then
        prerr_endline ("Error loading state " ^ Printexc.to_string exn)
      ;
      Hashtbl.create 1
  in
  let savestate () =
    try
      let w, h =
        match state.fullscreen with
        | None -> state.w, state.h
        | Some wh -> wh
      in
      if conf.markonquit then quickbookmark ();
      Hashtbl.replace pstate state.path (state.bookmarks, w, h);
      let oc = open_out_bin statepath in
      output_value oc pstate
    with exn ->
      if false
      then
        prerr_endline ("Error saving state " ^ Printexc.to_string exn)
      ;
  in
  let setstate () =
    try
      let statebookmarks, statew, stateh = Hashtbl.find pstate state.path in
      state.w <- statew;
      state.h <- stateh;
      state.bookmarks <- statebookmarks;
    with Not_found -> ()
    | exn ->
      prerr_endline ("Error setting state " ^ Printexc.to_string exn)
  in

  Arg.parse [] (fun s -> state.path <- s) "options:";
  let name =
    if String.length state.path = 0
    then (prerr_endline "filename missing"; exit 1)
    else state.path
  in

  setstate ();
  let _ = Glut.init Sys.argv in
  let () = Glut.initDisplayMode ~depth:false ~double_buffer:true () in
  let () = Glut.initWindowSize state.w state.h in
  let _ = Glut.createWindow ("llpp " ^ Filename.basename name) in

  let csock, ssock = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in

  init ssock;
  state.csock <- csock;
  state.ssock <- ssock;
  writecmd csock ("open " ^ name ^ "\000");

  let () = Glut.displayFunc display in
  let () = Glut.reshapeFunc reshape in
  let () = Glut.keyboardFunc keyboard in
  let () = Glut.specialFunc special in
  let () = Glut.idleFunc (Some idle) in
  let () = Glut.mouseFunc mouse in
  let () = Glut.motionFunc motion in
  let () = Glut.passiveMotionFunc pmotion in

  at_exit savestate;

  let rec handlelablglutbug () =
    try
      Glut.mainLoop ();
    with Glut.BadEnum "key in special_of_int" ->
      showtext '!' " LablGlut bug: special key not recognized";
      Glut.swapBuffers ();
      handlelablglutbug ()
  in
  handlelablglutbug ();
;;
