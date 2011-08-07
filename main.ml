type under =
    | Unone
    | Ulinkuri of string
    | Ulinkgoto of (int * int)
    | Utext of facename
and facename = string;;

let log fmt = Printf.kprintf prerr_endline fmt;;
let dolog fmt = Printf.kprintf prerr_endline fmt;;

type params = angle * proportional * texcount * sliceheight
and pageno = int
and width = int
and height = int
and leftx = int
and opaque = string
and recttype = int
and pixmapsize = int
and angle = int
and proportional = bool
and interpagespace = int
and texcount = int
and sliceheight = int
and gen = int
;;

external init : Unix.file_descr -> params -> unit = "ml_init";;
external draw : (int * int * int * int * bool) -> string  -> unit = "ml_draw";;
external seltext : string -> (int * int * int * int) -> int -> unit =
  "ml_seltext";;
external copysel : string ->  unit = "ml_copysel";;
external getpdimrect : int -> float array = "ml_getpdimrect";;
external whatsunder : string -> int -> int -> under = "ml_whatsunder";;
external zoomforh : int -> int -> int -> float = "ml_zoom_for_height";;

type mpos = int * int
and mstate =
    | Msel of (mpos * mpos)
    | Mpan of mpos
    | Mscroll
    | Mnone
;;

type 'a circbuf =
    { store : 'a array
    ; mutable rc : int
    ; mutable wc : int
    ; mutable len : int
    }
;;

type textentry = (char * string * onhist * onkey * ondone)
and onkey = string -> int -> te
and ondone = string -> unit
and histcancel = unit -> unit
and onhist = ((histcmd -> string) * histcancel) option
and histcmd = HCnext | HCprev | HCfirst | HClast
and te =
    | TEstop
    | TEdone of string
    | TEcont of string
    | TEswitch of textentry
;;

let cbnew n v =
  { store = Array.create n v
  ; rc = 0
  ; wc = 0
  ; len = 0
  }
;;

let cbcap b = Array.length b.store;;

let cbput b v =
  let cap = cbcap b in
  b.store.(b.wc) <- v;
  b.wc <- (b.wc + 1) mod cap;
  b.rc <- b.wc;
  b.len <- min (b.len + 1) cap;
;;

let cbempty b = b.len = 0;;

let cbgetg b circular dir =
  if cbempty b
  then b.store.(0)
  else
    let rc = b.rc + dir in
    let rc =
      if circular
      then (
        if rc = -1
        then b.len-1
        else (
          if rc = b.len
          then 0
          else rc
        )
      )
      else max 0 (min rc (b.len-1))
    in
    b.rc <- rc;
    b.store.(rc);
;;

let cbget b = cbgetg b false;;
let cbgetc b = cbgetg b true;;

let cbpeek b =
  let rc = b.wc - b.len in
  let rc = if rc < 0 then cbcap b + rc else rc in
  b.store.(rc);
;;

let cbdecr b = b.len <- b.len - 1;;

type layout =
    { pageno : int
    ; pagedimno : int
    ; pagew : int
    ; pageh : int
    ; pagedispy : int
    ; pagey : int
    ; pagevh : int
    ; pagex : int
    }
;;

type conf =
    { mutable scrollw : int
    ; mutable scrollh : int
    ; mutable icase : bool
    ; mutable preload : bool
    ; mutable pagebias : int
    ; mutable verbose : bool
    ; mutable scrollincr : int
    ; mutable maxhfit : bool
    ; mutable crophack : bool
    ; mutable autoscroll : bool
    ; mutable showall : bool
    ; mutable hlinks : bool
    ; mutable underinfo : bool
    ; mutable interpagespace : interpagespace
    ; mutable zoom : float
    ; mutable presentation : bool
    ; mutable angle : angle
    ; mutable winw : int
    ; mutable winh : int
    ; mutable savebmarks : bool
    ; mutable proportional : proportional
    ; mutable memlimit : int
    ; mutable texcount : texcount
    ; mutable sliceheight : sliceheight
    }
;;

type outline = string * int * int * float;;
type outlines =
    | Oarray of outline array
    | Olist of outline list
    | Onarrow of string * outline array * outline array
;;

type rect = (float * float * float * float * float * float * float * float);;

type pagemapkey = (pageno * width * angle * proportional * gen);;

type state =
    { mutable csock : Unix.file_descr
    ; mutable ssock : Unix.file_descr
    ; mutable w : int
    ; mutable x : int
    ; mutable y : int
    ; mutable maxy : int
    ; mutable layout : layout list
    ; pagemap : (pagemapkey, (opaque * pixmapsize)) Hashtbl.t
    ; mutable pdims : (pageno * width * height * leftx) list
    ; mutable pagecount : int
    ; pagecache : string circbuf
    ; mutable rendering : bool
    ; mutable mstate : mstate
    ; mutable searchpattern : string
    ; mutable rects : (pageno * recttype * rect) list
    ; mutable rects1 : (pageno * recttype * rect) list
    ; mutable text : string
    ; mutable fullscreen : (width * height) option
    ; mutable birdseye : (conf * leftx) option
    ; mutable textentry : textentry option
    ; mutable outlines : outlines
    ; mutable outline : (bool * int * int * outline array * string) option
    ; mutable bookmarks : outline list
    ; mutable path : string
    ; mutable password : string
    ; mutable invalidated : int
    ; mutable colorscale : float
    ; mutable memused : int
    ; mutable birdseyepageno : pageno
    ; mutable gen : gen
    ; mutable throttle : layout list option
    ; hists : hists
    }
and hists =
    { pat : string circbuf
    ; pag : string circbuf
    ; nav : float circbuf
    }
;;

let defconf =
  { scrollw = 7
  ; scrollh = 12
  ; icase = true
  ; preload = true
  ; pagebias = 0
  ; verbose = false
  ; scrollincr = 24
  ; maxhfit = true
  ; crophack = false
  ; autoscroll = false
  ; showall = false
  ; hlinks = false
  ; underinfo = false
  ; interpagespace = 2
  ; zoom = 1.0
  ; presentation = false
  ; angle = 0
  ; winw = 900
  ; winh = 900
  ; savebmarks = true
  ; proportional = true
  ; memlimit = 32*1024*1024
  ; texcount = 256
  ; sliceheight = 24
  }
;;

let conf = { defconf with angle = defconf.angle };;

let state =
  { csock = Unix.stdin
  ; ssock = Unix.stdin
  ; w = 0
  ; y = 0
  ; x = 0
  ; layout = []
  ; maxy = max_int
  ; pagemap = Hashtbl.create 10
  ; pagecache = cbnew 100 ""
  ; pdims = []
  ; pagecount = 0
  ; rendering = false
  ; mstate = Mnone
  ; rects = []
  ; rects1 = []
  ; text = ""
  ; fullscreen = None
  ; birdseye = None
  ; textentry = None
  ; searchpattern = ""
  ; outlines = Olist []
  ; outline = None
  ; bookmarks = []
  ; path = ""
  ; password = ""
  ; invalidated = 0
  ; hists =
      { nav = cbnew 100 0.0
      ; pat = cbnew 20 ""
      ; pag = cbnew 10 ""
      }
  ; colorscale = 1.0
  ; memused = 0
  ; birdseyepageno = 0
  ; gen = 0
  ; throttle = None
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
  if y = state.maxy
  then 1.0
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

let calcips h =
  if conf.presentation
  then
    let d = conf.winh - h in
    max 0 ((d + 1) / 2)
  else
    conf.interpagespace
;;

let calcheight () =
  let rec f pn ph pi fh l =
    match l with
    | (n, _, h, _) :: rest ->
        let ips = calcips h in
        let fh =
          if conf.presentation
          then fh+ips
          else (
            if state.birdseye <> None && pn = 0
            then fh + ips
            else fh
          )
        in
        let fh = fh + ((n - pn) * (ph + pi)) in
        f n h ips fh rest

    | [] ->
        let inc =
          if conf.presentation || (state.birdseye <> None && pn = 0)
          then 0
          else -pi
        in
        let fh = fh + ((state.pagecount - pn) * (ph + pi)) + inc in
        max 0 fh
  in
  let fh = f 0 0 0 0 state.pdims in
  fh;
;;

let getpageyh pageno =
  let rec f pn ph pi y l =
    match l with
    | (n, _, h, _) :: rest ->
        let ips = calcips h in
        if n >= pageno
        then
          if conf.presentation && n = pageno
          then
            y + (pageno - pn) * (ph + pi) + pi, h
          else
            y + (pageno - pn) * (ph + pi), h
        else
          let y = y + (if conf.presentation then pi else 0) in
          let y = y + (n - pn) * (ph + pi) in
          f n h ips y rest

    | [] ->
        y + (pageno - pn) * (ph + pi), ph
  in
  f 0 0 0 0 state.pdims
;;

let getpagey pageno = fst (getpageyh pageno);;

let layout y sh =
  let rec f ~pageno ~pdimno ~prev ~py ~dy ~pdims ~cacheleft ~accu =
    let ((w, h, ips, x) as curr), rest, pdimno, yinc =
      match pdims with
      | (pageno', w, h, x) :: rest when pageno' = pageno ->
          let ips = calcips h in
          let yinc =
            if conf.presentation || (state.birdseye <> None && pageno = 0)
            then ips
            else 0
          in
          (w, h, ips, x), rest, pdimno + 1, yinc
      | _ ->
          prev, pdims, pdimno, 0
    in
    let dy = dy + yinc in
    let py = py + yinc in
    if pageno = state.pagecount || cacheleft = 0 || dy >= sh
    then
      accu
    else
      let vy = y + dy in
      if py + h <= vy - yinc
      then
        let py = py + h + ips in
        let dy = max 0 (py - y) in
        f ~pageno:(pageno+1)
          ~pdimno
          ~prev:curr
          ~py
          ~dy
          ~pdims:rest
          ~cacheleft
          ~accu
      else
        let pagey = vy - py in
        let pagevh = h - pagey in
        let pagevh = min (sh - dy) pagevh in
        let off = if yinc > 0 then py - vy else 0 in
        let py = py + h + ips in
        let e =
          { pageno = pageno
          ; pagedimno = pdimno
          ; pagew = w
          ; pageh = h
          ; pagedispy = dy + off
          ; pagey = pagey + off
          ; pagevh = pagevh - off
          ; pagex = x
          }
        in
        let accu = e :: accu in
        f ~pageno:(pageno+1)
          ~pdimno
          ~prev:curr
          ~py
          ~dy:(dy+pagevh+ips)
          ~pdims:rest
          ~cacheleft:(cacheleft-1)
          ~accu
  in
  if state.invalidated = 0
  then (
    let accu =
      f
        ~pageno:0
        ~pdimno:~-1
        ~prev:(0,0,0,0)
        ~py:0
        ~dy:0
        ~pdims:state.pdims
        ~cacheleft:(cbcap state.pagecache)
        ~accu:[]
    in
    List.rev accu
  )
  else
    []
;;

let clamp incr =
  let y = state.y + incr in
  let y = max 0 y in
  let y = min y (state.maxy - (if conf.maxhfit then conf.winh else 0)) in
  y;
;;

let getopaque pageno =
  try Some (Hashtbl.find state.pagemap
               (pageno, state.w, conf.angle, conf.proportional, state.gen))
  with Not_found -> None
;;

let cache pageno opaque =
  Hashtbl.replace state.pagemap
    (pageno, state.w, conf.angle, conf.proportional, state.gen) opaque
;;

let validopaque opaque = String.length opaque > 0;;

let render l =
  match getopaque l.pageno with
  | None when not state.rendering ->
      state.rendering <- true;
      cache l.pageno ("", -1);
      wcmd "render" [`i (l.pageno + 1)
                    ;`i l.pagedimno
                    ;`i l.pagew
                    ;`i l.pageh];

  | _ -> ()
;;

let loadlayout layout =
  let rec f all = function
    | l :: ls ->
        begin match getopaque l.pageno with
        | None -> render l; f false ls
        | Some (opaque, _) -> f (all && validopaque opaque) ls
        end
    | [] -> all
  in
  f (layout <> []) layout;
;;

let findpageforopaque opaque =
  Hashtbl.fold
    (fun k (v, s) a -> if v = opaque then Some (k, s) else a)
    state.pagemap None
;;

let pagevisible layout n = List.exists (fun l -> l.pageno = n) layout;;

let preload () =
  if conf.preload
  then
    let oktopreload =
      let memleft = conf.memlimit - state.memused in
      if memleft < 0
      then
        let opaque = cbpeek state.pagecache in
        match findpageforopaque opaque with
        | Some ((n, _, _, _, _), size) ->
            memleft + size >= 0 && not (pagevisible state.layout n)
        | None -> false
      else true
    in
    if oktopreload
    then
      let rely = yratio state.y in
      let presentation = conf.presentation in
      let interpagespace = conf.interpagespace in
      let maxy = state.maxy in
      conf.presentation <- false;
      conf.interpagespace <- 0;
      state.maxy <- calcheight ();
      let y = truncate (float state.maxy *. rely) in
      let y = if y < conf.winh then 0 else y - conf.winh in
      let pages = layout y (conf.winh*3) in
      List.iter render pages;
      conf.presentation <- presentation;
      conf.interpagespace <- interpagespace;
      state.maxy <- maxy;
;;

let gotoy y =
  let y = max 0 y in
  let y = min state.maxy y in
  let pages = layout y conf.winh in
  let ready = loadlayout pages in
  if conf.showall
  then (
    if ready
    then (
      state.y <- y;
      state.layout <- pages;
      state.throttle <- None;
      Glut.postRedisplay ();
    )
    else (
      state.throttle <- Some pages;
    )
  )
  else (
    state.y <- y;
    state.layout <- pages;
    state.throttle <- None;
    Glut.postRedisplay ();
  );
  if state.birdseye <> None
  then (
    if not (pagevisible pages state.birdseyepageno)
    then
      match state.layout with
      | [] -> ()
      | l :: _ -> state.birdseyepageno <- l.pageno
  );
  preload ();
;;

let gotoy_and_clear_text y =
  gotoy y;
  if not conf.verbose then state.text <- "";
;;

let addnav () =
  cbput state.hists.nav (yratio state.y);
;;

let getnav () =
  let y = cbgetc state.hists.nav ~-1 in
  truncate (y *. float state.maxy)
;;

let gotopage n top =
  let y, h = getpageyh n in
  addnav ();
  gotoy_and_clear_text (y + (truncate (top *. float h)));
;;

let gotopage1 n top =
  let y = getpagey n in
  addnav ();
  gotoy_and_clear_text (y + top);
;;

let invalidate () =
  state.layout <- [];
  state.pdims <- [];
  state.rects <- [];
  state.rects1 <- [];
  state.invalidated <- state.invalidated + 1;
;;

let scalecolor c =
  let c = c *. state.colorscale in
  (c, c, c);
;;

let represent () =
  let y =
    match state.layout with
    | [] ->
        let rely = yratio state.y in
        state.maxy <- calcheight ();
        truncate (float state.maxy *. rely)

    | l :: _ ->
        state.maxy <- calcheight ();
        getpagey l.pageno
  in
  gotoy y
;;

let pagematrix () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.rotate ~x:1.0 ~angle:180.0 ();
  GlMat.translate ~x:~-.1.0 ~y:~-.1.0 ();
  GlMat.scale3 (2.0 /. float state.w, 2.0 /. float conf.winh, 1.0);
;;

let winmatrix () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.rotate ~x:1.0 ~angle:180.0 ();
  GlMat.translate ~x:~-.1.0 ~y:~-.1.0 ();
  GlMat.scale3 (2.0 /. float conf.winw, 2.0 /. float conf.winh, 1.0);
;;

let reshape ~w ~h =
  conf.winw <- w;
  let w = truncate (float w *. conf.zoom) - conf.scrollw in
  let w = max w 2 in
  state.w <- w;
  conf.winh <- h;
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.color (scalecolor 1.0);
  GlClear.clear [`color];

  invalidate ();
  wcmd "geometry" [`i w; `i h];
;;

let showtext c s =
  GlDraw.color (0.0, 0.0, 0.0);
  GlDraw.rect
    (0.0, float (conf.winh - 18))
    (float (conf.winw - conf.scrollw - 1), float conf.winh)
  ;
  let font = Glut.BITMAP_8_BY_13 in
  GlDraw.color (1.0, 1.0, 1.0);
  GlPix.raster_pos ~x:0.0 ~y:(float (conf.winh - 5)) ();
  Glut.bitmapCharacter ~font ~c:(Char.code c);
  String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c)) s;
;;

let enttext () =
  let len = String.length state.text in
  match state.textentry with
  | None ->
      if len > 0 then showtext ' ' state.text

  | Some (c, text, _, _, _) ->
      let s =
        if len > 0
        then
          text ^ " [" ^ state.text ^ "]"
        else
          text
      in
      showtext c s;
;;

let showtext c s =
  if true
  then (
    state.text <- Printf.sprintf "%c%s" c s;
    Glut.postRedisplay ();
  )
  else (
    showtext c s;
    Glut.swapBuffers ();
  )
;;

let act cmd =
  match cmd.[0] with
  | 'c' ->
      state.pdims <- [];

  | 'D' ->
      state.rects <- state.rects1;
      Glut.postRedisplay ()

  | 'C' ->
      let n = Scanf.sscanf cmd "C %u" (fun n -> n) in
      state.pagecount <- n;
      state.invalidated <- state.invalidated - 1;
      if state.invalidated = 0
      then represent ()

  | 't' ->
      let s = Scanf.sscanf cmd "t %n"
        (fun n -> String.sub cmd n (String.length cmd - n))
      in
      Glut.setWindowTitle s

  | 'T' ->
      let s = Scanf.sscanf cmd "T %n"
        (fun n -> String.sub cmd n (String.length cmd - n))
      in
      if state.textentry = None
      then (
        state.text <- s;
        showtext ' ' s;
      )
      else (
        state.text <- s;
        Glut.postRedisplay ();
      )

  | 'V' ->
      if conf.verbose
      then
        let s = Scanf.sscanf cmd "V %n"
          (fun n -> String.sub cmd n (String.length cmd - n))
        in
        state.text <- s;
        showtext ' ' s;

  | 'F' ->
      let pageno, c, x0, y0, x1, y1, x2, y2, x3, y3 =
        Scanf.sscanf cmd "F %u %d %f %f %f %f %f %f %f %f"
          (fun p c x0 y0 x1 y1 x2 y2 x3 y3 ->
            (p, c, x0, y0, x1, y1, x2, y2, x3, y3))
      in
      let y = (getpagey pageno) + truncate y0 in
      addnav ();
      gotoy y;
      state.rects1 <- [pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)]

  | 'R' ->
      let pageno, c, x0, y0, x1, y1, x2, y2, x3, y3 =
        Scanf.sscanf cmd "R %u %d %f %f %f %f %f %f %f %f"
          (fun p c x0 y0 x1 y1 x2 y2 x3 y3 ->
            (p, c, x0, y0, x1, y1, x2, y2, x3, y3))
      in
      state.rects1 <-
        (pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)) :: state.rects1

  | 'r' ->
      let n, w, h, r, l, s, p =
        Scanf.sscanf cmd "r %u %u %u %u %d %u %s"
          (fun n w h r l s p ->
            (n-1, w, h, r, l != 0, s, p))
      in

      Hashtbl.replace state.pagemap (n, w, r, l, state.gen) (p, s);
      state.memused <- state.memused + s;

      let layout =
        match state.throttle with
        | None -> state.layout
        | Some layout -> layout
      in

      let rec gc () =
        if (state.memused <= conf.memlimit) || cbempty state.pagecache
        then ()
        else (
          let evictedopaque = cbpeek state.pagecache in
          match findpageforopaque evictedopaque with
          | None -> failwith "bug in gc"
          | Some ((evictedn, _, _, _, gen) as k, evictedsize) ->
              if state.gen != gen || not (pagevisible layout evictedn)
              then (
                wcmd "free" [`s evictedopaque];
                state.memused <- state.memused - evictedsize;
                Hashtbl.remove state.pagemap k;
                cbdecr state.pagecache;
                gc ();
              )
        )
      in
      gc ();

      cbput state.pagecache p;
      state.rendering <- false;

      begin match state.throttle with
      | None ->
          if pagevisible state.layout n
          then gotoy state.y
          else (
            let allvisible = loadlayout state.layout in
            if allvisible then preload ();
          )

      | Some layout ->
          match layout with
          | [] -> ()
          | l :: _ ->
              let y = getpagey l.pageno + l.pagey in
              gotoy y
      end

  | 'l' ->
      let (n, w, h, x) as pdim =
        Scanf.sscanf cmd "l %u %u %u %u" (fun n w h x -> n, w, h, x)
      in
      state.pdims <- pdim :: state.pdims

  | 'o' ->
      let (l, n, t, h, pos) =
        Scanf.sscanf cmd "o %u %u %d %u %n" (fun l n t h pos -> l, n, t, h, pos)
      in
      let s = String.sub cmd pos (String.length cmd - pos) in
      let s =
        let l = String.length s in
        let b = Buffer.create (String.length s) in
        let rec loop pc2 i =
          if i = l
          then ()
          else
            let pc2 =
              match s.[i] with
              | '\xa0' when pc2 -> Buffer.add_char b ' '; false
              | '\xc2' -> true
              | c ->
                  let c = if Char.code c land 0x80 = 0 then c else '?' in
                  Buffer.add_char b c;
                  false
            in
            loop pc2 (i+1)
        in
        loop false 0;
        Buffer.contents b
      in
      let outline = (s, l, n, float t /. float h) in
      let outlines =
        match state.outlines with
        | Olist outlines -> Olist (outline :: outlines)
        | Oarray _ -> Olist [outline]
        | Onarrow _ -> Olist [outline]
      in
      state.outlines <- outlines

  | _ ->
      log "unknown cmd `%S'" cmd
;;

let now = Unix.gettimeofday;;

let idle () =
  let rec loop delay =
    let r, _, _ = Unix.select [state.csock] [] [] delay in
    begin match r with
    | [] ->
        if conf.autoscroll
        then begin
          let y = state.y + conf.scrollincr in
          let y = if y >= state.maxy then 0 else y in
          gotoy y;
          state.text <- "";
        end;

    | _ ->
        let cmd = readcmd state.csock in
        act cmd;
        loop 0.0
    end;
  in loop 0.001
;;

let onhist cb =
  let rc = cb.rc in
  let action = function
  | HCprev   -> cbget cb ~-1
  | HCnext   -> cbget cb 1
  | HCfirst  -> cbget cb ~-(cb.rc)
  | HClast   -> cbget cb (cb.len - 1 - cb.rc)
  and cancel () = cb.rc <- rc
  in (action, cancel)
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

let reinit angle proportional =
  conf.angle <- angle;
  conf.proportional <- proportional;
  invalidate ();
  wcmd "reinit" [`i angle; `b proportional];
;;

let optentry text key =
  let btos b = if b then "on" else "off" in
  let c = Char.unsafe_chr key in
  match c with
  | 's' ->
      let ondone s =
        try conf.scrollincr <- int_of_string s with exc ->
          state.text <- Printf.sprintf "bad integer `%s': %s"
            s (Printexc.to_string exc)
      in
      TEswitch ('#', "", None, intentry, ondone)

  | 'R' ->
      let ondone s =
        match try
            Some (int_of_string s)
          with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s"
              s (Printexc.to_string exc);
            None
        with
        | Some angle -> reinit angle conf.proportional
        | None -> ()
      in
      TEswitch ('^', "", None, intentry, ondone)

  | 'i' ->
      conf.icase <- not conf.icase;
      TEdone ("case insensitive search " ^ (btos conf.icase))

  | 'p' ->
      conf.preload <- not conf.preload;
      gotoy state.y;
      TEdone ("preload " ^ (btos conf.preload))

  | 'v' ->
      conf.verbose <- not conf.verbose;
      TEdone ("verbose " ^ (btos conf.verbose))

  | 'h' ->
      conf.maxhfit <- not conf.maxhfit;
      state.maxy <- state.maxy + (if conf.maxhfit then -conf.winh else conf.winh);
      TEdone ("maxhfit " ^ (btos conf.maxhfit))

  | 'c' ->
      conf.crophack <- not conf.crophack;
      TEdone ("crophack " ^ btos conf.crophack)

  | 'a' ->
      conf.showall <- not conf.showall;
      TEdone ("showall " ^ btos conf.showall)

  | 'f' ->
      conf.underinfo <- not conf.underinfo;
      TEdone ("underinfo " ^ btos conf.underinfo)

  | 'P' ->
      conf.savebmarks <- not conf.savebmarks;
      TEdone ("persistent bookmarks " ^ btos conf.savebmarks)

  | 'S' ->
      let ondone s =
        try
          let pageno, py =
            match state.layout with
            | [] -> 0, 0
            | l :: _ ->
                l.pageno, l.pagey
          in
          conf.interpagespace <- int_of_string s;
          state.maxy <- calcheight ();
          let y = getpagey pageno in
          gotoy (y + py)
        with exc ->
          state.text <- Printf.sprintf "bad integer `%s': %s"
            s (Printexc.to_string exc)
      in
      TEswitch ('%', "", None, intentry, ondone)

  | 'l' ->
      reinit conf.angle (not conf.proportional);
      TEdone ("proprortional display " ^ btos conf.proportional)

  | _ ->
      state.text <- Printf.sprintf "bad option %d `%c'" key c;
      TEstop
;;

let maxoutlinerows () = (conf.winh - 31) / 16;;

let enterselector  allowdel outlines errmsg msg =
  if Array.length outlines = 0
  then (
    showtext ' ' errmsg;
  )
  else (
    state.text <- msg;
    Glut.setCursor Glut.CURSOR_INHERIT;
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
      Some (allowdel, active,
           max 0 ((active - maxoutlinerows () / 2)), outlines, "");
    Glut.postRedisplay ();
  )
;;

let enteroutlinemode () =
  let outlines, msg =
    match state.outlines with
    | Oarray a -> a, ""
    | Olist l ->
        let a = Array.of_list (List.rev l) in
        state.outlines <- Oarray a;
        a, ""
    | Onarrow (pat, a, b) ->
        a, "Outline was narrowed to `" ^ pat ^ "' (Ctrl-u to restore)"
  in
  enterselector false outlines "Document has no outline" msg;
;;

let enterbookmarkmode () =
  let bookmarks = Array.of_list state.bookmarks in
  enterselector true bookmarks "Document has no bookmarks (yet)" "";
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
            Printf.sprintf "Quick (page %d) (bookmarked at %d/%d/%d %d:%d)"
              (l.pageno+1)
              tm.Unix.tm_mday
              tm.Unix.tm_mon
              (tm.Unix.tm_year + 1900)
              tm.Unix.tm_hour
              tm.Unix.tm_min
        | Some title -> title
      in
      state.bookmarks <-
        (title, 0, l.pageno, float l.pagey /. float l.pageh) :: state.bookmarks
;;

let doreshape w h =
  state.fullscreen <- None;
  Glut.reshapeWindow w h;
;;

let writeopen path password  =
  writecmd state.csock ("open " ^ path ^ "\000" ^ state.password ^ "\000");
;;

let opendoc path password =
  invalidate ();
  state.path <- path;
  state.password <- password;
  state.gen <- state.gen + 1;

  writeopen path password;
  Glut.setWindowTitle ("llpp " ^ Filename.basename path);
  wcmd "geometry" [`i state.w; `i conf.winh];
;;

let birdseyeoff (c, leftx) =
  state.birdseye <- None;
  conf.zoom <- c.zoom;
  conf.presentation <- c.presentation;
  conf.interpagespace <- c.interpagespace;
  conf.showall <- c.showall;
  conf.hlinks <- c.hlinks;
  state.x <- leftx;
  state.text <- Printf.sprintf "birds eye mode off (zoom %3.1f%%)"
    (100.0*.conf.zoom);
;;

let viewkeyboard ~key ~x ~y =
  let enttext te =
    state.textentry <- te;
    state.text <- "";
    enttext ();
    Glut.postRedisplay ()
  in
  match state.textentry with
  | None ->
      let c = Char.chr key in
      begin match c with
      | '\027' | 'q' ->
          exit 0

      | '\008' ->
          let y = getnav () in
          gotoy_and_clear_text y

      | '\013' ->
          begin match state.birdseye with
          | None -> ()
          | Some vals ->
              let y = getpagey state.birdseyepageno in
              state.y <- y;
              birdseyeoff vals;
              reshape conf.winw conf.winh;
          end;

      | 'o' ->
          enteroutlinemode ()

      | 'u' ->
          state.rects <- [];
          state.text <- "";
          Glut.postRedisplay ()

      | '/' | '?' ->
          let ondone isforw s =
            cbput state.hists.pat s;
            state.searchpattern <- s;
            search s isforw
          in
          enttext (Some (c, "", Some (onhist state.hists.pat),
                        textentry, ondone (c ='/')))

      | '+' when Glut.getModifiers () land Glut.active_ctrl != 0 ->
          let incr = if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01 in
          conf.zoom <- min 2.2 (conf.zoom +. incr);
          state.text <- Printf.sprintf "zoom is %3.1f%%" (100.0*.conf.zoom);
          reshape conf.winw conf.winh

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
          enttext (Some ('+', "", None, intentry, ondone))

      | '-' when Glut.getModifiers () land Glut.active_ctrl != 0 ->
          let decr = if conf.zoom -. 0.1 < 0.1 then 0.01 else 0.1 in
          conf.zoom <- max 0.01 (conf.zoom -. decr);
          if conf.zoom <= 1.0 then state.x <- 0;
          state.text <- Printf.sprintf "zoom is %3.1f%%" (100.0*.conf.zoom);
          reshape conf.winw conf.winh;

      | '-' ->
          let ondone msg =
            state.text <- msg;
          in
          enttext (Some ('-', "", None, optentry, ondone))

      | '0' when (Glut.getModifiers () land Glut.active_ctrl != 0) ->
          state.x <- 0;
          conf.zoom <- 1.0;
          state.text <- "zoom is 100%";
          reshape conf.winw conf.winh

      | '1' when (Glut.getModifiers () land Glut.active_ctrl != 0) ->
          let zoom = zoomforh conf.winw conf.winh conf.scrollw in
          if zoom < 1.0
          then (
            conf.zoom <- zoom;
            state.x <- 0;
            state.text <- Printf.sprintf "zoom is %3.1f%%" (100.0*.conf.zoom);
            reshape conf.winw conf.winh;
          )

      | '9' when (Glut.getModifiers () land Glut.active_ctrl != 0) ->
          begin match state.birdseye with
          | None ->
              let zoom = 76.0 /. float state.w in
              state.birdseye <- Some ({ conf with zoom = conf.zoom }, state.x);
              conf.zoom <- zoom;
              conf.presentation <- false;
              conf.interpagespace <- 10;
              conf.hlinks <- false;
              state.x <- 0;
              state.mstate <- Mnone;
              conf.showall <- false;
              Glut.setCursor Glut.CURSOR_INHERIT;
              state.text <- Printf.sprintf "birds eye mode on (zoom %3.1f%%)"
                (100.0*.zoom)

          | Some vals ->
              birdseyeoff vals;
          end;
          reshape conf.winw conf.winh

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
              cbput state.hists.pag (string_of_int n);
              gotoy_and_clear_text (getpagey (n + conf.pagebias - 1))
            )
          in
          let pageentry text key =
            match Char.unsafe_chr key with
            | 'g' -> TEdone text
            | _ -> intentry text key
          in
          let text = "x" in text.[0] <- c;
          enttext (Some (':', text, Some (onhist state.hists.pag),
                        pageentry, ondone))

      | 'b' ->
          conf.scrollw <- if conf.scrollw > 0 then 0 else defconf.scrollw;
          reshape conf.winw conf.winh;

      | 'l' ->
          conf.hlinks <- not conf.hlinks;
          state.text <- "highlightlinks " ^ if conf.hlinks then "on" else "off";
          Glut.postRedisplay ()

      | 'a' ->
          conf.autoscroll <- not conf.autoscroll

      | 'P' ->
          conf.presentation <- not conf.presentation;
          showtext ' ' ("presentation mode " ^
                           if conf.presentation then "on" else "off");
          represent ()

      | 'f' ->
          begin match state.fullscreen with
          | None ->
              state.fullscreen <- Some (conf.winw, conf.winh);
              Glut.fullScreen ()
          | Some (w, h) ->
              state.fullscreen <- None;
              doreshape w h
          end

      | 'g' ->
          gotoy_and_clear_text 0

      | 'n' ->
          search state.searchpattern true

      | 'p' | 'N' ->
          search state.searchpattern false

      | 't' ->
          begin match state.layout with
          | [] -> ()
          | l :: _ ->
              gotoy_and_clear_text (getpagey l.pageno)
          end

      | ' ' ->
          begin match List.rev state.layout with
          | [] -> ()
          | l :: _ ->
              let pageno = min (l.pageno+1) (state.pagecount-1) in
              gotoy_and_clear_text (getpagey pageno)
          end

      | '\127' ->
          begin match state.layout with
          | [] -> ()
          | l :: _ ->
              let pageno = max 0 (l.pageno-1) in
              gotoy_and_clear_text (getpagey pageno)
          end

      | '=' ->
          let f (fn, ln) l =
            if fn = -1 then l.pageno, l.pageno else fn, l.pageno
          in
          let fn, ln = List.fold_left f (-1, -1) state.layout in
          let s =
            let maxy = state.maxy - (if conf.maxhfit then conf.winh else 0) in
            let percent =
              if maxy <= 0
              then 100.
              else (100. *. (float state.y /. float maxy)) in
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

      | 'w' ->
          begin match state.layout with
          | [] -> ()
          | l :: _ ->
              doreshape (l.pagew + conf.scrollw) l.pageh;
              Glut.postRedisplay ();
          end

      | '\'' ->
          enterbookmarkmode ()

      | 'm' ->
          let ondone s =
            match state.layout with
            | l :: _ ->
                state.bookmarks <-
                  (s, 0, l.pageno, float l.pagey /. float l.pageh)
                :: state.bookmarks
            | _ -> ()
          in
          enttext (Some ('~', "", None, textentry, ondone))

      | '~' ->
          quickbookmark ();
          showtext ' ' "Quick bookmark added";

      | 'z' ->
          begin match state.layout with
          | l :: _ ->
              let rect = getpdimrect l.pagedimno in
              let w, h =
                if conf.crophack
                then
                  (truncate (1.8 *. (rect.(1) -. rect.(0))),
                  truncate (1.2 *. (rect.(3) -. rect.(0))))
                else
                  (truncate (rect.(1) -. rect.(0)),
                  truncate (rect.(3) -. rect.(0)))
              in
              if w != 0 && h != 0
              then
                doreshape (w + conf.scrollw) (h + conf.interpagespace)
              ;
              Glut.postRedisplay ();

          | [] -> ()
          end

      | '<' | '>' ->
          reinit (conf.angle + (if c = '>' then 30 else -30)) conf.proportional

      | '[' | ']' ->
          state.colorscale <-
            max 0.0
            (min (state.colorscale +. (if c = ']' then 0.1 else -0.1)) 1.0);
          Glut.postRedisplay ()

      | 'k' -> gotoy (clamp (-conf.scrollincr))
      | 'j' -> gotoy (clamp conf.scrollincr)

      | 'r' -> opendoc state.path state.password

      | _ ->
          vlog "huh? %d %c" key (Char.chr key);
      end

  | Some (c, text, opthist, onkey, ondone) when key = 8 ->
      let len = String.length text in
      if len = 0
      then (
        state.textentry <- None;
        Glut.postRedisplay ();
      )
      else (
        let s = String.sub text 0 (len - 1) in
        enttext (Some (c, s, opthist, onkey, ondone))
      )

  | Some (c, text, onhist, onkey, ondone) ->
      begin match Char.unsafe_chr key with
      | '\r' | '\n' ->
          ondone text;
          state.textentry <- None;
          Glut.postRedisplay ()

      | '\027' ->
          begin match onhist with
          | None -> ()
          | Some (_, onhistcancel) -> onhistcancel ()
          end;
          state.textentry <- None;
          Glut.postRedisplay ()

      | _ ->
          begin match onkey text key with
          | TEdone text ->
              state.textentry <- None;
              ondone text;
              Glut.postRedisplay ()

          | TEcont text ->
              enttext (Some (c, text, onhist, onkey, ondone));

          | TEstop ->
              state.textentry <- None;
              Glut.postRedisplay ()

          | TEswitch te ->
              state.textentry <- Some te;
              Glut.postRedisplay ()
          end;
      end;
;;

let narrow outlines pattern =
  let reopt = try Some (Str.regexp_case_fold pattern) with _ -> None in
  match reopt with
  | None -> None
  | Some re ->
      let rec fold accu n =
        if n = -1
        then accu
        else
          let (s, _, _, _) as o = outlines.(n) in
          let accu =
            if (try ignore (Str.search_forward re s 0); true
              with Not_found -> false)
            then (o :: accu)
            else accu
          in
          fold accu (n-1)
      in
      let matched = fold [] (Array.length outlines - 1) in
      if matched = [] then None else Some (Array.of_list matched)
;;

let outlinekeyboard ~key ~x ~y (allowdel, active, first, outlines, qsearch) =
  let search active pattern incr =
    let dosearch re =
      let rec loop n =
        if n = Array.length outlines || n = -1
        then None
        else
          let (s, _, _, _) = outlines.(n) in
          if
            (try ignore (Str.search_forward re s 0); true
              with Not_found -> false)
          then Some n
          else loop (n + incr)
      in
      loop active
    in
    try
      let re = Str.regexp_case_fold pattern in
      dosearch re
    with Failure s ->
      state.text <- s;
      None
  in
  let firstof active = max 0 (active - maxoutlinerows () / 2) in
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
        | None ->
            state.text <- qsearch ^ " [not found]";
            active, first
        | Some active ->
            state.text <- qsearch;
            active, firstof active
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
          let active, first =
            match search active qsearch ~-1 with
            | None ->
                state.text <- qsearch ^ " [not found]";
                active, first
            | Some active ->
                state.text <- qsearch;
                active, firstof active
          in
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
      let active, first =
        match search active pattern 1 with
        | None ->
            state.text <- pattern ^ " [not found]";
            active, first
        | Some active ->
            state.text <- pattern;
            active, firstof active
      in
      state.outline <- Some (allowdel, active, first, outlines, pattern);
      Glut.postRedisplay ()

  | 14 when not allowdel ->             (* ctrl-n *)
      if String.length qsearch > 0
      then (
        let optoutlines = narrow outlines qsearch in
        begin match optoutlines with
        | None -> state.text <- "can't narrow"
        | Some outlines ->
            state.outline <- Some (allowdel, 0, 0, outlines, qsearch);
            match state.outlines with
            | Olist l -> ()
            | Oarray a ->
                state.outlines <- Onarrow (qsearch, outlines, a)
            | Onarrow (pat, a, b) ->
                state.outlines <- Onarrow (qsearch, outlines, b)
        end;
      );
      Glut.postRedisplay ()

  | 21 when not allowdel ->             (* ctrl-u *)
      let outline =
        match state.outlines with
        | Oarray a -> a
        | Olist l ->
            let a = Array.of_list (List.rev l) in
            state.outlines <- Oarray a;
            a
        | Onarrow (pat, a, b) ->
            state.outlines <- Oarray b;
            state.text <- "";
            b
      in
      state.outline <- Some (allowdel, 0, 0, outline, qsearch);
      Glut.postRedisplay ()

  | 12 ->
      state.outline <-
        Some (allowdel, active, firstof active, outlines, qsearch);
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
  if key = 7
  then
    wcmd "interrupt" []
  else
    match state.outline with
    | None -> viewkeyboard ~key ~x ~y
    | Some outline -> outlinekeyboard ~key ~x ~y outline
;;

let special ~key ~x ~y =
  match state.outline with
  | None when state.birdseye <> None ->
      begin match key with
      | Glut.KEY_UP ->
          let pageno = max 0 (state.birdseyepageno - 1) in
          state.birdseyepageno <- pageno;
          if not (pagevisible state.layout pageno)
          then gotopage pageno 0.0
          else Glut.postRedisplay ();

      | Glut.KEY_DOWN ->
          let pageno = min (state.pagecount - 1) (state.birdseyepageno + 1) in
          state.birdseyepageno <- pageno;
          if not (pagevisible state.layout pageno)
          then
            begin match List.rev state.layout with
            | [] -> gotopage pageno 0.0
            | l :: _ ->
                gotoy (state.y + conf.interpagespace + l.pageh*2 - l.pagevh)
            end
          else Glut.postRedisplay ();

      | Glut.KEY_PAGE_UP ->
          begin match state.layout with
          | l :: _ ->
              if l.pageno = state.birdseyepageno
              then (
                match layout (state.y - conf.winh) conf.winh with
                | [] -> gotoy (clamp (-conf.winh))
                | l :: _ -> 
                    state.birdseyepageno <- max 0 (l.pageno - 1);
                    gotopage state.birdseyepageno 0.0
              )
              else (
                state.birdseyepageno <- max 0 (l.pageno - 1);
                gotopage state.birdseyepageno 0.0
              )
          | [] -> gotoy (clamp (-conf.winh))
          end;
      | Glut.KEY_PAGE_DOWN ->
          begin match List.rev state.layout with
          | l :: _ ->
              state.birdseyepageno <- min (state.pagecount - 1) (l.pageno + 1);
              gotoy (clamp (l.pagedispy + conf.interpagespace + l.pageh))
          | [] -> gotoy (clamp conf.winh)
          end;

      | Glut.KEY_HOME ->
          state.birdseyepageno <- 0;
          gotopage 0 0.0
      | Glut.KEY_END ->
          state.birdseyepageno <- state.pagecount - 1;
          if not (pagevisible state.layout state.birdseyepageno)
          then
            gotopage state.birdseyepageno 0.0
          else
            Glut.postRedisplay ()
          ;
      | _ -> ()
      end

  | None ->
      begin match state.textentry with
      | None ->
          let y =
            match key with
            | Glut.KEY_F3        -> search state.searchpattern true; state.y
            | Glut.KEY_UP        -> clamp (-conf.scrollincr)
            | Glut.KEY_DOWN      -> clamp conf.scrollincr
            | Glut.KEY_PAGE_UP   ->
                if Glut.getModifiers () land Glut.active_ctrl != 0
                then
                  match state.layout with
                  | [] -> state.y
                  | l :: _ -> state.y - l.pagey
                else
                  clamp (-conf.winh)
            | Glut.KEY_PAGE_DOWN ->
                if Glut.getModifiers () land Glut.active_ctrl != 0
                then
                  match List.rev state.layout with
                  | [] -> state.y
                  | l :: _ -> getpagey l.pageno
                else
                  clamp conf.winh
            | Glut.KEY_HOME -> addnav (); 0
            | Glut.KEY_END ->
                addnav ();
                state.maxy - (if conf.maxhfit then conf.winh else 0)

            | Glut.KEY_RIGHT when conf.zoom > 1.0 ->
                state.x <- state.x - 10;
                state.y
            | Glut.KEY_LEFT when conf.zoom > 1.0  ->
                state.x <- state.x + 10;
                state.y

            | _ -> state.y
          in
          gotoy_and_clear_text y

      | Some (c, s, (Some (action, _) as onhist), onkey, ondone) ->
          let s =
            match key with
            | Glut.KEY_UP    -> action HCprev
            | Glut.KEY_DOWN  -> action HCnext
            | Glut.KEY_HOME  -> action HCfirst
            | Glut.KEY_END   -> action HClast
            | _ -> state.text
          in
          state.textentry <- Some (c, s, onhist, onkey, ondone);
          Glut.postRedisplay ()

      | _ -> ()
      end

  | Some (allowdel, active, first, outlines, qsearch) ->
      let maxrows = maxoutlinerows () in
      let calcfirst first active =
        if active > first
        then
          let rows = active - first in
          if rows > maxrows then active - maxrows else first
        else active
      in
      let navigate incr =
        let active = active + incr in
        let active = max 0 (min active (Array.length outlines - 1)) in
        let first = calcfirst first active in
        state.outline <- Some (allowdel, active, first, outlines, qsearch);
        Glut.postRedisplay ()
      in
      let updownlevel incr =
        let len = Array.length outlines in
        let (_, curlevel, _, _) = outlines.(active) in
        let rec flow i =
          if i = len then i-1 else if i = -1 then 0 else
              let (_, l, _, _) = outlines.(i) in
              if l != curlevel then i else flow (i+incr)
        in
        let active = flow active in
        let first = calcfirst first active in
        state.outline <- Some (allowdel, active, first, outlines, qsearch);
        Glut.postRedisplay ()
      in
      match key with
      | Glut.KEY_UP        -> navigate ~-1
      | Glut.KEY_DOWN      -> navigate   1
      | Glut.KEY_PAGE_UP   -> navigate ~-maxrows
      | Glut.KEY_PAGE_DOWN -> navigate   maxrows

      | Glut.KEY_RIGHT when not allowdel -> updownlevel 1
      | Glut.KEY_LEFT when not allowdel -> updownlevel ~-1

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
  let margin = state.x + (conf.winw - (state.w + conf.scrollw)) / 2 in
  GlDraw.color (scalecolor 1.0);
  GlDraw.rect
    (float l.pagex, float l.pagedispy)
    (float (l.pagew + l.pagex), float (l.pagedispy + l.pagevh))
  ;
  let x = float (if margin < 0 then -margin else 0)
  and y = float (l.pagedispy + 13) in
  let font = Glut.BITMAP_8_BY_13 in
  GlDraw.color (0.0, 0.0, 0.0);
  GlPix.raster_pos ~x ~y ();
  String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c))
    ("Loading " ^ string_of_int (l.pageno + 1));
;;

let now () = Unix.gettimeofday ();;

let drawpage l =
  begin match getopaque l.pageno with
  | Some (opaque, _) when validopaque opaque ->
      if state.textentry = None
      then GlDraw.color (scalecolor 1.0)
      else GlDraw.color (scalecolor 0.4);
      let a = now () in
      draw (l.pagedispy, l.pagew, l.pagevh, l.pagey, conf.hlinks)
        opaque;
      let b = now () in
      let d = b-.a in
      vlog "draw %d %f sec" l.pageno d;

  | _ ->
      drawplaceholder l;
  end;
  if state.birdseye <> None && state.birdseyepageno = l.pageno
  then (
    GlDraw.polygon_mode `both `line;
    GlDraw.line_width 4.0;
    GlDraw.color (0.8, 0.0, 0.0);
    GlDraw.rect
      (float (l.pagex - 1), float (l.pagedispy - 1))
      (float (l.pagew + l.pagex + 1), float (l.pagedispy + l.pagevh + 1))
    ;
    GlDraw.line_width 1.0;
    GlDraw.polygon_mode `both `fill;
  );
;;

let scrollph y =
  let maxy = state.maxy - (if conf.maxhfit then conf.winh else 0) in
  let sh = (float (maxy + conf.winh) /. float conf.winh)  in
  let sh = float conf.winh /. sh in
  let sh = max sh (float conf.scrollh) in

  let percent =
    if state.y = state.maxy
    then 1.0
    else float y /. float maxy
  in
  let position = (float conf.winh -. sh) *. percent in

  let position =
    if position +. sh > float conf.winh
    then float conf.winh -. sh
    else position
  in
  position, sh;
;;

let scrollindicator () =
  GlDraw.color (0.64 , 0.64, 0.64);
  GlDraw.rect
    (float (conf.winw - conf.scrollw), 0.)
    (float conf.winw, float conf.winh)
  ;
  GlDraw.color (0.0, 0.0, 0.0);

  let position, sh = scrollph state.y in
  GlDraw.rect
    (float (conf.winw - conf.scrollw), position)
    (float conf.winw, position +. sh)
  ;
;;

let showsel margin =
  match state.mstate with
  | Mnone | Mscroll _ | Mpan _ ->
      ()

  | Msel ((x0, y0), (x1, y1)) ->
      let rec loop = function
        | l :: ls ->
            if (y0 >= l.pagedispy && y0 <= (l.pagedispy + l.pagevh))
              || ((y1 >= l.pagedispy && y1 <= (l.pagedispy + l.pagevh)))
            then
              match getopaque l.pageno with
              | Some (opaque, _) when validopaque opaque ->
                  let oy = -l.pagey + l.pagedispy in
                  seltext opaque
                    (x0 - margin - state.x, y0,
                    x1 - margin - state.x, y1) oy;
                  ()
              | _ -> ()
            else loop ls
        | [] -> ()
      in
      loop state.layout
;;

let showrects () =
  let panx = float state.x in
  Gl.enable `blend;
  GlDraw.color (0.0, 0.0, 1.0) ~alpha:0.5;
  GlFunc.blend_func `src_alpha `one_minus_src_alpha;
  List.iter
    (fun (pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)) ->
      List.iter (fun l ->
        if l.pageno = pageno
        then (
          let d = float (l.pagedispy - l.pagey) in
          GlDraw.color (0.0, 0.0, 1.0 /. float c) ~alpha:0.5;
          GlDraw.begins `quads;
          (
            GlDraw.vertex2 (x0+.panx, y0+.d);
            GlDraw.vertex2 (x1+.panx, y1+.d);
            GlDraw.vertex2 (x2+.panx, y2+.d);
            GlDraw.vertex2 (x3+.panx, y3+.d);
          );
          GlDraw.ends ();
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
      GlDraw.rect (0., 0.) (float conf.winw, float conf.winh);
      Gl.disable `blend;

      GlDraw.color (1., 1., 1.);
      let font = Glut.BITMAP_9_BY_15 in
      let draw_string x y s =
        GlPix.raster_pos ~x ~y ();
        String.iter (fun c -> Glut.bitmapCharacter ~font ~c:(Char.code c)) s
      in
      let rec loop row =
        if row = Array.length outlines || (row - first) * 16 > conf.winh
        then ()
        else (
          let (s, l, _, _) = outlines.(row) in
          let y = (row - first) * 16 in
          let x = 5 + 15*l in
          if row = active
          then (
            Gl.enable `blend;
            GlDraw.polygon_mode `both `line;
            GlFunc.blend_func `src_alpha `one_minus_src_alpha;
            GlDraw.color (1., 1., 1.) ~alpha:0.9;
            GlDraw.rect (0., float (y + 1))
              (float (conf.winw - 1), float (y + 18));
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
  let margin = (conf.winw - (state.w + conf.scrollw)) / 2 in
  GlDraw.viewport margin 0 state.w conf.winh;
  pagematrix ();
  if state.birdseye <> None
  then
    GlClear.color (0.5, 0.5, 0.55)
  else
    GlClear.color (scalecolor 0.5)
  ;
  GlClear.clear [`color];
  if state.x != 0
  then (
    let x = float state.x in
    GlMat.translate ~x ();
  );
  if conf.zoom > 1.0
  then (
    Gl.enable `scissor_test;
    GlMisc.scissor 0 0 (conf.winw - conf.scrollw) conf.winh;
  );
  List.iter drawpage state.layout;
  if conf.zoom > 1.0
  then
    Gl.disable `scissor_test
  ;
  if state.x != 0
  then (
    let x = -.float state.x in
    GlMat.translate ~x ();
  );
  showrects ();
  showsel margin;
  GlDraw.viewport 0 0 conf.winw conf.winh;
  winmatrix ();
  scrollindicator ();
  showoutline state.outline;
  enttext ();
  Glut.swapBuffers ();
;;

let getunder x y =
  let margin = (conf.winw - (state.w + conf.scrollw)) / 2 in
  let x = x - margin - state.x in
  let rec f = function
    | l :: rest ->
        begin match getopaque l.pageno with
        | Some (opaque, _) when validopaque opaque ->
            let y = y - l.pagedispy in
            if y > 0
            then
              let y = l.pagey + y in
              let x = x - l.pagex in
              match whatsunder opaque x y with
              | Unone -> f rest
              | under -> under
            else
              f rest
        | _ ->
            f rest
        end
    | [] -> Unone
  in
  f state.layout
;;

let mouse ~button ~bstate ~x ~y =
  match button with
  | Glut.OTHER_BUTTON n when (n == 3 || n == 4) && bstate = Glut.UP ->
      let incr =
        if n = 3
        then
          -conf.scrollincr
        else
          conf.scrollincr
      in
      let incr = incr * 2 in
      let y = clamp incr in
      gotoy_and_clear_text y

  | Glut.LEFT_BUTTON when state.outline = None
      && Glut.getModifiers () land Glut.active_ctrl != 0 ->
      if bstate = Glut.DOWN
      then (
        Glut.setCursor Glut.CURSOR_CROSSHAIR;
        state.mstate <- Mpan (x, y)
      )
      else
        state.mstate <- Mnone

  | Glut.LEFT_BUTTON
      when state.outline = None && x > conf.winw - conf.scrollw ->
      if bstate = Glut.DOWN
      then
        let position, sh = scrollph state.y in
        if y > truncate position && y < truncate (position +. sh)
        then
          state.mstate <- Mscroll
        else
          let percent = float y /. float conf.winh in
          let desty = truncate (float (state.maxy - conf.winh) *. percent) in
          gotoy desty;
          state.mstate <- Mscroll
      else
        state.mstate <- Mnone

  | Glut.LEFT_BUTTON when state.outline = None && state.birdseye <> None ->
      begin match state.birdseye with
      | Some vals ->
          let margin = (conf.winw - (state.w + conf.scrollw)) / 2 in
          let rec loop = function
            | [] -> ()
            | l :: rest ->
                if y > l.pagedispy && y < l.pagedispy + l.pagevh
                  && x > margin && x < margin + l.pagew
                then (
                  let y = getpagey l.pageno in
                  state.y <- y;
                  birdseyeoff vals;
                  reshape conf.winw conf.winh;
                )
                else loop rest
          in
          loop state.layout;
      | None -> ()                      (* impossible *)
      end

  | Glut.LEFT_BUTTON when state.outline = None ->
      let dest = if bstate = Glut.DOWN then getunder x y else Unone in
      begin match dest with
      | Ulinkgoto (pageno, top) ->
          if pageno >= 0
          then
            gotopage1 pageno top

      | Ulinkuri s ->
          print_endline s

      | Unone when bstate = Glut.DOWN ->
          Glut.setCursor Glut.CURSOR_CROSSHAIR;
          state.mstate <- Mpan (x, y);

      | Unone | Utext _ ->
          if bstate = Glut.DOWN
          then (
            if conf.angle mod 360 = 0
            then (
              state.mstate <- Msel ((x, y), (x, y));
              Glut.postRedisplay ()
            )
          )
          else (
            match state.mstate with
            | Mnone  -> ()

            | Mscroll ->
                state.mstate <- Mnone

            | Mpan _ ->
                Glut.setCursor Glut.CURSOR_INHERIT;
                state.mstate <- Mnone

            | Msel ((x0, y0), (x1, y1)) ->
                let f l =
                  if (y0 >= l.pagedispy && y0 <= (l.pagedispy + l.pagevh))
                    || ((y1 >= l.pagedispy && y1 <= (l.pagedispy + l.pagevh)))
                  then
                    match getopaque l.pageno with
                    | Some (opaque, _) when validopaque opaque ->
                        copysel opaque
                    | _ -> ()
                in
                List.iter f state.layout;
                copysel "";             (* ugly *)
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

    | Mpan (x0, y0) ->
        let dx = x - x0
        and dy = y0 - y in
        state.mstate <- Mpan (x, y);
        if conf.zoom > 1.0 then state.x <- state.x + dx;
        let y = clamp dy in
        gotoy_and_clear_text y

    | Msel (a, _) ->
        state.mstate <- Msel (a, (x, y));
        Glut.postRedisplay ()

    | Mscroll ->
        let y = min conf.winh (max 0 y) in
        let percent = float y /. float conf.winh in
        let y = truncate (float (state.maxy - conf.winh) *. percent) in
        gotoy_and_clear_text y
;;

let pmotion ~x ~y =
  if state.outline = None && state.birdseye = None
  then
    match state.mstate with
    | Mnone ->
        begin match getunder x y with
        | Unone -> Glut.setCursor Glut.CURSOR_INHERIT
        | Ulinkuri uri ->
            if conf.underinfo then showtext 'u' ("ri: " ^ uri);
            Glut.setCursor Glut.CURSOR_INFO
        | Ulinkgoto (page, y) ->
            if conf.underinfo then showtext 'p' ("age: " ^ string_of_int page);
            Glut.setCursor Glut.CURSOR_INFO
        | Utext s ->
            if conf.underinfo then showtext 'f' ("ont: " ^ s);
            Glut.setCursor Glut.CURSOR_TEXT
        end

    | Mpan _ | Msel _ | Mscroll ->
        ()
;;

module State =
struct
  open Parser

  let home =
    try
      match Sys.os_type with
      | "Win32" -> Sys.getenv "HOMEPATH"
      | _ -> Sys.getenv "HOME"
    with exn ->
      prerr_endline
        ("Can not determine home directory location: " ^
            Printexc.to_string exn);
      ""
  ;;

  let config_of c attrs =
    let apply c k v =
      try
        match k with
        | "scroll-bar-width" -> { c with scrollw = max 0 (int_of_string v) }
        | "scroll-handle-height" -> { c with scrollh = max 0 (int_of_string v) }
        | "case-insensitive-search" -> { c with icase = bool_of_string v }
        | "preload" -> { c with preload = bool_of_string v }
        | "page-bias" -> { c with pagebias = int_of_string v }
        | "scroll-step" -> { c with scrollincr = max 1 (int_of_string v) }
        | "max-height-fit" -> { c with maxhfit = bool_of_string v }
        | "crop-hack" -> { c with crophack = bool_of_string v }
        | "throttle" -> { c with showall = bool_of_string v }
        | "highlight-links" -> { c with hlinks = bool_of_string v }
        | "under-cursor-info" -> { c with underinfo = bool_of_string v }
        | "vertical-margin" -> { c with interpagespace = max 0 (int_of_string v) }
        | "zoom" ->
            let zoom = float_of_string v /. 100. in
            let zoom = max 0.01 (min 2.2 zoom) in
            { c with zoom = zoom }
        | "presentation" -> { c with presentation = bool_of_string v }
        | "rotation-angle" -> { c with angle = int_of_string v }
        | "width" -> { c with winw = max 20 (int_of_string v) }
        | "height" -> { c with winh = max 20 (int_of_string v) }
        | "persistent-bookmarks" -> { c with savebmarks = bool_of_string v }
        | "proportional-display" -> { c with proportional = bool_of_string v }
        | "pixmap-cache-size" -> { c with memlimit = max 2 (int_of_string v) }
        | "tex-count" -> { c with texcount = max 1 (int_of_string v) }
        | "slice-height" -> { c with sliceheight = max 2 (int_of_string v) }
        | _ -> c
      with exn ->
        prerr_endline ("Error processing attribute (`" ^
                          k ^ "'=`" ^ v ^ "'): " ^ Printexc.to_string exn);
        c
    in
    let rec fold c = function
      | [] -> c
      | (k, v) :: rest ->
          let c = apply c k v in
          fold c rest
    in
    fold c attrs;
  ;;

  let bookmark_of attrs =
    let rec fold title page rely = function
      | ("title", v) :: rest -> fold v page rely rest
      | ("page", v) :: rest -> fold title v rely rest
      | ("rely", v) :: rest -> fold title page v rest
      | _ :: rest -> fold title page rely rest
      | [] -> title, page, rely
    in
    fold "invalid" "0" "0" attrs
  ;;

  let setconf dst src =
    dst.scrollw        <- src.scrollw;
    dst.scrollh        <- src.scrollh;
    dst.icase          <- src.icase;
    dst.preload        <- src.preload;
    dst.pagebias       <- src.pagebias;
    dst.verbose        <- src.verbose;
    dst.scrollincr     <- src.scrollincr;
    dst.maxhfit        <- src.maxhfit;
    dst.crophack       <- src.crophack;
    dst.autoscroll     <- src.autoscroll;
    dst.showall        <- src.showall;
    dst.hlinks         <- src.hlinks;
    dst.underinfo      <- src.underinfo;
    dst.interpagespace <- src.interpagespace;
    dst.zoom           <- src.zoom;
    dst.presentation   <- src.presentation;
    dst.angle          <- src.angle;
    dst.winw           <- src.winw;
    dst.winh           <- src.winh;
    dst.savebmarks     <- src.savebmarks;
    dst.memlimit       <- src.memlimit;
    dst.proportional   <- src.proportional;
    dst.texcount       <- src.texcount;
    dst.sliceheight    <- src.sliceheight;
  ;;

  let unent s =
    let l = String.length s in
    let b = Buffer.create l in
    unent b s 0 l;
    Buffer.contents b;
  ;;

  let get s =
    let h = Hashtbl.create 10 in
    let dc = { defconf with angle = defconf.angle } in
    let rec toplevel v t spos epos =
      match t with
      | Vdata | Vcdata | Vend -> v
      | Vopen ("llppconfig", attrs, closed) ->
          if closed
          then v
          else { v with f = llppconfig }
      | Vopen _ ->
          error "unexpected subelement at top level" s spos
      | Vclose tag -> error "unexpected close at top level" s spos

    and llppconfig v t spos epos =
      match t with
      | Vdata | Vcdata | Vend -> v
      | Vopen ("defaults", attrs, closed) ->
          let c = config_of dc attrs in
          setconf dc c;
          if closed
          then v
          else { v with f = skip "defaults" (fun () -> v) }

      | Vopen ("doc", attrs, closed) ->
          let pathent =
            try
              List.assoc "path" attrs
            with Not_found -> error "doc is missing path attribute" s spos
          in
          let path = unent pathent in
          let c = config_of dc attrs in
          let y =
            try
              float_of_string (List.assoc "rely" attrs)
            with
            | Not_found -> 0.0
            | exn ->
                dolog "error while accesing rely: %s" (Printexc.to_string exn);
                0.0
          in
          let x =
            try
              int_of_string (List.assoc "pan" attrs)
            with
            | Not_found -> 0
            | exn ->
                dolog "error while accesing rely: %s" (Printexc.to_string exn);
                0
          in
          if closed
          then (Hashtbl.add h path (c, [], x, y); v)
          else { v with f = doc path x y c [] }

      | Vopen (tag, _, closed) ->
          error "unexpected subelement in llppconfig" s spos

      | Vclose "llppconfig" ->  { v with f = toplevel }
      | Vclose tag -> error "unexpected close in llppconfig" s spos

    and doc path x y c bookmarks v t spos epos =
      match t with
      | Vdata | Vcdata -> v
      | Vend -> error "unexpected end of input in doc" s spos
      | Vopen ("bookmarks", attrs, closed) ->
          { v with f = pbookmarks path x y c bookmarks }

      | Vopen (tag, _, _) ->
          error "unexpected subelement in doc" s spos

      | Vclose "doc" ->
          Hashtbl.add h path (c, List.rev bookmarks, x, y);
          { v with f = llppconfig }

      | Vclose tag -> error "unexpected close in doc" s spos

    and pbookmarks path x y c bookmarks v t spos epos =
      match t with
      | Vdata | Vcdata -> v
      | Vend -> error "unexpected end of input in bookmarks" s spos
      | Vopen ("item", attrs, closed) ->
          let titleent, spage, srely = bookmark_of attrs in
          let page =
            try
              int_of_string spage
            with exn ->
              dolog "Failed to convert page %S to integer: %s"
                spage (Printexc.to_string exn);
              0
          in
          let rely =
            try
              float_of_string srely
            with exn ->
              dolog "Failed to convert rely %S to real: %s"
                srely (Printexc.to_string exn);
              0.0
          in
          let bookmarks = (unent titleent, 0, page, rely) :: bookmarks in
          if closed
          then { v with f = pbookmarks path x y c bookmarks }
          else
            let f () = v in
            { v with f = skip "item" f }

      | Vopen _ ->
          error "unexpected subelement in bookmarks" s spos

      | Vclose "bookmarks" ->
          { v with f = doc path x y c bookmarks }

      | Vclose tag -> error "unexpected close in bookmarks" s spos

    and skip tag f v t spos epos =
      match t with
      | Vdata | Vcdata -> v
      | Vend ->
          error ("unexpected end of input in skipped " ^ tag) s spos
      | Vopen (tag', _, closed) ->
          if closed
          then v
          else
            let f' () = { v with f = skip tag f } in
            { v with f = skip tag' f' }
      | Vclose ctag ->
          if tag = ctag
          then f ()
          else error ("unexpected close in skipped " ^ tag) s spos
    in

    parse { f = toplevel; accu = () } s;
    h, dc;
  ;;

  let do_load f ic =
    try
      let len = in_channel_length ic in
      let s = String.create len in
      really_input ic s 0 len;
      f s;
    with
    | Parse_error (msg, s, pos) ->
        let subs = subs s pos in
        let s = Printf.sprintf "%s: at %d [..%s..]" msg pos subs in
        failwith ("parse error: " ^ s)

    | exn ->
        failwith ("config load error: " ^ Printexc.to_string exn)
  ;;

  let path =
    let dir =
      try
        let dir = Filename.concat home ".config" in
        if Sys.is_directory dir then dir else home
      with _ -> home
    in
    Filename.concat dir "llpp.conf"
  ;;

  let load1 f =
    if Sys.file_exists path
    then
      match
        (try Some (open_in_bin path)
          with exn ->
            prerr_endline
              ("Error opening configuation file `" ^ path ^ "': " ^
                  Printexc.to_string exn);
            None
        )
      with
      | Some ic ->
          begin try
              f (do_load get ic)
            with exn ->
              prerr_endline
                ("Error loading configuation from `" ^ path ^ "': " ^
                    Printexc.to_string exn);
          end;
          close_in ic;

      | None -> ()
    else
      f (Hashtbl.create 0, defconf)
  ;;

  let load () =
    let f (h, dc) =
      let pc, pb, px, py =
        try
          Hashtbl.find h state.path
        with Not_found -> dc, [], 0, 0.0
      in
      setconf defconf dc;
      setconf conf pc;
      state.bookmarks <- pb;
      state.x <- px;
      cbput state.hists.nav py;
    in
    load1 f
  ;;

  let add_attrs bb always dc c =
    let ob s a b =
      if always || a != b
      then Printf.bprintf bb "\n    %s='%b'" s a
    and oi s a b =
      if always || a != b
      then Printf.bprintf bb "\n    %s='%d'" s a
    and oz s a b =
      if always || a <> b
      then Printf.bprintf bb "\n    %s='%f'" s (a*.100.)
    in
    let w, h =
      if always
      then dc.winw, dc.winh
      else
        match state.fullscreen with
        | Some wh -> wh
        | None -> c.winw, c.winh
    in
    let zoom, presentation, interpagespace, showall=
      if always
      then dc.zoom, dc.presentation, dc.interpagespace, dc.showall
      else
        match state.birdseye with
        | Some (bc, _) ->
            bc.zoom, bc.presentation, bc.interpagespace, bc.showall
        | None -> c.zoom, c.presentation, c.interpagespace, c.showall
    in
    oi "width" w dc.winw;
    oi "height" h dc.winh;
    oi "scroll-bar-width" c.scrollw dc.scrollw;
    oi "scroll-handle-height" c.scrollh dc.scrollh;
    ob "case-insensitive-search" c.icase dc.icase;
    ob "preload" c.preload dc.preload;
    oi "page-bias" c.pagebias dc.pagebias;
    oi "scroll-step" c.scrollincr dc.scrollincr;
    ob "max-height-fit" c.maxhfit dc.maxhfit;
    ob "crop-hack" c.crophack dc.crophack;
    ob "throttle" showall dc.showall;
    ob "highlight-links" c.hlinks dc.hlinks;
    ob "under-cursor-info" c.underinfo dc.underinfo;
    oi "vertical-margin" interpagespace dc.interpagespace;
    oz "zoom" zoom dc.zoom;
    ob "presentation" presentation dc.presentation;
    oi "rotation-angle" c.angle dc.angle;
    ob "persistent-bookmarks" c.savebmarks dc.savebmarks;
    ob "proportional-display" c.proportional dc.proportional;
    oi "pixmap-cache-size" c.memlimit dc.memlimit;
    oi "texcount" c.texcount dc.texcount;
    oi "slice-height" c.sliceheight dc.sliceheight;
  ;;

  let save () =
    let bb = Buffer.create 32768 in
    let f (h, dc) =
      Buffer.add_string  bb "<llppconfig>\n<defaults ";
      add_attrs bb true dc dc;
      Buffer.add_string bb "/>\n";

      let adddoc path x y c bookmarks =
        if bookmarks == [] && c = dc && y = 0.0
        then ()
        else (
          Printf.bprintf bb "<doc path='%s'"
            (enent path 0 (String.length path));

          if y <> 0.0
          then Printf.bprintf bb " rely='%f'" y;

          if x != 0
          then Printf.bprintf bb " pan='%d'" x;

          add_attrs bb false dc c;

          begin match bookmarks with
          | [] -> Buffer.add_string bb "/>\n"
          | _ ->
              Buffer.add_string bb ">\n<bookmarks>\n";
              List.iter (fun (title, _level, page, rely) ->
                Printf.bprintf bb
                  "<item title='%s' page='%d' rely='%f'/>\n"
                  (enent title 0 (String.length title))
                  page
                  rely
              ) bookmarks;
              Buffer.add_string bb "</bookmarks>\n</doc>\n";
          end;
        )
      in

      let x =
        match state.birdseye with
        | Some (_, x) -> x
        | None -> state.x
      in
      adddoc state.path x (yratio state.y) conf
        (if conf.savebmarks then state.bookmarks else []);

      Hashtbl.iter (fun path (c, bookmarks, x, y) ->
        if path <> state.path
        then
          adddoc path x y c bookmarks
      ) h;
      Buffer.add_string bb "</llppconfig>";
    in
    load1 f;
    if Buffer.length bb > 0
    then
      try
        let tmp = path ^ ".tmp" in
        let oc = open_out_bin tmp in
        Buffer.output_buffer oc bb;
        close_out oc;
        Sys.rename tmp path;
      with exn ->
        prerr_endline
          ("error while saving configuration: " ^ Printexc.to_string exn)
  ;;
end;;

let () =
  Arg.parse
    ["-p", Arg.String (fun s -> state.password <- s) , "password"]
    (fun s -> state.path <- s)
    ("Usage: " ^ Sys.argv.(0) ^ " [options] some.pdf\noptions:")
  ;
  let path =
    if String.length state.path = 0
    then (prerr_endline "filename missing"; exit 1)
    else (
      if Filename.is_relative state.path
      then Filename.concat (Sys.getcwd ()) state.path
      else state.path
    )
  in
  state.path <- path;

  State.load ();

  let _ = Glut.init Sys.argv in
  let () = Glut.initDisplayMode ~depth:false ~double_buffer:true () in
  let () = Glut.initWindowSize conf.winw conf.winh in
  let _ = Glut.createWindow ("llpp " ^ Filename.basename path) in

  let csock, ssock =
    if Sys.os_type = "Unix"
    then
      Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
    else
      let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 1337) in
      let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock addr;
      Unix.listen sock 1;
      let csock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.connect csock addr;
      let ssock, _ = Unix.accept sock in
      Unix.close sock;
      let opts sock =
        Unix.setsockopt sock Unix.TCP_NODELAY true;
        Unix.setsockopt_optint sock Unix.SO_LINGER None;
      in
      opts ssock;
      opts csock;
      at_exit (fun () -> Unix.shutdown ssock Unix.SHUTDOWN_ALL);
      ssock, csock
  in

  let () = Glut.displayFunc display in
  let () = Glut.reshapeFunc reshape in
  let () = Glut.keyboardFunc keyboard in
  let () = Glut.specialFunc special in
  let () = Glut.idleFunc (Some idle) in
  let () = Glut.mouseFunc mouse in
  let () = Glut.motionFunc motion in
  let () = Glut.passiveMotionFunc pmotion in

  init ssock (conf.angle, conf.proportional, conf.texcount, conf.sliceheight);
  state.csock <- csock;
  state.ssock <- ssock;
  state.text <- "Opening " ^ path;
  writeopen state.path state.password;

  at_exit State.save;

  let rec handlelablglutbug () =
    try
      Glut.mainLoop ();
    with Glut.BadEnum "key in special_of_int" ->
      showtext '!' " LablGlut bug: special key not recognized";
      handlelablglutbug ()
  in
  handlelablglutbug ();
;;
