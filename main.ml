open Utils;;

exception Quit;;

type under =
    | Unone
    | Ulinkuri of string
    | Ulinkgoto of (int * int)
    | Utext of facename
    | Uunexpected of string
    | Ulaunch of string
    | Unamed of string
    | Uremote of (string * int)
and facename = string;;

type mark =
    | Mark_page
    | Mark_block
    | Mark_line
    | Mark_word
;;

type params = (angle * fitmodel * trimparams
                * texcount * sliceheight * memsize
                * colorspace * fontpath * trimcachepath
                * haspbo)
and pageno         = int
and width          = int
and height         = int
and leftx          = int
and opaque         = string
and recttype       = int
and pixmapsize     = int
and angle          = int
and trimmargins    = bool
and interpagespace = int
and texcount       = int
and sliceheight    = int
and gen            = int
and top            = float
and dtop           = float
and fontpath       = string
and trimcachepath  = string
and memsize        = int
and aalevel        = int
and irect          = (int * int * int * int)
and trimparams     = (trimmargins * irect)
and colorspace     = | Rgb | Bgr | Gray
and fitmodel       = | FitWidth | FitProportional | FitPage
and haspbo         = bool
;;

type x = int
and y = int
and tilex = int
and tiley = int
and tileparams = (x * y * width * height * tilex * tiley)
;;

type link =
    | Lnotfound
    | Lfound of int
and linkdir =
    | LDfirst
    | LDlast
    | LDfirstvisible of (int * int * int)
    | LDleft of int
    | LDright of int
    | LDdown of int
    | LDup of int
;;

type pagewithlinks =
    | Pwlnotfound
    | Pwl of int
;;

type keymap =
    | KMinsrt of key
    | KMinsrl of key list
    | KMmulti of key list * key list
and key = int * int
and keyhash = (key, keymap) Hashtbl.t
and keystate =
    | KSnone
    | KSinto of (key list * key list)
;;

type platform = | Punknown | Plinux | Posx | Psun | Pfreebsd
                | Pdragonflybsd | Popenbsd | Pnetbsd | Pcygwin;;

type pipe = (Unix.file_descr * Unix.file_descr);;

external init : pipe -> params -> unit = "ml_init";;
external seltext : string -> (int * int * int * int) -> unit = "ml_seltext";;
external copysel : Unix.file_descr -> opaque -> bool -> unit = "ml_copysel";;
external getpdimrect : int -> float array = "ml_getpdimrect";;
external whatsunder : string -> int -> int -> under = "ml_whatsunder";;
external markunder : string -> int -> int -> mark -> bool = "ml_markunder";;
external zoomforh : int -> int -> int -> int -> float = "ml_zoom_for_height";;
external drawstr : int -> int -> int -> string -> float = "ml_draw_string";;
external measurestr : int -> string -> float = "ml_measure_string";;
external postprocess :
  opaque -> int -> int -> int -> (int * string * int) -> int
  = "ml_postprocess";;
external pagebbox : opaque -> (int * int * int * int) = "ml_getpagebox";;
external platform : unit -> platform = "ml_platform";;
external setaalevel : int -> unit = "ml_setaalevel";;
external realloctexts : int -> bool = "ml_realloctexts";;
external findlink : opaque -> linkdir -> link = "ml_findlink";;
external getlink : opaque -> int -> under = "ml_getlink";;
external getlinkrect : opaque -> int -> irect = "ml_getlinkrect";;
external getlinkcount : opaque -> int = "ml_getlinkcount";;
external findpwl : int -> int -> pagewithlinks = "ml_find_page_with_links"
external popen : string -> (Unix.file_descr * int) list -> unit = "ml_popen";;
external getpbo : width -> height -> colorspace -> string = "ml_getpbo";;
external freepbo : string -> unit = "ml_freepbo";;
external unmappbo : string -> unit = "ml_unmappbo";;
external pbousable : unit -> bool = "ml_pbo_usable";;
external unproject : opaque -> int -> int -> (int * int) option
  = "ml_unproject";;
external drawtile : tileparams -> opaque -> unit = "ml_drawtile";;

let platform_to_string = function
  | Punknown      -> "unknown"
  | Plinux        -> "Linux"
  | Posx          -> "OSX"
  | Psun          -> "Sun"
  | Pfreebsd      -> "FreeBSD"
  | Pdragonflybsd -> "DragonflyBSD"
  | Popenbsd      -> "OpenBSD"
  | Pnetbsd       -> "NetBSD"
  | Pcygwin       -> "Cygwin"
;;

let platform = platform ();;

let now = Unix.gettimeofday;;

let selfexec = ref "";;

let popen cmd fda =
  if platform = Pcygwin
  then (
    let sh = "/bin/sh" in
    let args = [|sh; "-c"; cmd|] in
    let rec std si so se = function
      | [] -> si, so, se
      | (fd, 0) :: rest -> std fd so se rest
      | (fd, -1) :: rest ->
          Unix.set_close_on_exec fd;
          std si so se rest
      | (_, n) :: _ ->
          failwith ("unexpected fdn in cygwin popen " ^ string_of_int n)
    in
    let si, so, se = std Unix.stdin Unix.stdout Unix.stderr fda in
    ignore (Unix.create_process sh args si so se)
  )
  else popen cmd fda;
;;

type mpos = int * int
and mstate =
    | Msel of (mpos * mpos)
    | Mpan of mpos
    | Mscrolly | Mscrollx
    | Mzoom of (int * int)
    | Mzoomrect of (mpos * mpos)
    | Mnone
;;

type textentry = string * string * onhist option * onkey * ondone * cancelonempty
and onkey = string -> int -> te
and ondone = string -> unit
and histcancel = unit -> unit
and onhist = ((histcmd -> string) * histcancel)
and histcmd = HCnext | HCprev | HCfirst | HClast
and cancelonempty = bool
and te =
    | TEstop
    | TEdone of string
    | TEcont of string
    | TEswitch of textentry
;;

type 'a circbuf =
    { store : 'a array
    ; mutable rc : int
    ; mutable wc : int
    ; mutable len : int
    }
;;

let bound v minv maxv =
  max minv (min maxv v);
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
          if rc >= b.len
          then 0
          else rc
        )
      )
      else bound rc 0 (b.len-1)
    in
    b.rc <- rc;
    b.store.(rc);
;;

let cbget b = cbgetg b false;;
let cbgetc b = cbgetg b true;;

let drawstring size x y s =
  Gl.enable `blend;
  Gl.enable `texture_2d;
  GlFunc.blend_func `src_alpha `one_minus_src_alpha;
  ignore (drawstr size x y s);
  Gl.disable `blend;
  Gl.disable `texture_2d;
;;

let drawstring1 size x y s =
  drawstr size x y s;
;;

let drawstring2 size x y fmt =
  Printf.kprintf (drawstring size (x+1) (y+size+1)) fmt
;;

type page =
    { pageno    : int
    ; pagedimno : int
    ; pagew     : int
    ; pageh     : int
    ; pagex     : int
    ; pagey     : int
    ; pagevw    : int
    ; pagevh    : int
    ; pagedispx : int
    ; pagedispy : int
    ; pagecol   : int
    }
;;

let debugl l =
  dolog "l %d dim=%d {" l.pageno l.pagedimno;
  dolog "  WxH     %dx%d" l.pagew l.pageh;
  dolog "  vWxH    %dx%d" l.pagevw l.pagevh;
  dolog "  pagex,y %d,%d" l.pagex l.pagey;
  dolog "  dispx,y %d,%d" l.pagedispx l.pagedispy;
  dolog "  column  %d" l.pagecol;
  dolog "}";
;;

let debugrect (x0, y0, x1, y1, x2, y2, x3, y3) =
  dolog "rect {";
  dolog "  x0,y0=(% f, % f)" x0 y0;
  dolog "  x1,y1=(% f, % f)" x1 y1;
  dolog "  x2,y2=(% f, % f)" x2 y2;
  dolog "  x3,y3=(% f, % f)" x3 y3;
  dolog "}";
;;

type multicolumns = multicol * pagegeom
and singlecolumn = pagegeom
and splitcolumns = columncount * pagegeom
and pagegeom = ((pdimno * x * y * (pageno * width * height * leftx)) array)
and multicol = columncount * covercount * covercount
and pdimno = int
and columncount = int
and covercount = int;;

type scrollb = int;;
let scrollbvv = 1;;
let scrollbhv = 2;;

type conf =
    { mutable scrollbw       : int
    ; mutable scrollh        : int
    ; mutable scrollb        : scrollb
    ; mutable icase          : bool
    ; mutable preload        : bool
    ; mutable pagebias       : int
    ; mutable verbose        : bool
    ; mutable debug          : bool
    ; mutable scrollstep     : int
    ; mutable hscrollstep    : int
    ; mutable maxhfit        : bool
    ; mutable crophack       : bool
    ; mutable autoscrollstep : int
    ; mutable maxwait        : float option
    ; mutable hlinks         : bool
    ; mutable underinfo      : bool
    ; mutable interpagespace : interpagespace
    ; mutable zoom           : float
    ; mutable presentation   : bool
    ; mutable angle          : angle
    ; mutable cwinw          : int
    ; mutable cwinh          : int
    ; mutable savebmarks     : bool
    ; mutable fitmodel       : fitmodel
    ; mutable trimmargins    : trimmargins
    ; mutable trimfuzz       : irect
    ; mutable memlimit       : memsize
    ; mutable texcount       : texcount
    ; mutable sliceheight    : sliceheight
    ; mutable thumbw         : width
    ; mutable jumpback       : bool
    ; mutable bgcolor        : (float * float * float)
    ; mutable bedefault      : bool
    ; mutable tilew          : int
    ; mutable tileh          : int
    ; mutable mustoresize    : memsize
    ; mutable checkers       : bool
    ; mutable aalevel        : int
    ; mutable urilauncher    : string
    ; mutable pathlauncher   : string
    ; mutable colorspace     : colorspace
    ; mutable invert         : bool
    ; mutable colorscale     : float
    ; mutable redirectstderr : bool
    ; mutable ghyllscroll    : (int * int * int) option
    ; mutable columns        : columns
    ; mutable beyecolumns    : columncount option
    ; mutable selcmd         : string
    ; mutable paxcmd         : string
    ; mutable updatecurs     : bool
    ; mutable keyhashes      : (string * keyhash) list
    ; mutable hfsize         : int
    ; mutable pgscale        : float
    ; mutable usepbo         : bool
    ; mutable wheelbypage    : bool
    ; mutable stcmd          : string
    ; mutable riani          : bool
    ; mutable pax            : (float * int * int) ref option
    ; mutable paxmark        : mark
    }
and columns =
    | Csingle of singlecolumn
    | Cmulti of multicolumns
    | Csplit of splitcolumns
;;

type anchor = pageno * top * dtop;;

type outline = string * int * anchor;;

type rect = float * float * float * float * float * float * float * float;;

type tile = opaque * pixmapsize * elapsed
and elapsed = float;;
type pagemapkey = pageno * gen;;
type tilemapkey = pageno * gen * colorspace * angle * width * height * col * row
and row = int
and col = int;;

let emptyanchor = (0, 0.0, 0.0);;

type infochange = | Memused | Docinfo | Pdim;;

class type uioh = object
  method display : unit
  method key : int -> int -> uioh
  method button : int -> bool -> int -> int -> int -> uioh
  method motion : int -> int -> uioh
  method pmotion : int -> int -> uioh
  method infochanged : infochange -> unit
  method scrollpw : (int * float * float)
  method scrollph : (int * float * float)
  method modehash : keyhash
  method eformsgs : bool
end;;

type mode =
    | Birdseye of (conf * leftx * pageno * pageno * anchor)
    | Textentry of (textentry * onleave)
    | View
    | LinkNav of linktarget
and onleave = leavetextentrystatus -> unit
and leavetextentrystatus = | Cancel | Confirm
and helpitem = string * int * action
and action =
    | Noaction
    | Action of (uioh -> uioh)
and linktarget =
    | Ltexact of (pageno * int)
    | Ltgendir of int
;;

let isbirdseye = function Birdseye _ -> true | _ -> false;;
let istextentry = function Textentry _ -> true | _ -> false;;

type currently =
    | Idle
    | Loading of (page * gen)
    | Tiling of (
        page * opaque * colorspace * angle * gen * col * row * width * height
      )
    | Outlining of outline list
;;

let emptykeyhash = Hashtbl.create 0;;
let nouioh : uioh = object (self)
  method display = ()
  method key _ _ = self
  method button _ _ _ _ _ = self
  method motion _ _ = self
  method pmotion _ _ = self
  method infochanged _ = ()
  method scrollpw = (0, nan, nan)
  method scrollph = (0, nan, nan)
  method modehash = emptykeyhash
  method eformsgs = false
end;;

type state =
    { mutable sr            : Unix.file_descr
    ; mutable sw            : Unix.file_descr
    ; mutable wsfd          : Unix.file_descr
    ; mutable errfd         : Unix.file_descr option
    ; mutable stderr        : Unix.file_descr
    ; mutable errmsgs       : Buffer.t
    ; mutable newerrmsgs    : bool
    ; mutable w             : int
    ; mutable x             : int
    ; mutable y             : int
    ; mutable anchor        : anchor
    ; mutable ranchors      : (string * string * anchor * string) list
    ; mutable maxy          : int
    ; mutable layout        : page list
    ; pagemap               : (pagemapkey, opaque) Hashtbl.t
    ; tilemap               : (tilemapkey, tile) Hashtbl.t
    ; tilelru               : (tilemapkey * opaque * pixmapsize) Queue.t
    ; mutable pdims         : (pageno * width * height * leftx) list
    ; mutable pagecount     : int
    ; mutable currently     : currently
    ; mutable mstate        : mstate
    ; mutable searchpattern : string
    ; mutable rects         : (pageno * recttype * rect) list
    ; mutable rects1        : (pageno * recttype * rect) list
    ; mutable text          : string
    ; mutable winstate      : Wsi.winstate list
    ; mutable mode          : mode
    ; mutable uioh          : uioh
    ; mutable outlines      : outline array
    ; mutable bookmarks     : outline list
    ; mutable path          : string
    ; mutable password      : string
    ; mutable nameddest     : string
    ; mutable geomcmds      : (string * ((string * (unit -> unit)) list))
    ; mutable memused       : memsize
    ; mutable gen           : gen
    ; mutable throttle      : (page list * int * float) option
    ; mutable autoscroll    : int option
    ; mutable ghyll         : (int option -> unit)
    ; mutable help          : helpitem array
    ; mutable docinfo       : (int * string) list
    ; mutable texid         : GlTex.texture_id option
    ; hists                 : hists
    ; mutable prevzoom      : float
    ; mutable progress      : float
    ; mutable redisplay     : bool
    ; mutable mpos          : mpos
    ; mutable keystate      : keystate
    ; mutable glinks        : bool
    ; mutable prevcolumns   : (columns * float) option
    ; mutable winw          : int
    ; mutable winh          : int
    ; mutable reprf         : (unit -> unit)
    ; mutable origin        : string
    ; mutable roam          : (unit -> unit)
    }
and hists =
    { pat : string circbuf
    ; pag : string circbuf
    ; nav : anchor circbuf
    ; sel : string circbuf
    }
;;

let defconf =
  { scrollbw       = 7
  ; scrollh        = 12
  ; scrollb        = scrollbhv lor scrollbvv
  ; icase          = true
  ; preload        = true
  ; pagebias       = 0
  ; verbose        = false
  ; debug          = false
  ; scrollstep     = 24
  ; hscrollstep    = 24
  ; maxhfit        = true
  ; crophack       = false
  ; autoscrollstep = 2
  ; maxwait        = None
  ; hlinks         = false
  ; underinfo      = false
  ; interpagespace = 2
  ; zoom           = 1.0
  ; presentation   = false
  ; angle          = 0
  ; cwinw          = 900
  ; cwinh          = 900
  ; savebmarks     = true
  ; fitmodel       = FitProportional
  ; trimmargins    = false
  ; trimfuzz       = (0,0,0,0)
  ; memlimit       = 32 lsl 20
  ; texcount       = 256
  ; sliceheight    = 24
  ; thumbw         = 76
  ; jumpback       = true
  ; bgcolor        = (0.5, 0.5, 0.5)
  ; bedefault      = false
  ; tilew          = 2048
  ; tileh          = 2048
  ; mustoresize    = 256 lsl 20
  ; checkers       = true
  ; aalevel        = 8
  ; urilauncher    =
      (match platform with
      | Plinux | Pfreebsd | Pdragonflybsd
      | Popenbsd | Pnetbsd | Psun -> "xdg-open \"%s\""
      | Posx -> "open \"%s\""
      | Pcygwin -> "cygstart \"%s\""
      | Punknown -> "echo %s")
  ; pathlauncher   = "lp \"%s\""
  ; selcmd         =
      (match platform with
      | Plinux | Pfreebsd | Pdragonflybsd
      | Popenbsd | Pnetbsd | Psun -> "xsel -i"
      | Posx -> "pbcopy"
      | Pcygwin -> "wsel"
      | Punknown -> "cat")
  ; paxcmd         = "cat"
  ; colorspace     = Rgb
  ; invert         = false
  ; colorscale     = 1.0
  ; redirectstderr = false
  ; ghyllscroll    = None
  ; columns        = Csingle [||]
  ; beyecolumns    = None
  ; updatecurs     = false
  ; hfsize         = 12
  ; pgscale        = 1.0
  ; usepbo         = false
  ; wheelbypage    = false
  ; stcmd          = "echo SyncTex"
  ; riani          = false
  ; pax            = None
  ; paxmark        = Mark_word
  ; keyhashes      =
      let mk n = (n, Hashtbl.create 1) in
      [ mk "global"
      ; mk "info"
      ; mk "help"
      ; mk "outline"
      ; mk "listview"
      ; mk "birdseye"
      ; mk "textentry"
      ; mk "links"
      ; mk "view"
      ]
  }
;;

let wtmode = ref false;;

let findkeyhash c name =
  try List.assoc name c.keyhashes
  with Not_found -> failwith ("invalid mode name `" ^ name ^ "'")
;;

let conf = { defconf with angle = defconf.angle };;

let pgscale h = truncate (float h *. conf.pgscale);;

type fontstate =
    { mutable fontsize : int
    ; mutable wwidth : float
    ; mutable maxrows : int
    }
;;

let fstate =
  { fontsize = 14
  ; wwidth = nan
  ; maxrows = -1
  }
;;

let geturl s =
  let colonpos = try String.index s ':' with Not_found -> -1 in
  let len = String.length s in
  if colonpos >= 0 && colonpos + 3 < len
  then (
    if s.[colonpos+1] = '/' && s.[colonpos+2] = '/'
    then
      let schemestartpos =
        try String.rindex_from s colonpos ' '
        with Not_found -> -1
      in
      let scheme =
        String.sub s (schemestartpos+1) (colonpos-1-schemestartpos)
      in
      match scheme with
      | "http" | "ftp" | "mailto" ->
          let epos =
            try String.index_from s colonpos ' '
            with Not_found -> len
          in
          String.sub s (schemestartpos+1) (epos-1-schemestartpos)
      | _ -> ""
    else ""
  )
  else ""
;;

let gotouri uri =
  if String.length conf.urilauncher = 0
  then print_endline uri
  else (
    let url = geturl uri in
    if String.length url = 0
    then Printf.eprintf "obtained empty url from uri %S" uri
    else
      let re = Str.regexp "%s" in
      let command = Str.global_replace re url conf.urilauncher in
      try popen command []
      with exn ->
        Printf.eprintf
          "failed to execute `%s': %s\n" command (exntos exn);
        flush stderr;
  );
;;

let version () =
  Printf.sprintf "llpp version %s (%s/%dbit, ocaml %s)" Help.version
    (platform_to_string platform) Sys.word_size Sys.ocaml_version
;;

let makehelp () =
  let strings = version () :: "" :: Help.keys in
  Array.of_list (
    List.map (fun s ->
      let url = geturl s in
      if String.length url > 0
      then (s, 0, Action (fun u -> gotouri url; u))
      else (s, 0, Noaction)
    ) strings);
;;

let noghyll _ = ();;
let firstgeomcmds = "", [];;
let noreprf () = ();;

let state =
  { sr            = Unix.stdin
  ; sw            = Unix.stdin
  ; wsfd          = Unix.stdin
  ; errfd         = None
  ; stderr        = Unix.stderr
  ; errmsgs       = Buffer.create 0
  ; newerrmsgs    = false
  ; x             = 0
  ; y             = 0
  ; w             = 0
  ; anchor        = emptyanchor
  ; ranchors      = []
  ; layout        = []
  ; maxy          = max_int
  ; tilelru       = Queue.create ()
  ; pagemap       = Hashtbl.create 10
  ; tilemap       = Hashtbl.create 10
  ; pdims         = []
  ; pagecount     = 0
  ; currently     = Idle
  ; mstate        = Mnone
  ; rects         = []
  ; rects1        = []
  ; text          = ""
  ; mode          = View
  ; winstate      = []
  ; searchpattern = ""
  ; outlines      = [||]
  ; bookmarks     = []
  ; path          = ""
  ; password      = ""
  ; nameddest     = ""
  ; geomcmds      = firstgeomcmds
  ; hists         =
      { nav       = cbnew 10 emptyanchor
      ; pat       = cbnew 10 ""
      ; pag       = cbnew 10 ""
      ; sel       = cbnew 10 ""
      }
  ; memused       = 0
  ; gen           = 0
  ; throttle      = None
  ; autoscroll    = None
  ; ghyll         = noghyll
  ; help          = makehelp ()
  ; docinfo       = []
  ; texid         = None
  ; prevzoom      = 1.0
  ; progress      = -1.0
  ; uioh          = nouioh
  ; redisplay     = true
  ; mpos          = (-1, -1)
  ; keystate      = KSnone
  ; glinks        = false
  ; prevcolumns   = None
  ; winw          = -1
  ; winh          = -1
  ; reprf         = noreprf
  ; origin        = ""
  ; roam          = (fun () -> ())
  }
;;

let hscrollh () =
  if (conf.scrollb land scrollbhv = 0)
    || (state.x = 0 && state.w <= state.winw - conf.scrollbw)
  then 0
  else conf.scrollbw
;;

let vscrollw () =
  if (conf.scrollb land scrollbvv = 0)
  then 0
  else conf.scrollbw
;;

let wadjsb w = w - vscrollw ();;

let setfontsize n =
  fstate.fontsize <- n;
  fstate.wwidth <- measurestr fstate.fontsize "w";
  fstate.maxrows <- (state.winh - fstate.fontsize - 1) / (fstate.fontsize + 1);
;;

let vlog fmt =
  if conf.verbose
  then
    Printf.kprintf prerr_endline fmt
  else
    Printf.kprintf ignore fmt
;;

let launchpath () =
  if String.length conf.pathlauncher = 0
  then print_endline state.path
  else (
    let re = Str.regexp "%s" in
    let command = Str.global_replace re state.path conf.pathlauncher in
    try popen command []
    with exn ->
      Printf.eprintf "failed to execute `%s': %s\n" command (exntos exn);
      flush stderr;
  );
;;

module Ne = struct
  type 'a t = | Res of 'a | Exn of exn;;

  let pipe () =
    try Res (Unix.pipe ())
    with exn -> Exn exn
  ;;

  let clo fd f =
    try tempfailureretry Unix.close fd
    with exn -> f (exntos exn)
  ;;

  let dup fd =
    try Res (tempfailureretry Unix.dup fd)
    with exn -> Exn exn
  ;;

  let dup2 fd1 fd2 =
    try Res (tempfailureretry (Unix.dup2 fd1) fd2)
    with exn -> Exn exn
  ;;
end;;

let redirectstderr () =
  let clofail what errmsg = dolog "failed to close %s: %s" what errmsg in
  if conf.redirectstderr
  then
    match Ne.pipe () with
    | Ne.Exn exn ->
        dolog "failed to create stderr redirection pipes: %s" (exntos exn)

    | Ne.Res (r, w) ->
        begin match Ne.dup Unix.stderr with
        | Ne.Exn exn ->
            dolog "failed to dup stderr: %s" (exntos exn);
            Ne.clo r (clofail "pipe/r");
            Ne.clo w (clofail "pipe/w");

        | Ne.Res dupstderr ->
            begin match Ne.dup2 w Unix.stderr with
            | Ne.Exn exn ->
                dolog "failed to dup2 to stderr: %s" (exntos exn);
                Ne.clo dupstderr (clofail "stderr duplicate");
                Ne.clo r (clofail "redir pipe/r");
                Ne.clo w (clofail "redir pipe/w");

            | Ne.Res () ->
                state.stderr <- dupstderr;
                state.errfd <- Some r;
            end;
        end
  else (
    state.newerrmsgs <- false;
    begin match state.errfd with
    | Some fd ->
        begin match Ne.dup2 state.stderr Unix.stderr with
        | Ne.Exn exn ->
            dolog "failed to dup2 original stderr: %s" (exntos exn)
        | Ne.Res () ->
            Ne.clo fd (clofail "dup of stderr");
            state.errfd <- None;
        end;
    | None -> ()
    end;
    prerr_string (Buffer.contents state.errmsgs);
    flush stderr;
    Buffer.clear state.errmsgs;
  )
;;

module G =
struct
  let postRedisplay who =
    if conf.verbose
    then prerr_endline ("redisplay for " ^ who);
    state.redisplay <- true;
  ;;
end;;

let getopaque pageno =
  try Some (Hashtbl.find state.pagemap (pageno, state.gen))
  with Not_found -> None
;;

let putopaque pageno opaque =
  Hashtbl.replace state.pagemap (pageno, state.gen) opaque
;;

let pagetranslatepoint l x y =
  let dy = y - l.pagedispy in
  let y = dy + l.pagey in
  let dx = x - l.pagedispx in
  let x = dx + l.pagex in
  (x, y);
;;

let onppundermouse g x y d =
  let rec f = function
    | l :: rest ->
        begin match getopaque l.pageno with
        | Some opaque ->
            let x0 = l.pagedispx in
            let x1 = x0 + l.pagevw in
            let y0 = l.pagedispy in
            let y1 = y0 + l.pagevh in
            if y >= y0 && y <= y1 && x >= x0 && x <= x1
            then
              let px, py = pagetranslatepoint l x y in
              match g opaque l px py with
              | Some res -> res
              | None -> f rest
            else f rest
        | _ ->
            f rest
        end
    | [] -> d
  in
  f state.layout
;;

let getunder x y =
  let g opaque _ px py =
    match whatsunder opaque px py with
    | Unone -> None
    | under -> Some under
  in
  onppundermouse g x y Unone
;;

let unproject x y =
  let g opaque l x y =
    match unproject opaque x y with
    | Some (x, y) -> Some (Some (l.pageno, x, y))
    | None -> None
  in
  onppundermouse g x y None;
;;

let showtext c s =
  state.text <- Printf.sprintf "%c%s" c s;
  G.postRedisplay "showtext";
;;

let paxunder x y =
  let g opaque l px py =
    if markunder opaque px py conf.paxmark
    then (
      Some (fun () ->
        match getopaque l.pageno with
        | None -> ()
        | Some opaque ->
            match Ne.pipe () with
            | Ne.Exn exn ->
                showtext '!'
                  (Printf.sprintf
                      "can not create mark pipe: %s"
                      (exntos exn));
            | Ne.Res (r, w) ->
                let doclose what fd =
                  Ne.clo fd (fun msg ->
                    dolog "%s close failed: %s" what msg)
                in
                try
                  popen conf.paxcmd [r, 0; w, -1];
                  copysel w opaque false;
                  doclose "pipe/r" r;
                  G.postRedisplay "paxunder";
                with exn ->
                  dolog "can not execute %S: %s"
                    conf.paxcmd (exntos exn);
                  doclose "pipe/r" r;
                  doclose "pipe/w" w;
      )
    )
    else None
  in
  G.postRedisplay "paxunder";
  state.roam <-
    onppundermouse g x y (fun () -> showtext '!' "Whoopsie daisy");
;;

let selstring s =
  match Ne.pipe () with
  | Ne.Exn exn ->
      showtext '!' (Printf.sprintf "pipe failed: %s" (exntos exn))
  | Ne.Res (r, w) ->
      let popened =
        try popen conf.selcmd [r, 0; w, -1]; true
        with exn ->
          showtext '!'
            (Printf.sprintf "failed to execute %s: %s"
                conf.selcmd (exntos exn));
          false
      in
      let clo cap fd =
        Ne.clo fd (fun msg ->
          showtext '!' (Printf.sprintf "failed to close %s: %s" cap msg)
        )
      in
      if popened
      then
        (try
            let l = String.length s in
            let n = tempfailureretry (Unix.write w s 0) l in
            if n != l
            then
              showtext '!'
                (Printf.sprintf
                    "failed to write %d characters to sel pipe, wrote %d"
                    l n
                )
          with exn ->
            showtext '!'
              (Printf.sprintf "failed to write to sel pipe: %s"
                  (exntos exn)
              )
        )
      else dolog "%s" s;
      clo "pipe/r" r;
      clo "pipe/w" w;
;;

let undertext = function
  | Unone -> "none"
  | Ulinkuri s -> s
  | Ulinkgoto (pageno, _) -> Printf.sprintf "%s: page %d" state.path (pageno+1)
  | Utext s -> "font: " ^ s
  | Uunexpected s -> "unexpected: " ^ s
  | Ulaunch s -> "launch: " ^ s
  | Unamed s -> "named: " ^ s
  | Uremote (filename, pageno) ->
      Printf.sprintf "%s: page %d" filename (pageno+1)
;;

let updateunder x y =
  match getunder x y with
  | Unone -> Wsi.setcursor Wsi.CURSOR_INHERIT
  | Ulinkuri uri ->
      if conf.underinfo then showtext 'u' ("ri: " ^ uri);
      Wsi.setcursor Wsi.CURSOR_INFO
  | Ulinkgoto (pageno, _) ->
      if conf.underinfo
      then showtext 'p' ("age: " ^ string_of_int (pageno+1));
      Wsi.setcursor Wsi.CURSOR_INFO
  | Utext s ->
      if conf.underinfo then showtext 'f' ("ont: " ^ s);
      Wsi.setcursor Wsi.CURSOR_TEXT
  | Uunexpected s ->
      if conf.underinfo then showtext 'u' ("nexpected: " ^ s);
      Wsi.setcursor Wsi.CURSOR_INHERIT
  | Ulaunch s ->
      if conf.underinfo then showtext 'l' ("aunch: " ^ s);
      Wsi.setcursor Wsi.CURSOR_INHERIT
  | Unamed s ->
      if conf.underinfo then showtext 'n' ("amed: " ^ s);
      Wsi.setcursor Wsi.CURSOR_INHERIT
  | Uremote (filename, pageno) ->
      if conf.underinfo then showtext 'r'
        (Printf.sprintf "emote: %s (%d)" filename (pageno+1));
      Wsi.setcursor Wsi.CURSOR_INFO
;;

let showlinktype under =
  if conf.underinfo
  then
    match under with
    | Unone -> ()
    | under ->
        let s = undertext under in
        showtext ' ' s
;;

let addchar s c =
  let b = Buffer.create (String.length s + 1) in
  Buffer.add_string b s;
  Buffer.add_char b c;
  Buffer.contents b;
;;

let colorspace_of_string s =
  match String.lowercase s with
  | "rgb" -> Rgb
  | "bgr" -> Bgr
  | "gray" -> Gray
  | _ -> failwith "invalid colorspace"
;;

let int_of_colorspace = function
  | Rgb -> 0
  | Bgr -> 1
  | Gray -> 2
;;

let colorspace_of_int = function
  | 0 -> Rgb
  | 1 -> Bgr
  | 2 -> Gray
  | n -> failwith ("invalid colorspace index " ^ string_of_int n)
;;

let colorspace_to_string = function
  | Rgb -> "rgb"
  | Bgr -> "bgr"
  | Gray -> "gray"
;;

let mark_of_string s =
  match String.lowercase s with
  | "word" -> Mark_word
  | "line" -> Mark_line
  | "block" -> Mark_block
  | "page" -> Mark_page
  | _ -> failwith "invalid colorspace"
;;

let int_of_mark = function
  | Mark_page -> 0
  | Mark_block -> 1
  | Mark_line -> 2
  | Mark_word -> 3
;;

let mark_of_int = function
  | 0 -> Mark_page
  | 1 -> Mark_block
  | 2 -> Mark_line
  | 3 -> Mark_word
  | n -> failwith ("invalid mark index " ^ string_of_int n)
;;

let mark_to_string = function
  | Mark_word -> "word"
  | Mark_line -> "line"
  | Mark_block -> "block"
  | Mark_page -> "page"
;;

let fitmodel_of_string s =
  match String.lowercase s with
  | "width" -> FitWidth
  | "proportional" -> FitProportional
  | "page" -> FitPage
  | _ -> failwith "invalid fit model"
;;

let int_of_fitmodel = function
  | FitWidth -> 0
  | FitProportional -> 1
  | FitPage -> 2
;;

let fitmodel_of_int = function
  | 0 -> FitWidth
  | 1 -> FitProportional
  | 2 -> FitPage
  | n -> failwith ("invalid fit model index " ^ string_of_int n)
;;

let fitmodel_to_string = function
  | FitWidth -> "width"
  | FitProportional -> "proportional"
  | FitPage -> "page"
;;

let intentry_with_suffix text key =
  let c =
    if key >= 32 && key < 127
    then Char.chr key
    else '\000'
  in
  match Char.lowercase c with
  | '0' .. '9' ->
      let text = addchar text c in
      TEcont text

  | 'k' | 'm' | 'g' ->
      let text = addchar text c in
      TEcont text

  | _ ->
      state.text <- Printf.sprintf "invalid char (%d, `%c')" key c;
      TEcont text
;;

let multicolumns_to_string (n, a, b) =
  if a = 0 && b = 0
  then Printf.sprintf "%d" n
  else Printf.sprintf "%d,%d,%d" n a b;
;;

let multicolumns_of_string s =
  try
    (int_of_string s, 0, 0)
  with _ ->
    Scanf.sscanf s "%u,%u,%u" (fun n a b ->
      if a > 1 || b > 1
      then failwith "subtly broken"; (n, a, b)
    );
;;

let readcmd fd =
  let s = "xxxx" in
  let n = tempfailureretry (Unix.read fd s 0) 4 in
  if n != 4 then failwith "incomplete read(len)";
  let len = 0
    lor (Char.code s.[0] lsl 24)
    lor (Char.code s.[1] lsl 16)
    lor (Char.code s.[2] lsl  8)
    lor (Char.code s.[3] lsl  0)
  in
  let s = String.create len in
  let n = tempfailureretry (Unix.read fd s 0) len in
  if n != len then failwith "incomplete read(data)";
  s
;;

let btod b = if b then 1 else 0;;

let wcmd fmt =
  let b = Buffer.create 16 in
  Buffer.add_string b "llll";
  Printf.kbprintf
    (fun b ->
      let s = Buffer.contents b in
      let n = String.length s in
      let len = n - 4 in
      (* dolog "wcmd %S" (String.sub s 4 len); *)
      s.[0] <- Char.chr ((len lsr 24) land 0xff);
      s.[1] <- Char.chr ((len lsr 16) land 0xff);
      s.[2] <- Char.chr ((len lsr  8) land 0xff);
      s.[3] <- Char.chr (len land 0xff);
      let n' = tempfailureretry (Unix.write state.sw s 0) n in
      if n' != n then failwith "write failed";
    ) b fmt;
;;

let calcips h =
  let d = state.winh - h in
  max conf.interpagespace ((d + 1) / 2)
;;

let rowyh (c, coverA, coverB) b n =
  if c = 1 || (n < coverA || n >= state.pagecount - coverB)
  then
    let _, _, vy, (_, _, h, _) = b.(n) in
    (vy, h)
  else
    let n' = n - coverA in
    let d = n' mod c in
    let s = n - d in
    let e = min state.pagecount (s + c) in
    let rec find m miny maxh = if m = e then miny, maxh else
        let _, _, y, (_, _, h, _) = b.(m) in
        let miny = min miny y in
        let maxh = max maxh h in
        find (m+1) miny maxh
    in find s max_int 0
;;

let calcheight () =
  match conf.columns with
  | Cmulti ((_, _, _) as cl, b) ->
      if Array.length b > 0
      then
        let y, h = rowyh cl b (Array.length b - 1) in
        y + h + (if conf.presentation then calcips h else 0)
      else 0
  | Csingle b ->
      if Array.length b > 0
      then
        let (_, _, y, (_, _, h, _)) = b.(Array.length b - 1) in
        y + h + (if conf.presentation then calcips h else 0)
      else 0
  | Csplit (_, b) ->
      if Array.length b > 0
      then
        let (_, _, y, (_, _, h, _)) = b.(Array.length b - 1) in
        y + h
      else 0
;;

let getpageyh pageno =
  let pageno = bound pageno 0 (state.pagecount-1) in
  match conf.columns with
  | Csingle b ->
      if Array.length b = 0
      then 0, 0
      else
        let (_, _, y, (_, _, h, _)) = b.(pageno) in
        let y =
          if conf.presentation
          then y - calcips h
          else y
        in
        y, h
  | Cmulti (cl, b) ->
      if Array.length b = 0
      then 0, 0
      else
        let y, h = rowyh cl b pageno in
        let y =
          if conf.presentation
          then y - calcips h
          else y
        in
        y, h
  | Csplit (c, b) ->
      if Array.length b = 0
      then 0, 0
      else
        let n = pageno*c in
        let (_, _, y, (_, _, h, _)) = b.(n) in
        y, h
;;

let getpagedim pageno =
  let rec f ppdim l =
    match l with
    | (n, _, _, _) as pdim :: rest ->
        if n >= pageno
        then (if n = pageno then pdim else ppdim)
        else f pdim rest

    | [] -> ppdim
  in
  f (-1, -1, -1, -1) state.pdims
;;

let getpagey pageno = fst (getpageyh pageno);;

let nogeomcmds cmds =
  match cmds with
  | s, [] -> String.length s = 0
  | _ -> false
;;

let page_of_y y =
  let ((c, coverA, coverB) as cl), b =
    match conf.columns with
    | Csingle b -> (1, 0, 0), b
    | Cmulti (c, b) -> c, b
    | Csplit (_, b) -> (1, 0, 0), b
  in
  if Array.length b = 0
  then -1
  else
    let rec bsearch nmin nmax =
      if nmin > nmax
      then bound nmin 0 (state.pagecount-1)
      else
        let n = (nmax + nmin) / 2 in
        let vy, h = rowyh cl b n in
        let y0, y1 =
          if conf.presentation
          then
            let ips = calcips h in
            let y0 = vy - ips in
            let y1 = vy + h + ips in
            y0, y1
          else (
            if n = 0
            then 0, vy + h + conf.interpagespace
            else
              let y0 = vy - conf.interpagespace in
              y0, y0 + h + conf.interpagespace
          )
        in
        if y >= y0 && y < y1
        then (
          if c = 1
          then n
          else (
            if n > coverA
            then
              if n < state.pagecount - coverB
              then ((n-coverA)/c)*c + coverA
              else n
            else n
          )
        )
        else (
          if y > y0
          then bsearch (n+1) nmax
          else bsearch nmin (n-1)
        )
    in
    let r = bsearch 0 (state.pagecount-1) in
    r;
;;

let layoutN ((columns, coverA, coverB), b) y sh =
  let sh = sh - (hscrollh ()) in
  let rec fold accu n =
    if n = Array.length b
    then accu
    else
      let pdimno, dx, vy, (_, w, h, xoff) = b.(n) in
      if (vy - y) > sh &&
        (n = coverA - 1
            || n = state.pagecount - coverB
            || (n - coverA) mod columns = columns - 1)
      then accu
      else
        let accu =
          if vy + h > y
          then
            let pagey = max 0 (y - vy) in
            let pagedispy = if pagey > 0 then 0 else vy - y in
            let pagedispx, pagex =
              let pdx =
                if n = coverA - 1 || n = state.pagecount - coverB
                then state.x + (wadjsb state.winw - w) / 2
                else dx + xoff + state.x
              in
              if pdx < 0
              then 0, -pdx
              else pdx, 0
            in
            let pagevw =
              let vw = wadjsb state.winw - pagedispx in
              let pw = w - pagex in
              min vw pw
            in
            let pagevh = min (h - pagey) (sh - pagedispy) in
            if pagevw > 0 && pagevh > 0
            then
              let e =
                { pageno = n
                ; pagedimno = pdimno
                ; pagew = w
                ; pageh = h
                ; pagex = pagex
                ; pagey = pagey
                ; pagevw = pagevw
                ; pagevh = pagevh
                ; pagedispx = pagedispx
                ; pagedispy = pagedispy
                ; pagecol = 0
                }
              in
              e :: accu
            else
              accu
          else
            accu
        in
        fold accu (n+1)
  in
  List.rev (fold [] (page_of_y y));
;;

let layoutS (columns, b) y sh =
  let sh = sh - hscrollh () in
  let rec fold accu n =
    if n = Array.length b
    then accu
    else
      let pdimno, px, vy, (_, pagew, pageh, xoff) = b.(n) in
      if (vy - y) > sh
      then accu
      else
        let accu =
          if vy + pageh > y
          then
            let x = xoff + state.x in
            let pagey = max 0 (y - vy) in
            let pagedispy = if pagey > 0 then 0 else vy - y in
            let pagedispx, pagex =
              if px = 0
              then (
                if x < 0
                then 0, -x
                else x, 0
              )
              else (
                let px = px - x in
                if px < 0
                then -px, 0
                else 0, px
              )
            in
            let pagecolw = pagew/columns in
            let pagedispx =
              if pagecolw < state.winw
              then pagedispx + ((wadjsb state.winw - pagecolw) / 2)
              else pagedispx
            in
            let pagevw =
              let vw = wadjsb state.winw - pagedispx in
              let pw = pagew - pagex in
              min vw pw
            in
            let pagevw = min pagevw pagecolw in
            let pagevh = min (pageh - pagey) (sh - pagedispy) in
            if pagevw > 0 && pagevh > 0
            then
              let e =
                { pageno = n/columns
                ; pagedimno = pdimno
                ; pagew = pagew
                ; pageh = pageh
                ; pagex = pagex
                ; pagey = pagey
                ; pagevw = pagevw
                ; pagevh = pagevh
                ; pagedispx = pagedispx
                ; pagedispy = pagedispy
                ; pagecol = n mod columns
                }
              in
              e :: accu
            else
              accu
          else
            accu
        in
        fold accu (n+1)
  in
  List.rev (fold [] 0)
;;

let layout y sh =
  if nogeomcmds state.geomcmds
  then
    match conf.columns with
    | Csingle b -> layoutN ((1, 0, 0), b) y sh
    | Cmulti c -> layoutN c y sh
    | Csplit s -> layoutS s y sh
  else []
;;

let clamp incr =
  let y = state.y + incr in
  let y = max 0 y in
  let y = min y (state.maxy - (if conf.maxhfit then state.winh else 0)) in
  y;
;;

let itertiles l f =
  let tilex = l.pagex mod conf.tilew in
  let tiley = l.pagey mod conf.tileh in

  let col = l.pagex / conf.tilew in
  let row = l.pagey / conf.tileh in

  let rec rowloop row y0 dispy h =
    if h = 0
    then ()
    else (
      let dh = conf.tileh - y0 in
      let dh = min h dh in
      let rec colloop col x0 dispx w =
        if w = 0
        then ()
        else (
          let dw = conf.tilew - x0 in
          let dw = min w dw in

          f col row dispx dispy x0 y0 dw dh;
          colloop (col+1) 0 (dispx+dw) (w-dw)
        )
      in
      colloop col tilex l.pagedispx l.pagevw;
      rowloop (row+1) 0 (dispy+dh) (h-dh)
    )
  in
  if l.pagevw > 0 && l.pagevh > 0
  then rowloop row tiley l.pagedispy l.pagevh;
;;

let gettileopaque l col row =
  let key =
    l.pageno, state.gen, conf.colorspace, conf.angle, l.pagew, l.pageh, col, row
  in
  try Some (Hashtbl.find state.tilemap key)
  with Not_found -> None
;;

let puttileopaque l col row gen colorspace angle opaque size elapsed =
  let key = l.pageno, gen, colorspace, angle, l.pagew, l.pageh, col, row in
  Hashtbl.add state.tilemap key (opaque, size, elapsed)
;;

let drawtiles l color =
  GlDraw.color color;
  let f col row x y tilex tiley w h =
    match gettileopaque l col row with
    | Some (opaque, _, t) ->
        let params = x, y, w, h, tilex, tiley in
        if conf.invert
        then (
          Gl.enable `blend;
          GlFunc.blend_func `zero `one_minus_src_color;
        );
        drawtile params opaque;
        if conf.invert
        then Gl.disable `blend;
        if conf.debug
        then (
          let s = Printf.sprintf
            "%d[%d,%d] %f sec"
            l.pageno col row t
          in
          let w = measurestr fstate.fontsize s in
          GlMisc.push_attrib [`current];
          GlDraw.color (0.0, 0.0, 0.0);
          GlDraw.rect
            (float (x-2), float (y-2))
            (float (x+2) +. w, float (y + fstate.fontsize + 2));
          GlDraw.color (1.0, 1.0, 1.0);
          drawstring fstate.fontsize x (y + fstate.fontsize - 1) s;
          GlMisc.pop_attrib ();
        );

    | _ ->
        let w =
          let lw = wadjsb state.winw - x in
          min lw w
        and h =
          let lh = state.winh - y in
          min lh h
        in
        begin match state.texid with
        | Some id ->
            Gl.enable `texture_2d;
            GlTex.bind_texture `texture_2d id;
            let x0 = float x
            and y0 = float y
            and x1 = float (x+w)
            and y1 = float (y+h) in

            let tw = float w /. 16.0
            and th = float h /. 16.0 in
            let tx0 = float tilex /. 16.0
            and ty0 = float tiley /. 16.0 in
            let tx1 = tx0 +. tw
            and ty1 = ty0 +. th in
            GlDraw.begins `quads;
            GlTex.coord2 (tx0, ty0); GlDraw.vertex2 (x0, y0);
            GlTex.coord2 (tx0, ty1); GlDraw.vertex2 (x0, y1);
            GlTex.coord2 (tx1, ty1); GlDraw.vertex2 (x1, y1);
            GlTex.coord2 (tx1, ty0); GlDraw.vertex2 (x1, y0);
            GlDraw.ends ();

            Gl.disable `texture_2d;
        | None ->
            GlDraw.color (1.0, 1.0, 1.0);
            GlDraw.rect
              (float x, float y)
              (float (x+w), float (y+h));
        end;
        if w > 128 && h > fstate.fontsize + 10
        then (
          GlDraw.color (0.0, 0.0, 0.0);
          let c, r =
            if conf.verbose
            then (col*conf.tilew, row*conf.tileh)
            else col, row
          in
          drawstring2 fstate.fontsize x y "Loading %d [%d,%d]" l.pageno c r;
        );
        GlDraw.color color;
  in
  itertiles l f
;;

let pagevisible layout n = List.exists (fun l -> l.pageno = n) layout;;

let tilevisible1 l x y =
  let ax0 = l.pagex
  and ax1 = l.pagex + l.pagevw
  and ay0 = l.pagey
  and ay1 = l.pagey + l.pagevh in

  let bx0 = x
  and by0 = y in
  let bx1 = min (bx0 + conf.tilew) l.pagew
  and by1 = min (by0 + conf.tileh) l.pageh in

  let rx0 = max ax0 bx0
  and ry0 = max ay0 by0
  and rx1 = min ax1 bx1
  and ry1 = min ay1 by1 in

  let nonemptyintersection = rx1 > rx0 && ry1 > ry0 in
  nonemptyintersection
;;

let tilevisible layout n x y =
  let rec findpageinlayout m = function
    | l :: rest when l.pageno = n ->
        tilevisible1 l x y || (
          match conf.columns with
          | Csplit (c, _) when c > m -> findpageinlayout (m+1) rest
          | _ -> false
        )
    | _ :: rest -> findpageinlayout 0 rest
    | [] -> false
  in
  findpageinlayout 0 layout;
;;

let tileready l x y =
  tilevisible1 l x y &&
    gettileopaque l (x/conf.tilew) (y/conf.tileh) != None
;;

let tilepage n p layout =
  let rec loop = function
    | l :: rest ->
        if l.pageno = n
        then
          let f col row _ _ _ _ _ _ =
            if state.currently = Idle
            then
              match gettileopaque l col row with
              | Some _ -> ()
              | None ->
                  let x = col*conf.tilew
                  and y = row*conf.tileh in
                  let w =
                    let w = l.pagew - x in
                    min w conf.tilew
                  in
                  let h =
                    let h = l.pageh - y in
                    min h conf.tileh
                  in
                  let pbo =
                    if conf.usepbo
                    then getpbo w h conf.colorspace
                    else "0"
                  in
                  wcmd "tile %s %d %d %d %d %s" p x y w h pbo;
                  state.currently <-
                    Tiling (
                      l, p, conf.colorspace, conf.angle, state.gen, col, row,
                      conf.tilew, conf.tileh
                    );
          in
          itertiles l f;
        else
          loop rest

    | [] -> ()
  in
  if nogeomcmds state.geomcmds
  then loop layout;
;;

let preloadlayout y =
  let y = if y < state.winh then 0 else y - state.winh in
  let h = state.winh*3 in
  layout y h;
;;

let load pages =
  let rec loop pages =
    if state.currently != Idle
    then ()
    else
      match pages with
      | l :: rest ->
          begin match getopaque l.pageno with
          | None ->
              wcmd "page %d %d" l.pageno l.pagedimno;
              state.currently <- Loading (l, state.gen);
          | Some opaque ->
              tilepage l.pageno opaque pages;
              loop rest
          end;
      | _ -> ()
  in
  if nogeomcmds state.geomcmds
  then loop pages
;;

let preload pages =
  load pages;
  if conf.preload && state.currently = Idle
  then load (preloadlayout state.y);
;;

let layoutready layout =
  let rec fold all ls =
    all && match ls with
    | l :: rest ->
        let seen = ref false in
        let allvisible = ref true in
        let foo col row _ _ _ _ _ _ =
          seen := true;
          allvisible := !allvisible &&
            begin match gettileopaque l col row with
            | Some _ -> true
            | None -> false
            end
        in
        itertiles l foo;
        fold (!seen && !allvisible) rest
    | [] -> true
  in
  let alltilesvisible = fold true layout in
  alltilesvisible;
;;

let gotoy y =
  let y = bound y 0 state.maxy in
  let y, layout, proceed =
    match conf.maxwait with
    | Some time when state.ghyll == noghyll ->
        begin match state.throttle with
        | None ->
            let layout = layout y state.winh in
            let ready = layoutready layout in
            if not ready
            then (
              load layout;
              state.throttle <- Some (layout, y, now ());
            )
            else G.postRedisplay "gotoy showall (None)";
            y, layout, ready
        | Some (_, _, started) ->
            let dt = now () -. started in
            if dt > time
            then (
              state.throttle <- None;
              let layout = layout y state.winh in
              load layout;
              G.postRedisplay "maxwait";
              y, layout, true
            )
            else -1, [], false
        end

    | _ ->
        let layout = layout y state.winh in
        if not !wtmode || layoutready layout
        then G.postRedisplay "gotoy ready";
        y, layout, true
  in
  if proceed
  then (
    state.y <- y;
    state.layout <- layout;
    begin match state.mode with
    | LinkNav (Ltexact (pageno, linkno)) ->
        let rec loop = function
          | [] ->
              state.mode <- LinkNav (Ltgendir 0)
          | l :: _ when l.pageno = pageno ->
              begin match getopaque pageno with
              | None ->
                  state.mode <- LinkNav (Ltgendir 0)
              | Some opaque ->
                  let x0, y0, x1, y1 = getlinkrect opaque linkno in
                  if not (x0 >= l.pagex && x1 <= l.pagex + l.pagevw
                           && y0 >= l.pagey && y1 <= l.pagey + l.pagevh)
                  then state.mode <- LinkNav (Ltgendir 0)
              end
          | _ :: rest -> loop rest
        in
        loop layout
    | _ -> ()
    end;
    begin match state.mode with
    | Birdseye (conf, leftx, pageno, hooverpageno, anchor) ->
        if not (pagevisible layout pageno)
        then (
          match state.layout with
          | [] -> ()
          | l :: _ ->
              state.mode <- Birdseye (
                conf, leftx, l.pageno, hooverpageno, anchor
              )
        );
    | LinkNav (Ltgendir dir as lt) ->
        let linknav =
          let rec loop = function
            | [] -> lt
            | l :: rest ->
                match getopaque l.pageno with
                | None -> loop rest
                | Some opaque ->
                    let link =
                      let ld =
                        if dir = 0
                        then LDfirstvisible (l.pagex, l.pagey, dir)
                        else (
                          if dir > 0 then LDfirst else LDlast
                        )
                      in
                      findlink opaque ld
                    in
                    match link with
                    | Lnotfound -> loop rest
                    | Lfound n ->
                        showlinktype (getlink opaque n);
                        Ltexact (l.pageno, n)
          in
          loop state.layout
        in
        state.mode <- LinkNav linknav
    | _ -> ()
    end;
    preload layout;
  );
  state.ghyll <- noghyll;
  if conf.updatecurs
  then (
    let mx, my = state.mpos in
    updateunder mx my;
  );
;;

let conttiling pageno opaque =
  tilepage pageno opaque
    (if conf.preload then preloadlayout state.y else state.layout)
;;

let gotoy_and_clear_text y =
  if not conf.verbose then state.text <- "";
  gotoy y;
;;

let getanchor1 l =
  let top =
    let coloff = l.pagecol * l.pageh in
    float (l.pagey + coloff) /. float l.pageh
  in
  let dtop =
    if l.pagedispy = 0
    then
      0.0
    else (
      if conf.presentation
      then float l.pagedispy /. float (calcips l.pageh)
      else float l.pagedispy /. float conf.interpagespace
    )
  in
  (l.pageno, top, dtop)
;;

let getanchor () =
  match state.layout with
  | l :: _ -> getanchor1 l
  | []     ->
      let n = page_of_y state.y in
      if n = -1
      then state.anchor
      else
        let y, h = getpageyh n in
        let dy = y - state.y in
        let dtop =
          if conf.presentation
          then
            let ips = calcips h in
            float (dy + ips) /. float ips
          else
            float dy /. float conf.interpagespace
        in
        (n, 0.0, dtop)
;;

let getanchory (n, top, dtop) =
  let y, h = getpageyh n in
  if conf.presentation
  then
    let ips = calcips h in
    y + truncate (top*.float h -. dtop*.float ips) + ips;
  else
    y + truncate (top*.float h -. dtop*.float conf.interpagespace)
;;

let gotoanchor anchor =
  gotoy (getanchory anchor);
;;

let addnav () =
  cbput state.hists.nav (getanchor ());
;;

let getnav dir =
  let anchor = cbgetc state.hists.nav dir in
  getanchory anchor;
;;

let gotoghyll y =
  let scroll f n a b =
    (* http://devmaster.net/forums/topic/9796-ease-in-ease-out-algorithm/ *)
    let snake f a b =
      let s x = 3.0*.x**2.0 -. 2.0*.x**3.0 in
      if f < a
      then s (float f /. float a)
      else (
        if f > b
        then 1.0 -. s ((float (f-b) /. float (n-b)))
        else 1.0
      );
    in
    snake f a b
  and summa f n a b =
    (* courtesy: (calc-eval "integ(3x^2-2x^3,x)") *)
    let iv x = x**3.-.0.5*.x**4. in
    let iv1 = iv f in
    let ins = float a *. iv1
    and outs = float (n-b) *. iv1 in
    let ones = b - a in
    ins +. outs +. float ones
  in
  let rec set (_N, _A, _B) y sy =
    let sum = summa 1.0 _N _A _B in
    let dy = float (y - sy) in
    state.ghyll <- (
      let rec gf n y1 o =
        if n >= _N
        then state.ghyll <- noghyll
        else
          let go n =
            let s = scroll n _N _A _B in
            let y1 = y1 +. ((s *. dy) /. sum) in
            gotoy_and_clear_text (truncate y1);
            state.ghyll <- gf (n+1) y1;
          in
          match o with
          | None -> go n
          | Some y' -> set (_N/2, 1, 1) y' state.y
      in
      gf 0 (float state.y)
    )
  in
  match conf.ghyllscroll with
  | None ->
      gotoy_and_clear_text y
  | Some nab ->
      if state.ghyll == noghyll
      then set nab y state.y
      else state.ghyll (Some y)
;;

let gotopage n top =
  let y, h = getpageyh n in
  let y = y + (truncate (top *. float h)) in
  gotoghyll y
;;

let gotopage1 n top =
  let y = getpagey n in
  let y = y + top in
  gotoghyll y
;;

let invalidate s f =
  state.layout <- [];
  state.pdims <- [];
  state.rects <- [];
  state.rects1 <- [];
  match state.geomcmds with
  | ps, [] when String.length ps = 0 ->
      f ();
      state.geomcmds <- s, [];

  | ps, [] ->
      state.geomcmds <- ps, [s, f];

  | ps, (s', _) :: rest when s' = s ->
      state.geomcmds <- ps, ((s, f) :: rest);

  | ps, cmds ->
      state.geomcmds <- ps, ((s, f) :: cmds);
;;

let flushpages () =
  Hashtbl.iter (fun _ opaque ->
    wcmd "freepage %s" opaque;
  ) state.pagemap;
  Hashtbl.clear state.pagemap;
;;

let flushtiles () =
  if not (Queue.is_empty state.tilelru)
  then (
    Queue.iter (fun (k, p, s) ->
      wcmd "freetile %s" p;
      state.memused <- state.memused - s;
      Hashtbl.remove state.tilemap k;
    ) state.tilelru;
    state.uioh#infochanged Memused;
    Queue.clear state.tilelru;
  );
  load state.layout;
;;

let stateh h =
  let h = truncate (float h*.conf.zoom) in
  let d = conf.interpagespace lsl (if conf.presentation then 1 else 0) in
  h - d
;;

let opendoc path password =
  state.path <- path;
  state.password <- password;
  state.gen <- state.gen + 1;
  state.docinfo <- [];

  flushpages ();
  setaalevel conf.aalevel;
  let titlepath =
    if String.length state.origin = 0
    then path
    else state.origin
  in
  Wsi.settitle ("llpp " ^ (mbtoutf8 (Filename.basename titlepath)));
  wcmd "open %d %s\000%s\000" (btod !wtmode) path password;
  invalidate "reqlayout"
    (fun () ->
      wcmd "reqlayout %d %d %d %s\000"
        conf.angle (int_of_fitmodel conf.fitmodel)
        (stateh state.winh) state.nameddest
    )
;;

let reload () =
  state.anchor <- getanchor ();
  opendoc state.path state.password;
;;

let scalecolor c =
  let c = c *. conf.colorscale in
  (c, c, c);
;;

let scalecolor2 (r, g, b) =
  (r *. conf.colorscale, g *. conf.colorscale, b *. conf.colorscale);
;;

let docolumns = function
  | Csingle _ ->
      let a = Array.make state.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
      let rec loop pageno pdimno pdim y ph pdims =
        if pageno = state.pagecount
        then ()
        else
          let pdimno, ((_, w, h, xoff) as pdim), pdims =
            match pdims with
            | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
                pdimno+1, pdim, rest
            | _ ->
                pdimno, pdim, pdims
          in
          let x = max 0 (((wadjsb state.winw - w) / 2) - xoff) in
          let y = y +
            (if conf.presentation
            then (if pageno = 0 then calcips h else calcips ph + calcips h)
              else (if pageno = 0 then 0 else conf.interpagespace)
            )
          in
          a.(pageno) <- (pdimno, x, y, pdim);
          loop (pageno+1) pdimno pdim (y + h) h pdims
      in
      loop 0 ~-1 (-1,-1,-1,-1) 0 0 state.pdims;
      conf.columns <- Csingle a;

  | Cmulti ((columns, coverA, coverB), _) ->
      let a = Array.make state.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
      let rec loop pageno pdimno pdim x y rowh pdims =
        let rec fixrow m = if m = pageno then () else
            let (pdimno, x, y, ((_, _, h, _) as pdim)) = a.(m) in
            if h < rowh
            then (
              let y = y + (rowh - h) / 2 in
              a.(m) <- (pdimno, x, y, pdim);
            );
            fixrow (m+1)
        in
        if pageno = state.pagecount
        then fixrow (((pageno - 1) / columns) * columns)
        else
          let pdimno, ((_, w, h, xoff) as pdim), pdims =
            match pdims with
            | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
                pdimno+1, pdim, rest
            | _ ->
                pdimno, pdim, pdims
          in
          let x, y, rowh' =
            if pageno = coverA - 1 || pageno = state.pagecount - coverB
            then (
              let x = (wadjsb state.winw - w) / 2 in
              let ips =
                if conf.presentation then calcips h else conf.interpagespace in
              x, y + ips + rowh, h
            )
            else (
              if (pageno - coverA) mod columns = 0
              then (
                let x = max 0 (wadjsb state.winw - state.w) / 2 in
                let y =
                  if conf.presentation
                  then
                    let ips = calcips h in
                    y + (if pageno = 0 then 0 else calcips rowh + ips)
                  else
                    y + (if pageno = 0 then 0 else conf.interpagespace)
                in
                x, y + rowh, h
              )
              else x, y, max rowh h
            )
          in
          let y =
            if pageno > 1 && (pageno - coverA) mod columns = 0
            then (
              let y =
                if pageno = columns && conf.presentation
                then (
                  let ips = calcips rowh in
                  for i = 0 to pred columns
                  do
                    let (pdimno, x, y, pdim) = a.(i) in
                    a.(i) <- (pdimno, x, y+ips, pdim)
                  done;
                  y+ips;
                )
                else y
              in
              fixrow (pageno - columns);
              y
            )
            else y
          in
          a.(pageno) <- (pdimno, x, y, pdim);
          let x = x + w + xoff*2 + conf.interpagespace in
          loop (pageno+1) pdimno pdim x y rowh' pdims
      in
      loop 0 ~-1 (-1,-1,-1,-1) 0 0 0 state.pdims;
      conf.columns <- Cmulti ((columns, coverA, coverB), a);

  | Csplit (c, _) ->
      let a = Array.make (state.pagecount*c) (-1, -1, -1, (-1, -1, -1, -1)) in
      let rec loop pageno pdimno pdim y pdims =
        if pageno = state.pagecount
        then ()
        else
          let pdimno, ((_, w, h, _) as pdim), pdims =
            match pdims with
            | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
                pdimno+1, pdim, rest
            | _ ->
                pdimno, pdim, pdims
          in
          let cw = w / c in
          let rec loop1 n x y =
            if n = c then y else (
              a.(pageno*c + n) <- (pdimno, x, y, pdim);
              loop1 (n+1) (x+cw) (y + h + conf.interpagespace)
            )
          in
          let y = loop1 0 0 y in
          loop (pageno+1) pdimno pdim y pdims
      in
      loop 0 ~-1 (-1,-1,-1,-1) 0 state.pdims;
      conf.columns <- Csplit (c, a);
;;

let represent () =
  docolumns conf.columns;
  state.maxy <- calcheight ();
  if state.reprf == noreprf
  then (
    match state.mode with
    | Birdseye (_, _, pageno, _, _) ->
        let y, h = getpageyh pageno in
        let top = (state.winh - h) / 2 in
        gotoy (max 0 (y - top))
    | _ -> gotoanchor state.anchor
  )
  else (
    state.reprf ();
    state.reprf <- noreprf;
  );
;;

let reshape w h =
  GlDraw.viewport 0 0 w h;
  let firsttime = state.geomcmds == firstgeomcmds in
  if not firsttime && nogeomcmds state.geomcmds
  then state.anchor <- getanchor ();

  state.winw <- w;
  let w = wadjsb (truncate (float w *. conf.zoom)) in
  let w = max w 2 in
  state.winh <- h;
  setfontsize fstate.fontsize;
  GlMat.mode `modelview;
  GlMat.load_identity ();

  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.rotate ~x:1.0 ~angle:180.0 ();
  GlMat.translate ~x:~-.1.0 ~y:~-.1.0 ();
  GlMat.scale3 (2.0 /. float state.winw, 2.0 /. float state.winh, 1.0);

  let relx =
    if conf.zoom <= 1.0
    then 0.0
    else float state.x /. float state.w
  in
  invalidate "geometry"
    (fun () ->
      state.w <- w;
      if not firsttime
      then state.x <- truncate (relx *. float w);
      let w =
        match conf.columns with
        | Csingle _ -> w
        | Cmulti ((c, _, _), _) -> (w - (c-1)*conf.interpagespace) / c
        | Csplit (c, _) -> w * c
      in
      wcmd "geometry %d %d %d"
        w (stateh h) (int_of_fitmodel conf.fitmodel)
    );
;;

let enttext () =
  let len = String.length state.text in
  let drawstring s =
    let hscrollh =
      match state.mode with
      | Textentry _ | View | LinkNav _ ->
          let h, _, _ = state.uioh#scrollpw in
          h
      | _ -> 0
    in
    let rect x w =
      GlDraw.rect
        (x, float (state.winh - (fstate.fontsize + 4) - hscrollh))
        (x+.w, float (state.winh - hscrollh))
    in

    let w = float (wadjsb state.winw - 1) in
    if state.progress >= 0.0 && state.progress < 1.0
    then (
      GlDraw.color (0.3, 0.3, 0.3);
      let w1 = w *. state.progress in
      rect 0.0 w1;
      GlDraw.color (0.0, 0.0, 0.0);
      rect w1 (w-.w1)
    )
    else (
      GlDraw.color (0.0, 0.0, 0.0);
      rect 0.0 w;
    );

    GlDraw.color (1.0, 1.0, 1.0);
    drawstring fstate.fontsize
      (if len > 0 then 8 else 2) (state.winh - hscrollh - 5) s;
  in
  let s =
    match state.mode with
    | Textentry ((prefix, text, _, _, _, _), _) ->
        let s =
          if len > 0
          then
            Printf.sprintf "%s%s_ [%s]" prefix text state.text
          else
            Printf.sprintf "%s%s_"  prefix text
        in
        s

    | _ -> state.text
  in
  let s =
    if state.newerrmsgs
    then (
      if not (istextentry state.mode) && state.uioh#eformsgs
      then
        let s1 = "(press 'e' to review error messasges)" in
        if String.length s > 0 then s ^ " " ^ s1 else s1
      else s
    )
    else s
  in
  if String.length s > 0
  then drawstring s
;;

let gctiles () =
  let len = Queue.length state.tilelru in
  let layout = lazy (
    match state.throttle with
    | None ->
        if conf.preload
        then preloadlayout state.y
        else state.layout
    | Some (layout, _, _) ->
        layout
  ) in
  let rec loop qpos =
    if state.memused <= conf.memlimit
    then ()
    else (
      if qpos < len
      then
        let (k, p, s) as lruitem = Queue.pop state.tilelru in
        let n, gen, colorspace, angle, pagew, pageh, col, row = k in
        let (_, pw, ph, _) = getpagedim n in
        if
          gen = state.gen
          && colorspace = conf.colorspace
          && angle = conf.angle
          && pagew = pw
          && pageh = ph
          && (
            let x = col*conf.tilew
            and y = row*conf.tileh in
            tilevisible (Lazy.force_val layout) n x y
          )
        then Queue.push lruitem state.tilelru
        else (
          freepbo p;
          wcmd "freetile %s" p;
          state.memused <- state.memused - s;
          state.uioh#infochanged Memused;
          Hashtbl.remove state.tilemap k;
        );
        loop (qpos+1)
    )
  in
  loop 0
;;

let logcurrently = function
  | Idle -> dolog "Idle"
  | Loading (l, gen) ->
      dolog "Loading %d gen=%d curgen=%d" l.pageno gen state.gen
  | Tiling (l, pageopaque, colorspace, angle, gen, col, row, tilew, tileh) ->
      dolog
        "Tiling %d[%d,%d] page=%s cs=%s angle"
        l.pageno col row pageopaque
        (colorspace_to_string colorspace)
      ;
      dolog "gen=(%d,%d) (%d,%d) tile=(%d,%d) (%d,%d)"
        angle gen conf.angle state.gen
        tilew tileh
        conf.tilew conf.tileh
      ;
  | Outlining _ ->
      dolog "outlining"
;;

let splitatspace =
  let r = Str.regexp " " in
  fun s -> Str.bounded_split r s 2;
;;

let onpagerect pageno f =
  let b =
    match conf.columns with
    | Cmulti (_, b) -> b
    | Csingle b -> b
    | Csplit (_, b) -> b
  in
  if pageno >= 0 && pageno < Array.length b
  then
    let (pdimno, _, _, (_, _, _, _)) = b.(pageno) in
    let r = getpdimrect pdimno in
    f (r.(1)-.r.(0)) (r.(3)-.r.(2))
;;

let gotopagexy1 pageno x y  =
  onpagerect pageno (fun w h ->
    let top = y /. h in
    let _,w1,_,leftx = getpagedim pageno in
    let wh = state.winh - hscrollh () in
    let sw = float w1 /. w in
    let x = sw *. x in
    let x = leftx + state.x + truncate x in
    let sx =
      if x < 0 || x >= wadjsb state.winw
      then state.x - x
      else state.x
    in
    let py, h = getpageyh pageno in
    let pdy = truncate (top *. float h) in
    let y' = py + pdy in
    let dy = y' - state.y in
    let sy =
      if x != state.x || not (dy > 0 && dy < wh)
      then (
        if conf.presentation
        then
          if abs (py - y') > wh
          then y'
          else py
        else y';
      )
      else state.y
    in
    if state.x != sx || state.y != sy
    then (
      let x, y =
        if !wtmode
        then (
          let ww = wadjsb state.winw in
          let qx = sx / ww
          and qy = pdy / wh in
          let x = qx * ww
          and y = py + qy * wh in
          let x = if -x + ww > w1 then -(w1-ww) else x
          and y' = if y + wh > state.maxy then state.maxy - wh else y in
          let y =
            if conf.presentation
            then
              if abs (py - y') > wh
              then y'
              else py
            else y';
          in
          (x, y)
        )
        else (sx, sy)
      in
      state.x <- x;
      gotoy_and_clear_text y;
    )
    else gotoy_and_clear_text state.y;
  );
;;

let gotopagexy pageno x y =
  match state.mode with
  | Birdseye _ -> gotopage pageno 0.0
  | _ -> gotopagexy1 pageno x y
;;

let act cmds =
  (* dolog "%S" cmds; *)
  let cl = splitatspace cmds in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn ->
      dolog "error processing '%S': %s" cmds (exntos exn);
      exit 1
  in
  match cl with
  | "clear" :: [] ->
      state.uioh#infochanged Pdim;
      state.pdims <- [];

  | "clearrects" :: [] ->
      state.rects <- state.rects1;
      G.postRedisplay "clearrects";

  | "continue" :: args :: [] ->
      let n = scan args "%u" (fun n -> n) in
      state.pagecount <- n;
      begin match state.currently with
      | Outlining l ->
          state.currently <- Idle;
          state.outlines <- Array.of_list (List.rev l)
      | _ -> ()
      end;

      let cur, cmds = state.geomcmds in
      if String.length cur = 0
      then failwith "umpossible";

      begin match List.rev cmds with
      | [] ->
          state.geomcmds <- "", [];
          represent ();
      | (s, f) :: rest ->
          f ();
          state.geomcmds <- s, List.rev rest;
      end;
      if conf.maxwait = None && not !wtmode
      then G.postRedisplay "continue";

  | "title" :: args :: [] ->
      Wsi.settitle args

  | "msg" :: args :: [] ->
      showtext ' ' args

  | "vmsg" :: args :: [] ->
      if conf.verbose
      then showtext ' ' args

  | "emsg" :: args :: [] ->
      Buffer.add_string state.errmsgs args;
      state.newerrmsgs <- true;
      G.postRedisplay "error message"

  | "progress" :: args :: [] ->
      let progress, text =
        scan args "%f %n"
          (fun f pos ->
            f, String.sub args pos (String.length args - pos))
      in
      state.text <- text;
      state.progress <- progress;
      G.postRedisplay "progress"

  | "firstmatch" :: args :: [] ->
      let pageno, c, x0, y0, x1, y1, x2, y2, x3, y3 =
        scan args "%u %d %f %f %f %f %f %f %f %f"
          (fun p c x0 y0 x1 y1 x2 y2 x3 y3 ->
            (p, c, x0, y0, x1, y1, x2, y2, x3, y3))
      in
      let y = (getpagey pageno) + truncate y0 in
      addnav ();
      gotoy y;
      state.rects1 <- [pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)]

  | "match" :: args :: [] ->
      let pageno, c, x0, y0, x1, y1, x2, y2, x3, y3 =
        scan args "%u %d %f %f %f %f %f %f %f %f"
          (fun p c x0 y0 x1 y1 x2 y2 x3 y3 ->
            (p, c, x0, y0, x1, y1, x2, y2, x3, y3))
      in
      state.rects1 <-
        (pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)) :: state.rects1

  | "page" :: args :: [] ->
      let pageopaque, t = scan args "%s %f" (fun p t -> p, t) in
      begin match state.currently with
      | Loading (l, gen) ->
          vlog "page %d took %f sec" l.pageno t;
          Hashtbl.replace state.pagemap (l.pageno, gen) pageopaque;
          begin match state.throttle with
          | None ->
              let preloadedpages =
                if conf.preload
                then preloadlayout state.y
                else state.layout
              in
              let evict () =
                let set =
                  List.fold_left (fun s l -> IntSet.add l.pageno s)
                    IntSet.empty preloadedpages
                in
                let evictedpages =
                  Hashtbl.fold (fun ((pageno, _) as key) opaque accu ->
                    if not (IntSet.mem pageno set)
                    then (
                      wcmd "freepage %s" opaque;
                      key :: accu
                    )
                    else accu
                  ) state.pagemap []
                in
                List.iter (Hashtbl.remove state.pagemap) evictedpages;
              in
              evict ();
              state.currently <- Idle;
              if gen = state.gen
              then (
                tilepage l.pageno pageopaque state.layout;
                load state.layout;
                load preloadedpages;
                if pagevisible state.layout l.pageno
                  && layoutready state.layout
                then G.postRedisplay "page";
              )

          | Some (layout, _, _) ->
              state.currently <- Idle;
              tilepage l.pageno pageopaque layout;
              load state.layout
          end;

      | _ ->
          dolog "Inconsistent loading state";
          logcurrently state.currently;
          exit 1
      end

  | "tile" :: args :: [] ->
      let (x, y, opaque, size, t) =
        scan args "%u %u %s %u %f"
          (fun x y p size t -> (x, y, p, size, t))
      in
      begin match state.currently with
      | Tiling (l, pageopaque, cs, angle, gen, col, row, tilew, tileh) ->
          vlog "tile %d [%d,%d] took %f sec" l.pageno col row t;

          unmappbo opaque;
          if tilew != conf.tilew || tileh != conf.tileh
          then (
            wcmd "freetile %s" opaque;
            state.currently <- Idle;
            load state.layout;
          )
          else  (
            puttileopaque l col row gen cs angle opaque size t;
            state.memused <- state.memused + size;
            state.uioh#infochanged Memused;
            gctiles ();
            Queue.push ((l.pageno, gen, cs, angle, l.pagew, l.pageh, col, row),
                       opaque, size) state.tilelru;

            let layout =
              match state.throttle with
              | None -> state.layout
              | Some (layout, _, _) -> layout
            in

            state.currently <- Idle;
            if   gen = state.gen
              && conf.colorspace = cs
              && conf.angle = angle
              && tilevisible layout l.pageno x y
            then conttiling l.pageno pageopaque;

            begin match state.throttle with
            | None ->
                preload state.layout;
                if   gen = state.gen
                  && conf.colorspace = cs
                  && conf.angle = angle
                  && tilevisible state.layout l.pageno x y
                  && (not !wtmode || layoutready state.layout)
                then G.postRedisplay "tile nothrottle";

            | Some (layout, y, _) ->
                let ready = layoutready layout in
                if ready
                then (
                  state.y <- y;
                  state.layout <- layout;
                  state.throttle <- None;
                  G.postRedisplay "throttle";
                )
                else load layout;
            end;
          );

      | _ ->
          dolog "Inconsistent tiling state";
          logcurrently state.currently;
          exit 1
      end

  | "pdim" :: args :: [] ->
      let (n, w, h, _) as pdim =
        scan args "%u %u %u %u" (fun n w h x -> n, w, h, x)
      in
      let pdim =
        match conf.fitmodel, conf.columns with
        | (FitPage | FitProportional), Csplit _ -> (n, w, h, 0)
        | _ -> pdim
      in
      state.uioh#infochanged Pdim;
      state.pdims <- pdim :: state.pdims

  | "o" :: args :: [] ->
      let (l, n, t, h, pos) =
        scan args "%u %u %d %u %n"
          (fun l n t h pos -> l, n, t, h, pos)
      in
      let s = String.sub args pos (String.length args - pos) in
      let outline = (s, l, (n, float t /. float h, 0.0)) in
      begin match state.currently with
      | Outlining outlines ->
          state.currently <- Outlining (outline :: outlines)
      | Idle ->
          state.currently <- Outlining [outline]
      | currently ->
          dolog "invalid outlining state";
          logcurrently currently
      end

  | "a" :: args :: [] ->
      let (n, l, t) =
        scan args "%u %d %d" (fun n l t -> n, l, t)
      in
      state.reprf <- (fun () -> gotopagexy n (float l) (float t))

  | "info" :: args :: [] ->
      state.docinfo <- (1, args) :: state.docinfo

  | "infoend" :: [] ->
      state.uioh#infochanged Docinfo;
      state.docinfo <- List.rev state.docinfo

  | _ ->
      failwith (Printf.sprintf "unknown cmd `%S'" cmds)
;;

let onhist cb =
  let rc = cb.rc in
  let action = function
    | HCprev  -> cbget cb ~-1
    | HCnext  -> cbget cb 1
    | HCfirst -> cbget cb ~-(cb.rc)
    | HClast  -> cbget cb (cb.len - 1 - cb.rc)
  and cancel () = cb.rc <- rc
  in (action, cancel)
;;

let search pattern forward =
  match conf.columns with
  | Csplit _ ->
      showtext '!' "searching does not work properly in split columns mode"
  | _ ->
      if String.length pattern > 0
      then
        let pn, py =
          match state.layout with
          | [] -> 0, 0
          | l :: _ ->
              l.pageno, (l.pagey + if forward then 0 else 0*l.pagevh)
        in
        wcmd "search %d %d %d %d,%s\000"
          (btod conf.icase) pn py (btod forward) pattern;
;;

let intentry text key =
  let c =
    if key >= 32 && key < 127
    then Char.chr key
    else '\000'
  in
  match c with
  | '0' .. '9' ->
      let text = addchar text c in
      TEcont text

  | _ ->
      state.text <- Printf.sprintf "invalid char (%d, `%c')" key c;
      TEcont text
;;

let linknentry text key =
  let c =
    if key >= 32 && key < 127
    then Char.chr key
    else '\000'
  in
  match c with
  | 'a' .. 'z' ->
      let text = addchar text c in
      TEcont text

  | _ ->
      state.text <- Printf.sprintf "invalid char (%d, `%c')" key c;
      TEcont text
;;

let linkndone f s =
  if String.length s > 0
  then (
    let n =
      let l = String.length s in
      let rec loop pos n = if pos = l then n else
          let m = Char.code s.[pos] - (if pos = 0 && l > 1 then 96 else 97) in
          loop (pos+1) (n*26 + m)
      in loop 0 0
    in
    let rec loop n = function
      | [] -> ()
      | l :: rest ->
          match getopaque l.pageno with
          | None -> loop n rest
          | Some opaque ->
              let m = getlinkcount opaque in
              if n < m
              then (
                let under = getlink opaque n in
                f under
              )
              else loop (n-m) rest
    in
    loop n state.layout;
  )
;;

let textentry text key =
  if key land 0xff00 = 0xff00
  then TEcont text
  else TEcont (text ^ toutf8 key)
;;

let reqlayout angle fitmodel =
  match state.throttle with
  | None ->
      if nogeomcmds state.geomcmds
      then state.anchor <- getanchor ();
      conf.angle <- angle mod 360;
      if conf.angle != 0
      then (
        match state.mode with
        | LinkNav _ -> state.mode <- View
        | _ -> ()
      );
      conf.fitmodel <- fitmodel;
      invalidate "reqlayout"
        (fun () ->
          wcmd "reqlayout %d %d %d"
            conf.angle (int_of_fitmodel conf.fitmodel) (stateh state.winh)
        );
  | _ -> ()
;;

let settrim trimmargins trimfuzz =
  if nogeomcmds state.geomcmds
  then state.anchor <- getanchor ();
  conf.trimmargins <- trimmargins;
  conf.trimfuzz <- trimfuzz;
  let x0, y0, x1, y1 = trimfuzz in
  invalidate "settrim"
    (fun () ->
      wcmd "settrim %d %d %d %d %d" (btod conf.trimmargins) x0 y0 x1 y1);
  flushpages ();
;;

let setzoom zoom =
  match state.throttle with
  | None ->
      let zoom = max 0.0001 zoom in
      if zoom <> conf.zoom
      then (
        state.prevzoom <- conf.zoom;
        conf.zoom <- zoom;
        reshape state.winw state.winh;
        state.text <- Printf.sprintf "zoom is now %-5.2f" (zoom *. 100.0);
      )

  | Some (layout, y, started) ->
      let time =
        match conf.maxwait with
        | None -> 0.0
        | Some t -> t
      in
      let dt = now () -. started in
      if dt > time
      then (
        state.y <- y;
        load layout;
      )
;;

let setcolumns mode columns coverA coverB =
  state.prevcolumns <- Some (conf.columns, conf.zoom);
  if columns < 0
  then (
    if isbirdseye mode
    then showtext '!' "split mode doesn't work in bird's eye"
    else (
      conf.columns <- Csplit (-columns, [||]);
      state.x <- 0;
      conf.zoom <- 1.0;
    );
  )
  else (
    if columns < 2
    then (
      conf.columns <- Csingle [||];
      state.x <- 0;
      setzoom 1.0;
    )
    else (
      conf.columns <- Cmulti ((columns, coverA, coverB), [||]);
      conf.zoom <- 1.0;
    );
  );
  reshape state.winw state.winh;
;;

let enterbirdseye () =
  let zoom = float conf.thumbw /. float state.winw in
  let birdseyepageno =
    let cy = state.winh / 2 in
    let fold = function
      | [] -> 0
      | l :: rest ->
          let rec fold best = function
            | [] -> best.pageno
            | l :: rest ->
                let d = cy - (l.pagedispy + l.pagevh/2)
                and dbest = cy - (best.pagedispy + best.pagevh/2) in
                if abs d < abs dbest
                then fold l rest
                else best.pageno
          in fold l rest
    in
    fold state.layout
  in
  state.mode <- Birdseye (
    { conf with zoom = conf.zoom }, state.x, birdseyepageno, -1, getanchor ()
  );
  conf.zoom <- zoom;
  conf.presentation <- false;
  conf.interpagespace <- 10;
  conf.hlinks <- false;
  conf.fitmodel <- FitProportional;
  state.x <- 0;
  state.mstate <- Mnone;
  conf.maxwait <- None;
  conf.columns <- (
    match conf.beyecolumns with
    | Some c ->
        conf.zoom <- 1.0;
        Cmulti ((c, 0, 0), [||])
    | None -> Csingle [||]
  );
  Wsi.setcursor Wsi.CURSOR_INHERIT;
  if conf.verbose
  then
    state.text <- Printf.sprintf "birds eye mode on (zoom %3.1f%%)"
      (100.0*.zoom)
  else
    state.text <- ""
  ;
  reshape state.winw state.winh;
;;

let leavebirdseye (c, leftx, pageno, _, anchor) goback =
  state.mode <- View;
  conf.zoom <- c.zoom;
  conf.presentation <- c.presentation;
  conf.interpagespace <- c.interpagespace;
  conf.maxwait <- c.maxwait;
  conf.hlinks <- c.hlinks;
  conf.fitmodel <- c.fitmodel;
  conf.beyecolumns <- (
    match conf.columns with
    | Cmulti ((c, _, _), _) -> Some c
    | Csingle _ -> None
    | Csplit _ -> failwith "leaving bird's eye split mode"
  );
  conf.columns <- (
    match c.columns with
    | Cmulti (c, _) -> Cmulti (c, [||])
    | Csingle _ -> Csingle [||]
    | Csplit (c, _) -> Csplit (c, [||])
  );
  state.x <- leftx;
  if conf.verbose
  then
    state.text <- Printf.sprintf "birds eye mode off (zoom %3.1f%%)"
      (100.0*.conf.zoom)
  ;
  reshape state.winw state.winh;
  state.anchor <- if goback then anchor else (pageno, 0.0, 1.0);
;;

let togglebirdseye () =
  match state.mode with
  | Birdseye vals -> leavebirdseye vals true
  | View -> enterbirdseye ()
  | _ -> ()
;;

let upbirdseye incr (conf, leftx, pageno, hooverpageno, anchor) =
  let pageno = max 0 (pageno - incr) in
  let rec loop = function
    | [] -> gotopage1 pageno 0
    | l :: _ when l.pageno = pageno ->
        if l.pagedispy >= 0 && l.pagey = 0
        then G.postRedisplay "upbirdseye"
        else gotopage1 pageno 0
    | _ :: rest -> loop rest
  in
  loop state.layout;
  state.mode <- Birdseye (conf, leftx, pageno, hooverpageno, anchor)
;;

let downbirdseye incr (conf, leftx, pageno, hooverpageno, anchor) =
  let pageno = min (state.pagecount - 1) (pageno + incr) in
  state.mode <- Birdseye (conf, leftx, pageno, hooverpageno, anchor);
  let rec loop = function
    | [] ->
        let y, h = getpageyh pageno in
        let dy = (y - state.y) - (state.winh - h - conf.interpagespace) in
        gotoy (clamp dy)
    | l :: _ when l.pageno = pageno ->
        if l.pagevh != l.pageh
        then gotoy (clamp (l.pageh - l.pagevh + conf.interpagespace))
        else G.postRedisplay "downbirdseye"
    | _ :: rest -> loop rest
  in
  loop state.layout
;;

let optentry mode _ key =
  let btos b = if b then "on" else "off" in
  if key >= 32 && key < 127
  then
    let c = Char.chr key in
    match c with
    | 's' ->
        let ondone s =
          try conf.scrollstep <- int_of_string s with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s (exntos exc)
        in
        TEswitch ("scroll step: ", "", None, intentry, ondone, true)

    | 'A' ->
        let ondone s =
          try
            conf.autoscrollstep <- int_of_string s;
            if state.autoscroll <> None
            then state.autoscroll <- Some conf.autoscrollstep
          with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s (exntos exc)
        in
        TEswitch ("auto scroll step: ", "", None, intentry, ondone, true)

    | 'C' ->
        let ondone s =
          try
            let n, a, b = multicolumns_of_string s in
            setcolumns mode n a b;
          with exc ->
            state.text <- Printf.sprintf "bad columns `%s': %s" s (exntos exc)
        in
        TEswitch ("columns: ", "", None, textentry, ondone, true)

    | 'Z' ->
        let ondone s =
          try
            let zoom = float (int_of_string s) /. 100.0 in
            setzoom zoom
          with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s (exntos exc)
        in
        TEswitch ("zoom: ", "", None, intentry, ondone, true)

    | 't' ->
        let ondone s =
          try
            conf.thumbw <- bound (int_of_string s) 2 4096;
            state.text <-
              Printf.sprintf "thumbnail width is set to %d" conf.thumbw;
            begin match mode with
            | Birdseye beye ->
                leavebirdseye beye false;
                enterbirdseye ();
            | _ -> ();
            end
          with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s (exntos exc)
        in
        TEswitch ("thumbnail width: ", "", None, intentry, ondone, true)

    | 'R' ->
        let ondone s =
          match try
              Some (int_of_string s)
            with exc ->
              state.text <- Printf.sprintf "bad integer `%s': %s"
                s (exntos exc);
              None
          with
          | Some angle -> reqlayout angle conf.fitmodel
          | None -> ()
        in
        TEswitch ("rotation: ", "", None, intentry, ondone, true)

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

    | 'd' ->
        conf.debug <- not conf.debug;
        TEdone ("debug " ^ (btos conf.debug))

    | 'h' ->
        conf.maxhfit <- not conf.maxhfit;
        state.maxy <- calcheight ();
        TEdone ("maxhfit " ^ (btos conf.maxhfit))

    | 'c' ->
        conf.crophack <- not conf.crophack;
        TEdone ("crophack " ^ btos conf.crophack)

    | 'a' ->
        let s =
          match conf.maxwait with
          | None ->
              conf.maxwait <- Some infinity;
              "always wait for page to complete"
          | Some _ ->
              conf.maxwait <- None;
              "show placeholder if page is not ready"
        in
        TEdone s

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
            docolumns conf.columns;
            state.maxy <- calcheight ();
            let y = getpagey pageno in
            gotoy (y + py)
          with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s (exntos exc)
        in
        TEswitch ("vertical margin: ", "", None, intentry, ondone, true)

    | 'l' ->
        let fm =
          match conf.fitmodel with
          | FitProportional -> FitWidth
          | _ -> FitProportional
        in
        reqlayout conf.angle fm;
        TEdone ("proportional display " ^ btos (fm == FitProportional))

    | 'T' ->
        settrim (not conf.trimmargins) conf.trimfuzz;
        TEdone ("trim margins " ^ btos conf.trimmargins)

    | 'I' ->
        conf.invert <- not conf.invert;
        TEdone ("invert colors " ^ btos conf.invert)

    | 'x' ->
        let ondone s =
          cbput state.hists.sel s;
          conf.selcmd <- s;
        in
        TEswitch ("selection command: ", "", Some (onhist state.hists.sel),
                 textentry, ondone, true)

    | 'M' ->
        if conf.pax == None
        then conf.pax <- Some (ref (0.0, 0, 0))
        else conf.pax <- None;
        TEdone ("PAX " ^ btos (conf.pax != None))

    | _ ->
        state.text <- Printf.sprintf "bad option %d `%c'" key c;
        TEstop
  else
    TEcont state.text
;;

class type lvsource = object
  method getitemcount : int
  method getitem : int -> (string * int)
  method hasaction : int -> bool
  method exit :
    uioh:uioh ->
    cancel:bool ->
    active:int ->
    first:int ->
    pan:int ->
    qsearch:string ->
    uioh option
  method getactive : int
  method getfirst : int
  method getqsearch : string
  method setqsearch : string -> unit
  method getpan : int
end;;

class virtual lvsourcebase = object
  val mutable m_active = 0
  val mutable m_first = 0
  val mutable m_qsearch = ""
  val mutable m_pan = 0
  method getactive = m_active
  method getfirst = m_first
  method getqsearch = m_qsearch
  method getpan = m_pan
  method setqsearch s = m_qsearch <- s
end;;

let withoutlastutf8 s =
  let len = String.length s in
  if len = 0
  then s
  else
    let rec find pos =
      if pos = 0
      then pos
      else
        let b = Char.code s.[pos] in
        if b land 0b11000000 = 0b11000000
        then pos
        else find (pos-1)
    in
    let first =
      if Char.code s.[len-1] land 0x80 = 0
      then len-1
      else find (len-1)
    in
    String.sub s 0 first;
;;

let textentrykeyboard
    key _mask ((c, text, opthist, onkey, ondone, cancelonempty), onleave) =
  let key =
    if key >= 0xffb0 && key <= 0xffb9
    then key - 0xffb0 + 48 else key
  in
  let enttext te =
    state.mode <- Textentry (te, onleave);
    state.text <- "";
    enttext ();
    G.postRedisplay "textentrykeyboard enttext";
  in
  let histaction cmd =
    match opthist with
    | None -> ()
    | Some (action, _) ->
        state.mode <- Textentry (
          (c, action cmd, opthist, onkey, ondone, cancelonempty), onleave
        );
        G.postRedisplay "textentry histaction"
  in
  match key with
  | 0xff08 ->                           (* backspace *)
      let s = withoutlastutf8 text in
      let len = String.length s in
      if cancelonempty && len = 0
      then (
        onleave Cancel;
        G.postRedisplay "textentrykeyboard after cancel";
      )
      else (
        enttext (c, s, opthist, onkey, ondone, cancelonempty)
      )

  | 0xff0d | 0xff8d ->                  (* (kp) enter *)
      ondone text;
      onleave Confirm;
      G.postRedisplay "textentrykeyboard after confirm"

  | 0xff52 | 0xff97 -> histaction HCprev (* (kp) up *)
  | 0xff54 | 0xff99 -> histaction HCnext (* (kp) down *)
  | 0xff50 | 0xff95 -> histaction HCfirst (* (kp) home) *)
  | 0xff57 | 0xff9c -> histaction HClast (* (kp) end *)

  | 0xff1b ->                           (* escape*)
      if String.length text = 0
      then (
        begin match opthist with
        | None -> ()
        | Some (_, onhistcancel) -> onhistcancel ()
        end;
        onleave Cancel;
        state.text <- "";
        G.postRedisplay "textentrykeyboard after cancel2"
      )
      else (
        enttext (c, "", opthist, onkey, ondone, cancelonempty)
      )

  | 0xff9f | 0xffff -> ()               (* delete *)

  | _ when key != 0
      && key land 0xff00 != 0xff00      (* keyboard *)
      && key land 0xfe00 != 0xfe00      (* xkb *)
      && key land 0xfd00 != 0xfd00      (* 3270 *)
      ->
      begin match onkey text key with
      | TEdone text ->
          ondone text;
          onleave Confirm;
          G.postRedisplay "textentrykeyboard after confirm2";

      | TEcont text ->
          enttext (c, text, opthist, onkey, ondone, cancelonempty);

      | TEstop ->
          onleave Cancel;
          G.postRedisplay "textentrykeyboard after cancel3"

      | TEswitch te ->
          state.mode <- Textentry (te, onleave);
          G.postRedisplay "textentrykeyboard switch";
      end;

  | _ ->
      vlog "unhandled key %s" (Wsi.keyname key)
;;

let firstof first active =
  if first > active || abs (first - active) > fstate.maxrows - 1
  then max 0 (active - (fstate.maxrows/2))
  else first
;;

let calcfirst first active =
  if active > first
  then
    let rows = active - first in
    if rows > fstate.maxrows then active - fstate.maxrows else first
  else active
;;

let scrollph y maxy =
  let sh = float (maxy + state.winh) /. float state.winh in
  let sh = float state.winh /. sh in
  let sh = max sh (float conf.scrollh) in

  let percent = float y /. float maxy in
  let position = (float state.winh -. sh) *. percent in

  let position =
    if position +. sh > float state.winh
    then float state.winh -. sh
    else position
  in
  position, sh;
;;

let coe s = (s :> uioh);;

class listview ~(source:lvsource) ~trusted ~modehash =
object (self)
  val m_pan = source#getpan
  val m_first = source#getfirst
  val m_active = source#getactive
  val m_qsearch = source#getqsearch
  val m_prev_uioh = state.uioh

  method private elemunder y =
    let n = y / (fstate.fontsize+1) in
    if m_first + n < source#getitemcount
    then (
      if source#hasaction (m_first + n)
      then Some (m_first + n)
      else None
    )
    else None

  method display =
    Gl.enable `blend;
    GlFunc.blend_func `src_alpha `one_minus_src_alpha;
    GlDraw.color (0., 0., 0.) ~alpha:0.85;
    GlDraw.rect (0., 0.) (float state.winw, float state.winh);
    GlDraw.color (1., 1., 1.);
    Gl.enable `texture_2d;
    let fs = fstate.fontsize in
    let nfs = fs + 1 in
    let ww = fstate.wwidth in
    let tabw = 30.0*.ww in
    let itemcount = source#getitemcount in
    let rec loop row =
      if (row - m_first) > fstate.maxrows
      then ()
      else (
        if row >= 0 && row < itemcount
        then (
          let (s, level) = source#getitem row in
          let y = (row - m_first) * nfs in
          let x = 5.0 +. float (level + m_pan) *. ww in
          if row = m_active
          then (
            Gl.disable `texture_2d;
            GlDraw.polygon_mode `both `line;
            let alpha = if source#hasaction row then 0.9 else 0.3 in
            GlDraw.color (1., 1., 1.) ~alpha;
            GlDraw.rect (1., float (y + 1))
              (float (state.winw - conf.scrollbw - 1), float (y + fs + 3));
            GlDraw.polygon_mode `both `fill;
            GlDraw.color (1., 1., 1.);
            Gl.enable `texture_2d;
          );

          let drawtabularstring s =
            let drawstr x s = drawstring1 fs (truncate x) (y+nfs) s in
            if trusted
            then
              let tabpos = try String.index s '\t' with Not_found -> -1 in
              if tabpos > 0
              then
                let len = String.length s - tabpos - 1 in
                let s1 = String.sub s 0 tabpos
                and s2 = String.sub s (tabpos + 1) len in
                let nx = drawstr x s1 in
                let sw = nx -. x in
                let x = x +. (max tabw sw) in
                drawstr x s2
              else
                drawstr x s
            else
              drawstr x s
          in
          let _ = drawtabularstring s in
          loop (row+1)
        )
      )
    in
    loop m_first;
    Gl.disable `blend;
    Gl.disable `texture_2d;

  method updownlevel incr =
    let len = source#getitemcount in
    let curlevel =
      if m_active >= 0 && m_active < len
      then snd (source#getitem m_active)
      else -1
    in
    let rec flow i =
      if i = len then i-1 else if i = -1 then 0 else
          let _, l = source#getitem i in
          if l != curlevel then i else flow (i+incr)
    in
    let active = flow m_active in
    let first = calcfirst m_first active in
    G.postRedisplay "outline updownlevel";
    {< m_active = active; m_first = first >}

  method private key1 key mask =
    let set1 active first qsearch =
      coe {< m_active = active; m_first = first; m_qsearch = qsearch >}
    in
    let search active pattern incr =
      let active = if active = -1 then m_first else active in
      let dosearch re =
        let rec loop n =
          if n >= 0 && n < source#getitemcount
          then (
            let s, _ = source#getitem n in
            if
              (try ignore (Str.search_forward re s 0); true
                with Not_found -> false)
            then Some n
            else loop (n + incr)
          )
          else None
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
    let itemcount = source#getitemcount in
    let find start incr =
      let rec find i =
        if i = -1 || i = itemcount
        then -1
        else (
          if source#hasaction i
          then i
          else find (i + incr)
        )
      in
      find start
    in
    let set active first =
      let first = bound first 0 (itemcount - fstate.maxrows) in
      state.text <- "";
      coe {< m_active = active; m_first = first; m_qsearch = "" >}
    in
    let navigate incr =
      let isvisible first n = n >= first && n - first <= fstate.maxrows in
      let active, first =
        let incr1 = if incr > 0 then 1 else -1 in
        if isvisible m_first m_active
        then
          let next =
            let next = m_active + incr in
            let next =
              if next < 0 || next >= itemcount
              then -1
              else find next incr1
            in
            if abs (m_active - next) > fstate.maxrows
            then -1
            else next
          in
          if next = -1
          then
            let first = m_first + incr in
            let first = bound first 0 (itemcount - fstate.maxrows) in
            let next =
              let next = m_active + incr in
              let next = bound next 0 (itemcount - 1) in
              find next ~-incr1
            in
            let active =
              if next = -1
              then m_active
              else (
                if isvisible first next
                then next
                else m_active
              )
            in
            active, first
          else
            let first = min next m_first in
            let first =
              if abs (next - first) > fstate.maxrows
              then first + incr
              else first
            in
            next, first
        else
          let first = m_first + incr in
          let first = bound first 0 (itemcount - 1) in
          let active =
            let next = m_active + incr in
            let next = bound next 0 (itemcount - 1) in
            let next = find next incr1 in
            let active =
              if next = -1 || abs (m_active - first) > fstate.maxrows
              then (
                let active = if m_active = -1 then next else m_active in
                active
              )
              else next
            in
            if isvisible first active
            then active
            else -1
          in
          active, first
      in
      G.postRedisplay "listview navigate";
      set active first;
    in
    match key with
    | (0x72|0x73) when Wsi.withctrl mask -> (* ctrl-r/ctlr-s *)
        let incr = if key = 0x72 then -1 else 1 in
        let active, first =
          match search (m_active + incr) m_qsearch incr with
          | None ->
              state.text <- m_qsearch ^ " [not found]";
              m_active, m_first
          | Some active ->
              state.text <- m_qsearch;
              active, firstof m_first active
        in
        G.postRedisplay "listview ctrl-r/s";
        set1 active first m_qsearch;

    | 0xff63 when Wsi.withctrl mask ->  (* ctrl-insert *)
        if m_active >= 0 && m_active < source#getitemcount
        then (
          let s, _ = source#getitem m_active in
          selstring s;
        );
        coe self

    | 0xff08 ->                         (* backspace *)
        if String.length m_qsearch = 0
        then coe self
        else (
          let qsearch = withoutlastutf8 m_qsearch in
          let len = String.length qsearch in
          if len = 0
          then (
            state.text <- "";
            G.postRedisplay "listview empty qsearch";
            set1 m_active m_first "";
          )
          else
            let active, first =
              match search m_active qsearch ~-1 with
              | None ->
                  state.text <- qsearch ^ " [not found]";
                  m_active, m_first
              | Some active ->
                  state.text <- qsearch;
                  active, firstof m_first active
            in
            G.postRedisplay "listview backspace qsearch";
            set1 active first qsearch
        );

    | key when (key != 0 && key land 0xff00 != 0xff00) ->
        let pattern = m_qsearch ^ toutf8 key in
        let active, first =
          match search m_active pattern 1 with
          | None ->
              state.text <- pattern ^ " [not found]";
              m_active, m_first
          | Some active ->
              state.text <- pattern;
              active, firstof m_first active
        in
        G.postRedisplay "listview qsearch add";
        set1 active first pattern;

    | 0xff1b ->                         (* escape *)
        state.text <- "";
        if String.length m_qsearch = 0
        then (
          G.postRedisplay "list view escape";
          begin
            match
              source#exit (coe self) true m_active m_first m_pan m_qsearch
            with
            | None -> m_prev_uioh
            | Some uioh -> uioh
          end
        )
        else (
          G.postRedisplay "list view kill qsearch";
          source#setqsearch "";
          coe {< m_qsearch = "" >}
        )

    | 0xff0d | 0xff8d ->                (* (kp) enter *)
        state.text <- "";
        let self = {< m_qsearch = "" >} in
        source#setqsearch "";
        let opt =
          G.postRedisplay "listview enter";
          if m_active >= 0 && m_active < source#getitemcount
          then (
            source#exit (coe self) false m_active m_first m_pan "";
          )
          else (
            source#exit (coe self) true m_active m_first m_pan "";
          );
        in
        begin match opt with
        | None -> m_prev_uioh
        | Some uioh -> uioh
        end

    | 0xff9f | 0xffff ->                (* (kp) delete *)
        coe self

    | 0xff52 | 0xff97 -> navigate ~-1   (* (kp) up *)
    | 0xff54 | 0xff99 -> navigate   1   (* (kp) down *)
    | 0xff55 | 0xff9a -> navigate ~-(fstate.maxrows) (* (kp) prior *)
    | 0xff56 | 0xff9b -> navigate   fstate.maxrows (* (kp) next *)

    | 0xff53 | 0xff98 ->                (* (kp) right *)
        state.text <- "";
        G.postRedisplay "listview right";
        coe {< m_pan = m_pan - 1 >}

    | 0xff51 | 0xff96 ->                (* (kp) left *)
        state.text <- "";
        G.postRedisplay "listview left";
        coe {< m_pan = m_pan + 1 >}

    | 0xff50 | 0xff95 ->                (* (kp) home *)
        let active = find 0 1 in
        G.postRedisplay "listview home";
        set active 0;

    | 0xff57 | 0xff9c ->                (* (kp) end *)
        let first = max 0 (itemcount - fstate.maxrows) in
        let active = find (itemcount - 1) ~-1 in
        G.postRedisplay "listview end";
        set active first;

    | key when (key = 0 || key land 0xff00 = 0xff00) ->
        coe self

    | _ ->
        dolog "listview unknown key %#x" key; coe self

  method key key mask =
    match state.mode with
    | Textentry te -> textentrykeyboard key mask te; coe self
    | _ -> self#key1 key mask

  method button button down x y _ =
    let opt =
      match button with
      | 1 when x > state.winw - conf.scrollbw ->
          G.postRedisplay "listview scroll";
          if down
          then
            let _, position, sh = self#scrollph in
            if y > truncate position && y < truncate (position +. sh)
            then (
              state.mstate <- Mscrolly;
              Some (coe self)
            )
            else
              let s = float (max 0 (y - conf.scrollh)) /. float state.winh in
              let first = truncate (s *. float source#getitemcount) in
              let first = min source#getitemcount first in
              Some (coe {< m_first = first; m_active = first >})
          else (
            state.mstate <- Mnone;
            Some (coe self);
          );
      | 1 when not down ->
          begin match self#elemunder y with
          | Some n ->
              G.postRedisplay "listview click";
              source#exit
                (coe {< m_active = n >}) false n m_first m_pan m_qsearch
          | _ ->
              Some (coe self)
          end
      | n when (n == 4 || n == 5) && not down ->
          let len = source#getitemcount in
          let first =
            if n = 5 && m_first + fstate.maxrows >= len
            then
              m_first
            else
              let first = m_first + (if n == 4 then -1 else 1) in
              bound first 0 (len - 1)
          in
          G.postRedisplay "listview wheel";
          Some (coe {< m_first = first >})
      | n when (n = 6 || n = 7) && not down ->
          let inc = m_first + (if n = 7 then -1 else 1) in
          G.postRedisplay "listview hwheel";
          Some (coe {< m_pan = m_pan + inc >})
      | _ ->
          Some (coe self)
    in
    match opt with
    | None -> m_prev_uioh
    | Some uioh -> uioh

  method motion _ y =
    match state.mstate with
    | Mscrolly ->
        let s = float (max 0 (y - conf.scrollh)) /. float state.winh in
        let first = truncate (s *. float source#getitemcount) in
        let first = min source#getitemcount first in
        G.postRedisplay "listview motion";
        coe {< m_first = first; m_active = first >}
    | _ -> coe self

  method pmotion x y =
    if x < state.winw - conf.scrollbw
    then
      let n =
        match self#elemunder y with
        | None -> Wsi.setcursor Wsi.CURSOR_INHERIT; m_active
        | Some n -> Wsi.setcursor Wsi.CURSOR_INFO; n
      in
      let o =
        if n != m_active
        then (G.postRedisplay "listview pmotion"; {< m_active = n >})
        else self
      in
      coe o
    else (
      Wsi.setcursor Wsi.CURSOR_INHERIT;
      coe self
    )

  method infochanged _ = ()

  method scrollpw = (0, 0.0, 0.0)
  method scrollph =
    let nfs = fstate.fontsize + 1 in
    let y = m_first * nfs in
    let itemcount = source#getitemcount in
    let maxi = max 0 (itemcount - fstate.maxrows) in
    let maxy = maxi * nfs in
    let p, h = scrollph y maxy in
    conf.scrollbw, p, h

  method modehash = modehash
  method eformsgs = false
end;;

class outlinelistview ~source =
object (self)
  inherit listview
    ~source:(source :> lvsource)
    ~trusted:false
    ~modehash:(findkeyhash conf "outline")
    as super

  method key key mask =
    let calcfirst first active =
      if active > first
      then
        let rows = active - first in
        let maxrows =
          if String.length state.text = 0
          then fstate.maxrows
          else fstate.maxrows - 2
        in
        if rows > maxrows then active - maxrows else first
      else active
    in
    let navigate incr =
      let active = m_active + incr in
      let active = bound active 0 (source#getitemcount - 1) in
      let first = calcfirst m_first active in
      G.postRedisplay "outline navigate";
      coe {< m_active = active; m_first = first >}
    in
    let ctrl = Wsi.withctrl mask in
    match key with
    | 110 when ctrl ->                  (* ctrl-n *)
        source#narrow m_qsearch;
        G.postRedisplay "outline ctrl-n";
        coe {< m_first = 0; m_active = 0 >}

    | 117 when ctrl ->                  (* ctrl-u *)
        source#denarrow;
        G.postRedisplay "outline ctrl-u";
        state.text <- "";
        coe {< m_first = 0; m_active = 0 >}

    | 108 when ctrl ->                  (* ctrl-l *)
        let first = max 0 (m_active - (fstate.maxrows / 2)) in
        G.postRedisplay "outline ctrl-l";
        coe {< m_first = first >}

    | 0xff9f | 0xffff ->                (* (kp) delete *)
        source#remove m_active;
        G.postRedisplay "outline delete";
        let active = max 0 (m_active-1) in
        coe {< m_first = firstof m_first active;
               m_active = active >}

    | 0xff52 | 0xff97 -> navigate ~-1   (* (kp) up *)
    | 0xff54 | 0xff99 -> navigate 1     (* (kp) down *)
    | 0xff55 | 0xff9a ->                (* (kp) prior *)
        navigate ~-(fstate.maxrows)
    | 0xff56 | 0xff9b ->                (* (kp) next *)
        navigate fstate.maxrows

    | 0xff53 | 0xff98 ->                (* [ctrl-] (kp) right *)
        let o =
          if ctrl
          then (
            G.postRedisplay "outline ctrl right";
            {< m_pan = m_pan + 1 >}
          )
          else self#updownlevel 1
        in
        coe o

    | 0xff51 | 0xff96 ->                (* [ctrl-] (kp) left *)
        let o =
          if ctrl
          then (
            G.postRedisplay "outline ctrl left";
            {< m_pan = m_pan - 1 >}
          )
          else self#updownlevel ~-1
        in
        coe o

    | 0xff50 | 0xff95 ->                (* (kp) home *)
        G.postRedisplay "outline home";
        coe {< m_first = 0; m_active = 0 >}

    | 0xff57 | 0xff9c ->                (* (kp) end *)
        let active = source#getitemcount - 1 in
        let first = max 0 (active - fstate.maxrows) in
        G.postRedisplay "outline end";
        coe {< m_active = active; m_first = first >}

    | _ -> super#key key mask
end

let outlinesource usebookmarks =
  let empty = [||] in
  (object
    inherit lvsourcebase
    val mutable m_items = empty
    val mutable m_orig_items = empty
    val mutable m_prev_items = empty
    val mutable m_narrow_pattern = ""
    val mutable m_hadremovals = false

    method getitemcount =
      Array.length m_items + (if m_hadremovals then 1 else 0)

    method getitem n =
      if n == Array.length m_items && m_hadremovals
      then
        ("[Confirm removal]", 0)
      else
        let s, n, _ = m_items.(n) in
        (s, n)

    method exit ~uioh ~cancel ~active ~first ~pan ~qsearch =
      ignore (uioh, first, qsearch);
      let confrimremoval = m_hadremovals && active = Array.length m_items in
      let items =
        if String.length m_narrow_pattern = 0
        then m_orig_items
        else m_items
      in
      if not cancel
      then (
        if not confrimremoval
        then(
          let _, _, anchor = m_items.(active) in
          gotoghyll (getanchory anchor);
          m_items <- items;
        )
        else (
          state.bookmarks <- Array.to_list m_items;
          m_orig_items <- m_items;
        )
      )
      else m_items <- items;
      m_pan <- pan;
      None

    method hasaction _ = true

    method greetmsg =
      if Array.length m_items != Array.length m_orig_items
      then "Narrowed to " ^ m_narrow_pattern ^ " (ctrl-u to restore)"
      else ""

    method narrow pattern =
      let reopt = try Some (Str.regexp_case_fold pattern) with _ -> None in
      match reopt with
      | None -> ()
      | Some re ->
          let rec loop accu n =
            if n = -1
            then (
              m_narrow_pattern <- pattern;
              m_items <- Array.of_list accu
            )
            else
              let (s, _, _) as o = m_items.(n) in
              let accu =
                if (try ignore (Str.search_forward re s 0); true
                  with Not_found -> false)
                then o :: accu
                else accu
              in
              loop accu (n-1)
          in
          loop [] (Array.length m_items - 1)

    method denarrow =
      m_orig_items <- (
        if usebookmarks
        then Array.of_list state.bookmarks
        else state.outlines
      );
      m_items <- m_orig_items

    method remove m =
      if usebookmarks
      then
        if m >= 0 && m < Array.length m_items
        then (
          m_hadremovals <- true;
          m_items <- Array.init (Array.length m_items - 1) (fun n ->
            let n = if n >= m then n+1 else n in
            m_items.(n)
          )
        )

    method reset anchor items =
      m_hadremovals <- false;
      if m_orig_items == empty || m_prev_items != items
      then (
        m_orig_items <- items;
        if String.length m_narrow_pattern = 0
        then m_items <- items;
      );
      m_prev_items <- items;
      let rely = getanchory anchor in
      let active =
        let rec loop n best bestd =
          if n = Array.length m_items
          then best
          else
            let (_, _, anchor) = m_items.(n) in
            let orely = getanchory anchor in
            let d = abs (orely - rely) in
            if d < bestd
            then loop (n+1) n d
            else loop (n+1) best bestd
        in
        loop 0 ~-1 max_int
      in
      m_active <- active;
      m_first <- firstof m_first active
  end)
;;

let enterselector usebookmarks =
  let source = outlinesource usebookmarks in
  fun errmsg ->
    let outlines =
      if usebookmarks
      then Array.of_list state.bookmarks
      else state.outlines
    in
    if Array.length outlines = 0
    then (
      showtext ' ' errmsg;
    )
    else (
      state.text <- source#greetmsg;
      Wsi.setcursor Wsi.CURSOR_INHERIT;
      let anchor = getanchor () in
      source#reset anchor outlines;
      state.uioh <- coe (new outlinelistview ~source);
      G.postRedisplay "enter selector";
    )
;;

let enteroutlinemode =
  let f = enterselector false in
  fun ()-> f "Document has no outline";
;;

let enterbookmarkmode =
  let f = enterselector true in
  fun () -> f "Document has no bookmarks (yet)";
;;

let color_of_string s =
  Scanf.sscanf s "%d/%d/%d" (fun r g b ->
    (float r /. 256.0, float g /. 256.0, float b /. 256.0)
  )
;;

let color_to_string (r, g, b) =
  let r = truncate (r *. 256.0)
  and g = truncate (g *. 256.0)
  and b = truncate (b *. 256.0) in
  Printf.sprintf "%d/%d/%d" r g b
;;

let irect_of_string s =
  Scanf.sscanf s "%d/%d/%d/%d" (fun x0 y0 x1 y1 -> (x0,y0,x1,y1))
;;

let irect_to_string (x0,y0,x1,y1) =
  Printf.sprintf "%d/%d/%d/%d" x0 y0 x1 y1
;;

let makecheckers () =
  (* Based on lablGL-1.04/LablGlut/examples/lablGL/checker.ml which had
     following to say:
     converted by Issac Trotts.  July 25, 2002 *)
  let image = GlPix.create `ubyte ~format:`luminance ~width:2 ~height:2 in
  Raw.sets_string (GlPix.to_raw image) ~pos:0 "\255\200\200\255";
  let id = GlTex.gen_texture () in
  GlTex.bind_texture `texture_2d id;
  GlPix.store (`unpack_alignment 1);
  GlTex.image2d image;
  List.iter (GlTex.parameter ~target:`texture_2d)
    [ `mag_filter `nearest; `min_filter `nearest ];
  id;
;;

let setcheckers enabled =
  match state.texid with
  | None ->
      if enabled then state.texid <- Some (makecheckers ())

  | Some texid ->
      if not enabled
      then (
        GlTex.delete_texture texid;
        state.texid <- None;
      );
;;

let int_of_string_with_suffix s =
  let l = String.length s in
  let s1, shift =
    if l > 1
    then
      let suffix = Char.lowercase s.[l-1] in
      match suffix with
      | 'k' -> String.sub s 0 (l-1), 10
      | 'm' -> String.sub s 0 (l-1), 20
      | 'g' -> String.sub s 0 (l-1), 30
      | _ -> s, 0
    else s, 0
  in
  let n = int_of_string s1 in
  let m = n lsl shift in
  if m < 0 || m < n
  then raise (Failure "value too large")
  else m
;;

let string_with_suffix_of_int n =
  if n = 0
  then "0"
  else
    let n, s =
      if n land ((1 lsl 30) - 1) = 0
      then n lsr 30, "G"
      else (
        if n land ((1 lsl 20) - 1) = 0
        then n lsr 20, "M"
        else (
          if n land ((1 lsl 10) - 1) = 0
          then n lsr 10, "K"
          else n, ""
        )
      )
    in
    let rec loop s n =
      let h = n mod 1000 in
      let n = n / 1000 in
      if n = 0
      then string_of_int h ^ s
      else (
        let s = Printf.sprintf "_%03d%s" h s in
        loop s n
      )
    in
    loop "" n ^ s;
;;

let defghyllscroll = (40, 8, 32);;
let ghyllscroll_of_string s =
  let (n, a, b) as nab =
    if s = "default"
    then defghyllscroll
    else Scanf.sscanf s "%u,%u,%u" (fun n a b -> n, a, b)
  in
  if n <= a || n <= b || a >= b
  then failwith "invalid ghyll N,A,B (N <= A, A < B, N <= B)";
  nab;
;;

let ghyllscroll_to_string ((n, a, b) as nab) =
  if nab = defghyllscroll
  then "default"
  else Printf.sprintf "%d,%d,%d" n a b;
;;

let describe_location () =
  let fn = page_of_y state.y in
  let ln = page_of_y (state.y + state.winh - hscrollh () - 1) in
  let maxy = state.maxy - (if conf.maxhfit then state.winh else 0) in
  let percent =
    if maxy <= 0
    then 100.
    else (100. *. (float state.y /. float maxy))
  in
  if fn = ln
  then
    Printf.sprintf "page %d of %d [%.2f%%]"
      (fn+1) state.pagecount percent
  else
    Printf.sprintf
      "pages %d-%d of %d [%.2f%%]"
      (fn+1) (ln+1) state.pagecount percent
;;

let setpresentationmode v =
  let n = page_of_y state.y in
  state.anchor <- (n, 0.0, 1.0);
  conf.presentation <- v;
  if conf.fitmodel = FitPage
  then reqlayout conf.angle conf.fitmodel;
  represent ();
;;

let enterinfomode =
  let btos b = if b then "\xe2\x88\x9a" else "" in
  let showextended = ref false in
  let leave mode = function
    | Confirm -> state.mode <- mode
    | Cancel -> state.mode <- mode in
  let src =
    (object
      val mutable m_first_time = true
      val mutable m_l = []
      val mutable m_a = [||]
      val mutable m_prev_uioh = nouioh
      val mutable m_prev_mode = View

      inherit lvsourcebase

      method reset prev_mode prev_uioh =
        m_a <- Array.of_list (List.rev m_l);
        m_l <- [];
        m_prev_mode <- prev_mode;
        m_prev_uioh <- prev_uioh;
        if m_first_time
        then (
          let rec loop n =
            if n >= Array.length m_a
            then ()
            else
              match m_a.(n) with
              | _, _, _, Action _ -> m_active <- n
              | _ -> loop (n+1)
          in
          loop 0;
          m_first_time <- false;
        )

      method int name get set =
        m_l <-
          (name, `int get, 1, Action (
            fun u ->
              let ondone s =
                try set (int_of_string s)
                with exn ->
                  state.text <- Printf.sprintf "bad integer `%s': %s"
                    s (exntos exn)
              in
              state.text <- "";
              let te = name ^ ": ", "", None, intentry, ondone, true in
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method int_with_suffix name get set =
        m_l <-
          (name, `intws get, 1, Action (
            fun u ->
              let ondone s =
                try set (int_of_string_with_suffix s)
                with exn ->
                  state.text <- Printf.sprintf "bad integer `%s': %s"
                    s (exntos exn)
              in
              state.text <- "";
              let te =
                name ^ ": ", "", None, intentry_with_suffix, ondone, true
              in
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method bool ?(offset=1) ?(btos=btos) name get set =
        m_l <-
          (name, `bool (btos, get), offset, Action (
            fun u ->
              let v = get () in
              set (not v);
              u
          )) :: m_l

      method color name get set =
        m_l <-
          (name, `color get, 1, Action (
            fun u ->
              let invalid = (nan, nan, nan) in
              let ondone s =
                let c =
                  try color_of_string s
                  with exn ->
                    state.text <- Printf.sprintf "bad color `%s': %s"
                      s (exntos exn);
                    invalid
                in
                if c <> invalid
                then set c;
              in
              let te = name ^ ": ", "", None, textentry, ondone, true in
              state.text <- color_to_string (get ());
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method string name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun u ->
              let ondone s = set s in
              let te = name ^ ": ", "", None, textentry, ondone, true in
              state.mode <- Textentry (te, leave m_prev_mode);
              u
          )) :: m_l

      method colorspace name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun _ ->
              let source =
                let vals = [| "rgb"; "bgr"; "gray" |] in
                (object
                  inherit lvsourcebase

                  initializer
                    m_active <- int_of_colorspace conf.colorspace;
                    m_first <- 0;

                  method getitemcount = Array.length vals
                  method getitem n = (vals.(n), 0)
                  method exit ~uioh ~cancel ~active ~first ~pan ~qsearch =
                    ignore (uioh, first, pan, qsearch);
                    if not cancel then set active;
                    None
                  method hasaction _ = true
                end)
              in
              state.text <- "";
              let modehash = findkeyhash conf "info" in
              coe (new listview ~source ~trusted:true ~modehash)
          )) :: m_l

      method roammark name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun _ ->
              let source =
                let vals = [| "page"; "block"; "line"; "word" |] in
                (object
                  inherit lvsourcebase

                  initializer
                    m_active <- int_of_mark conf.paxmark;
                    m_first <- 0;

                  method getitemcount = Array.length vals
                  method getitem n = (vals.(n), 0)
                  method exit ~uioh ~cancel ~active ~first ~pan ~qsearch =
                    ignore (uioh, first, pan, qsearch);
                    if not cancel then set active;
                    None
                  method hasaction _ = true
                end)
              in
              state.text <- "";
              let modehash = findkeyhash conf "info" in
              coe (new listview ~source ~trusted:true ~modehash)
          )) :: m_l

      method fitmodel name get set =
        m_l <-
          (name, `string get, 1, Action (
            fun _ ->
              let source =
                let vals = [| "fit width"; "proportional"; "fit page" |] in
                (object
                  inherit lvsourcebase

                  initializer
                    m_active <- int_of_fitmodel conf.fitmodel;
                    m_first <- 0;

                  method getitemcount = Array.length vals
                  method getitem n = (vals.(n), 0)
                  method exit ~uioh ~cancel ~active ~first ~pan ~qsearch =
                    ignore (uioh, first, pan, qsearch);
                    if not cancel then set active;
                    None
                  method hasaction _ = true
                end)
              in
              state.text <- "";
              let modehash = findkeyhash conf "info" in
              coe (new listview ~source ~trusted:true ~modehash)
          )) :: m_l

      method caption s offset =
        m_l <- (s, `empty, offset, Noaction) :: m_l

      method caption2 s f offset =
        m_l <- (s, `string f, offset, Noaction) :: m_l

      method getitemcount = Array.length m_a

      method getitem n =
        let tostr = function
          | `int f -> string_of_int (f ())
          | `intws f -> string_with_suffix_of_int (f ())
          | `string f -> f ()
          | `color f -> color_to_string (f ())
          | `bool (btos, f) -> btos (f ())
          | `empty -> ""
        in
        let name, t, offset, _ = m_a.(n) in
        ((let s = tostr t in
          if String.length s > 0
          then Printf.sprintf "%s\t%s" name s
          else name),
        offset)

      method exit ~uioh ~cancel ~active ~first ~pan ~qsearch =
        let uiohopt =
          if not cancel
          then (
            m_qsearch <- qsearch;
            let uioh =
              match m_a.(active) with
              | _, _, _, Action f -> f uioh
              | _ -> uioh
            in
            Some uioh
          )
          else None
        in
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        uiohopt

      method hasaction n =
        match m_a.(n) with
        | _, _, _, Action _ -> true
        | _ -> false
    end)
  in
  let rec fillsrc prevmode prevuioh =
    let sep () = src#caption "" 0 in
    let colorp name get set =
      src#string name
        (fun () -> color_to_string (get ()))
        (fun v ->
          try
            let c = color_of_string v in
            set c
          with exn ->
            state.text <- Printf.sprintf "bad color `%s': %s" v (exntos exn)
        )
    in
    let oldmode = state.mode in
    let birdseye = isbirdseye state.mode in

    src#caption (if birdseye then "Setup (Bird's eye)" else "Setup") 0;

    src#bool "presentation mode"
      (fun () -> conf.presentation)
      (fun v -> setpresentationmode v);

    src#bool "ignore case in searches"
      (fun () -> conf.icase)
      (fun v -> conf.icase <- v);

    src#bool "preload"
      (fun () -> conf.preload)
      (fun v -> conf.preload <- v);

    src#bool "highlight links"
      (fun () -> conf.hlinks)
      (fun v -> conf.hlinks <- v);

    src#bool "under info"
      (fun () -> conf.underinfo)
      (fun v -> conf.underinfo <- v);

    src#bool "persistent bookmarks"
      (fun () -> conf.savebmarks)
      (fun v -> conf.savebmarks <- v);

    src#fitmodel "fit model"
      (fun () -> fitmodel_to_string conf.fitmodel)
      (fun v -> reqlayout conf.angle (fitmodel_of_int v));

    src#bool "trim margins"
      (fun () -> conf.trimmargins)
      (fun v -> settrim v conf.trimfuzz; fillsrc prevmode prevuioh);

    src#bool "persistent location"
      (fun () -> conf.jumpback)
      (fun v -> conf.jumpback <- v);

    sep ();
    src#int "inter-page space"
      (fun () -> conf.interpagespace)
      (fun n ->
        conf.interpagespace <- n;
        docolumns conf.columns;
        let pageno, py =
          match state.layout with
          | [] -> 0, 0
          | l :: _ ->
              l.pageno, l.pagey
        in
        state.maxy <- calcheight ();
        let y = getpagey pageno in
        gotoy (y + py)
      );

    src#int "page bias"
      (fun () -> conf.pagebias)
      (fun v -> conf.pagebias <- v);

    src#int "scroll step"
      (fun () -> conf.scrollstep)
      (fun n -> conf.scrollstep <- n);

    src#int "horizontal scroll step"
      (fun () -> conf.hscrollstep)
      (fun v -> conf.hscrollstep <- v);

    src#int "auto scroll step"
      (fun () ->
        match state.autoscroll with
        | Some step -> step
        | _ -> conf.autoscrollstep)
      (fun n ->
        if state.autoscroll <> None
        then state.autoscroll <- Some n;
        conf.autoscrollstep <- n);

    src#int "zoom"
      (fun () -> truncate (conf.zoom *. 100.))
      (fun v -> setzoom ((float v) /. 100.));

    src#int "rotation"
      (fun () -> conf.angle)
      (fun v -> reqlayout v conf.fitmodel);

    src#int "scroll bar width"
      (fun () -> conf.scrollbw)
      (fun v ->
        conf.scrollbw <- v;
        reshape state.winw state.winh;
      );

    src#int "scroll handle height"
      (fun () -> conf.scrollh)
      (fun v -> conf.scrollh <- v;);

    src#int "thumbnail width"
      (fun () -> conf.thumbw)
      (fun v ->
        conf.thumbw <- min 4096 v;
        match oldmode with
        | Birdseye beye ->
            leavebirdseye beye false;
            enterbirdseye ()
        | _ -> ()
      );

    let mode = state.mode in
    src#string "columns"
      (fun () ->
        match conf.columns with
        | Csingle _ -> "1"
        | Cmulti (multi, _) -> multicolumns_to_string multi
        | Csplit (count, _) -> "-" ^ string_of_int count
      )
      (fun v ->
        let n, a, b = multicolumns_of_string v in
        setcolumns mode n a b);

    sep ();
    src#caption "Pixmap cache" 0;
    src#int_with_suffix "size (advisory)"
      (fun () -> conf.memlimit)
      (fun v -> conf.memlimit <- v);

    src#caption2 "used"
      (fun () -> Printf.sprintf "%s bytes, %d tiles"
        (string_with_suffix_of_int state.memused)
        (Hashtbl.length state.tilemap)) 1;

    sep ();
    src#caption "Layout" 0;
    src#caption2 "Dimension"
      (fun () ->
        Printf.sprintf "%dx%d (virtual %dx%d)"
          state.winw state.winh
          state.w state.maxy)
      1;
    if conf.debug
    then
      src#caption2 "Position" (fun () ->
        Printf.sprintf "%dx%d" state.x state.y
      ) 1
    else
      src#caption2 "Position" (fun () -> describe_location ()) 1
    ;

    sep ();
    src#bool ~offset:0 ~btos:(fun v -> if v then "(on)" else "(off)")
      "Save these parameters as global defaults at exit"
      (fun () -> conf.bedefault)
      (fun v -> conf.bedefault <- v)
    ;

    sep ();
    let btos b = if b then "\xc2\xab" else "\xc2\xbb" in
    src#bool ~offset:0 ~btos "Extended parameters"
      (fun () -> !showextended)
      (fun v -> showextended := v; fillsrc prevmode prevuioh);
    if !showextended
    then (
      src#bool "checkers"
        (fun () -> conf.checkers)
        (fun v -> conf.checkers <- v; setcheckers v);
      src#bool "update cursor"
        (fun () -> conf.updatecurs)
        (fun v -> conf.updatecurs <- v);
      src#bool "verbose"
        (fun () -> conf.verbose)
        (fun v -> conf.verbose <- v);
      src#bool "invert colors"
        (fun () -> conf.invert)
        (fun v -> conf.invert <- v);
      src#bool "max fit"
        (fun () -> conf.maxhfit)
        (fun v -> conf.maxhfit <- v);
      src#bool "redirect stderr"
        (fun () -> conf.redirectstderr)
        (fun v -> conf.redirectstderr <- v; redirectstderr ());
      src#bool "pax mode"
        (fun () -> conf.pax != None)
        (fun v ->
          if v
          then conf.pax <- Some (ref (now (), 0, 0))
          else conf.pax <- None);
      src#string "uri launcher"
        (fun () -> conf.urilauncher)
        (fun v -> conf.urilauncher <- v);
      src#string "path launcher"
        (fun () -> conf.pathlauncher)
        (fun v -> conf.pathlauncher <- v);
      src#string "tile size"
        (fun () -> Printf.sprintf "%dx%d" conf.tilew conf.tileh)
        (fun v ->
          try
            let w, h = Scanf.sscanf v "%dx%d" (fun w h -> w, h) in
            conf.tilew <- max 64 w;
            conf.tileh <- max 64 h;
            flushtiles ();
          with exn ->
            state.text <- Printf.sprintf "bad tile size `%s': %s"
              v (exntos exn)
        );
      src#int "texture count"
        (fun () -> conf.texcount)
        (fun v ->
          if realloctexts v
          then conf.texcount <- v
          else showtext '!' " Failed to set texture count please retry later"
        );
      src#int "slice height"
        (fun () -> conf.sliceheight)
        (fun v ->
          conf.sliceheight <- v;
          wcmd "sliceh %d" conf.sliceheight;
        );
      src#int "anti-aliasing level"
        (fun () -> conf.aalevel)
        (fun v ->
          conf.aalevel <- bound v 0 8;
          state.anchor <- getanchor ();
          opendoc state.path state.password;
        );
      src#string "page scroll scaling factor"
        (fun () -> string_of_float conf.pgscale)
        (fun v ->
          try
            let s = float_of_string v in
            conf.pgscale <- s
          with exn ->
            state.text <- Printf.sprintf
              "bad page scroll scaling factor `%s': %s" v (exntos exn)
        )
      ;
      src#int "ui font size"
        (fun () -> fstate.fontsize)
        (fun v -> setfontsize (bound v 5 100));
      src#int "hint font size"
        (fun () -> conf.hfsize)
        (fun v -> conf.hfsize <- bound v 5 100);
      colorp "background color"
        (fun () -> conf.bgcolor)
        (fun v -> conf.bgcolor <- v);
      src#bool "crop hack"
        (fun () -> conf.crophack)
        (fun v -> conf.crophack <- v);
      src#string "trim fuzz"
        (fun () -> irect_to_string conf.trimfuzz)
        (fun v ->
          try
            conf.trimfuzz <- irect_of_string v;
            if conf.trimmargins
            then settrim true conf.trimfuzz;
          with exn ->
            state.text <- Printf.sprintf "bad irect `%s': %s" v (exntos exn)
        );
      src#string "throttle"
        (fun () ->
          match conf.maxwait with
          | None -> "show place holder if page is not ready"
          | Some time ->
              if time = infinity
              then "wait for page to fully render"
              else
                "wait " ^ string_of_float time
                ^ " seconds before showing placeholder"
        )
        (fun v ->
          try
            let f = float_of_string v in
            if f <= 0.0
            then conf.maxwait <- None
            else conf.maxwait <- Some f
          with exn ->
            state.text <- Printf.sprintf "bad time `%s': %s" v (exntos exn)
        );
      src#string "ghyll scroll"
        (fun () ->
          match conf.ghyllscroll with
          | None -> ""
          | Some nab -> ghyllscroll_to_string nab
        )
        (fun v ->
          try
            let gs =
              if String.length v = 0
              then None
              else Some (ghyllscroll_of_string v)
            in
            conf.ghyllscroll <- gs
          with exn ->
            state.text <- Printf.sprintf "bad ghyll `%s': %s" v (exntos exn)
        );
      src#string "selection command"
        (fun () -> conf.selcmd)
        (fun v -> conf.selcmd <- v);
      src#string "synctex command"
        (fun () -> conf.stcmd)
        (fun v -> conf.stcmd <- v);
      src#string "pax command"
        (fun () -> conf.paxcmd)
        (fun v -> conf.paxcmd <- v);
      src#colorspace "color space"
        (fun () -> colorspace_to_string conf.colorspace)
        (fun v ->
          conf.colorspace <- colorspace_of_int v;
          wcmd "cs %d" v;
          load state.layout;
        );
      src#roammark "pax mark method"
        (fun () -> mark_to_string conf.paxmark)
        (fun v -> conf.paxmark <- mark_of_int v);
      if pbousable ()
      then
        src#bool "use PBO"
          (fun () -> conf.usepbo)
          (fun v -> conf.usepbo <- v);
      src#bool "mouse wheel scrolls pages"
        (fun () -> conf.wheelbypage)
        (fun v -> conf.wheelbypage <- v);
      src#bool "open remote links in a new instance"
        (fun () -> conf.riani)
        (fun v -> conf.riani <- v);
    );

    sep ();
    src#caption "Document" 0;
    List.iter (fun (_, s) -> src#caption s 1) state.docinfo;
    src#caption2 "Pages"
      (fun () ->  string_of_int state.pagecount) 1;
    src#caption2 "Dimensions"
      (fun () -> string_of_int (List.length state.pdims)) 1;
    if conf.trimmargins
    then (
      sep ();
      src#caption "Trimmed margins" 0;
      src#caption2 "Dimensions"
        (fun () -> string_of_int (List.length state.pdims)) 1;
    );

    sep ();
    src#caption "OpenGL" 0;
    src#caption (Printf.sprintf "Vendor\t%s" (GlMisc.get_string `vendor)) 1;
    src#caption (Printf.sprintf "Renderer\t%s" (GlMisc.get_string `renderer)) 1;

    sep ();
    src#caption "Location" 0;
    if String.length state.origin > 0
    then src#caption ("Orign\t" ^ mbtoutf8 state.origin) 1;
    src#caption ("Path\t" ^ mbtoutf8 state.path) 1;

    src#reset prevmode prevuioh;
  in
  fun () ->
    state.text <- "";
    let prevmode = state.mode
    and prevuioh = state.uioh in
    fillsrc prevmode prevuioh;
    let source = (src :> lvsource) in
    let modehash = findkeyhash conf "info" in
    state.uioh <- coe (object (self)
      inherit listview ~source ~trusted:true ~modehash as super
      val mutable m_prevmemused = 0
      method infochanged = function
        | Memused ->
            if m_prevmemused != state.memused
            then (
              m_prevmemused <- state.memused;
              G.postRedisplay "memusedchanged";
            )
        | Pdim -> G.postRedisplay "pdimchanged"
        | Docinfo -> fillsrc prevmode prevuioh

      method key key mask =
        if not (Wsi.withctrl mask)
        then
          match key with
          | 0xff51 | 0xff96 -> coe (self#updownlevel ~-1) (* (kp) left *)
          | 0xff53 | 0xff98 -> coe (self#updownlevel 1) (* (kp) right *)
          | _ -> super#key key mask
        else super#key key mask
    end);
    G.postRedisplay "info";
;;

let enterhelpmode =
  let source =
    (object
      inherit lvsourcebase
      method getitemcount = Array.length state.help
      method getitem n =
        let s, l, _ = state.help.(n) in
        (s, l)

      method exit ~uioh ~cancel ~active ~first ~pan ~qsearch =
        let optuioh =
          if not cancel
          then (
            m_qsearch <- qsearch;
            match state.help.(active) with
            | _, _, Action f -> Some (f uioh)
            | _ -> Some (uioh)
          )
          else None
        in
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        optuioh

      method hasaction n =
        match state.help.(n) with
        | _, _, Action _ -> true
        | _ -> false

      initializer
        m_active <- -1
    end)
  in fun () ->
    let modehash = findkeyhash conf "help" in
    state.uioh <- coe (new listview ~source ~trusted:true ~modehash);
    G.postRedisplay "help";
;;

let entermsgsmode =
  let msgsource =
    let re = Str.regexp "[\r\n]" in
    (object
      inherit lvsourcebase
      val mutable m_items = [||]

      method getitemcount = 1 + Array.length m_items

      method getitem n =
        if n = 0
        then "[Clear]", 0
        else m_items.(n-1), 0

      method exit ~uioh ~cancel ~active ~first ~pan ~qsearch =
        ignore uioh;
        if not cancel
        then (
          if active = 0
          then Buffer.clear state.errmsgs;
          m_qsearch <- qsearch;
        );
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        None

      method hasaction n =
        n = 0

      method reset =
        state.newerrmsgs <- false;
        let l = Str.split re (Buffer.contents state.errmsgs) in
        m_items <- Array.of_list l

      initializer
        m_active <- 0
    end)
  in fun () ->
    state.text <- "";
    msgsource#reset;
    let source = (msgsource :> lvsource) in
    let modehash = findkeyhash conf "listview" in
    state.uioh <- coe (object
      inherit listview ~source ~trusted:false ~modehash as super
      method display =
        if state.newerrmsgs
        then msgsource#reset;
        super#display
    end);
    G.postRedisplay "msgs";
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
      state.bookmarks <- (title, 0, getanchor1 l) :: state.bookmarks
;;

let setautoscrollspeed step goingdown =
  let incr = max 1 ((abs step) / 2) in
  let incr = if goingdown then incr else -incr in
  let astep = step + incr in
  state.autoscroll <- Some astep;
;;

let gotounder = function
  | Ulinkgoto (pageno, top) ->
      if pageno >= 0
      then (
        addnav ();
        gotopage1 pageno top;
      )

  | Ulinkuri s ->
      gotouri s

  | Uremote (filename, pageno) ->
      let path =
        if String.length filename > 0
        then
          if Filename.is_relative filename
          then
            let dir = Filename.dirname state.path in
            let dir =
              if Filename.is_implicit dir
              then Filename.concat (Sys.getcwd ()) dir
              else dir
            in
            Filename.concat dir filename
          else filename
        else ""
      in
      let path =
        if Sys.file_exists path
        then path
        else ""
      in
      if String.length path > 0
      then (
        if conf.riani
        then
          let command = !selfexec ^ " " ^ path in
          try popen command []
          with exn ->
            Printf.eprintf
              "failed to execute `%s': %s\n" command (exntos exn);
            flush stderr;
        else
          let anchor = getanchor () in
          let ranchor = state.path, state.password, anchor, state.origin in
          state.origin <- "";
          state.anchor <- (pageno, 0.0, 0.0);
          state.ranchors <- ranchor :: state.ranchors;
          opendoc path "";
      )
      else showtext '!' ("Could not find " ^ filename)

  | Uunexpected _ | Ulaunch _ | Unamed _ | Utext _ | Unone -> ()
;;

let canpan () =
  match conf.columns with
  | Csplit _ -> true
  | _ -> state.x != 0 || conf.zoom > 1.0
;;

let panbound x = bound x (-state.w) (wadjsb state.winw);;

let existsinrow pageno (columns, coverA, coverB) p =
  let last = ((pageno - coverA) mod columns) + columns in
  let rec any = function
    | [] -> false
    | l :: rest ->
        if l.pageno = coverA - 1 || l.pageno = state.pagecount - coverB
        then p l
        else (
          if not (p l)
          then (if l.pageno = last then false else any rest)
          else true
        )
  in
  any state.layout
;;

let nextpage () =
  match state.layout with
  | [] ->
      let pageno = page_of_y state.y in
      gotoghyll (getpagey (pageno+1))
  | l :: rest ->
      match conf.columns with
      | Csingle _ ->
          if conf.presentation && rest == [] && l.pageh > l.pagey + l.pagevh
          then
            let y = clamp (pgscale state.winh) in
            gotoghyll y
          else
            let pageno = min (l.pageno+1) (state.pagecount-1) in
            gotoghyll (getpagey pageno)
      | Cmulti ((c, _, _) as cl, _) ->
          if conf.presentation
            && (existsinrow l.pageno cl
                   (fun l -> l.pageh > l.pagey + l.pagevh))
          then
            let y = clamp (pgscale state.winh) in
            gotoghyll y
          else
            let pageno = min (l.pageno+c) (state.pagecount-1) in
            gotoghyll (getpagey pageno)
      | Csplit (n, _) ->
          if l.pageno < state.pagecount - 1 || l.pagecol < n - 1
          then
            let pagey, pageh = getpageyh l.pageno in
            let pagey = pagey + pageh * l.pagecol in
            let ips = if l.pagecol = 0 then 0 else conf.interpagespace in
            gotoghyll (pagey + pageh + ips)
;;

let prevpage () =
  match state.layout with
  | [] ->
      let pageno = page_of_y state.y in
      gotoghyll (getpagey (pageno-1))
  | l :: _ ->
      match conf.columns with
      | Csingle _ ->
          if conf.presentation && l.pagey != 0
          then
            gotoghyll (clamp (pgscale ~-(state.winh)))
          else
            let pageno = max 0 (l.pageno-1) in
            gotoghyll (getpagey pageno)
      | Cmulti ((c, _, coverB) as cl, _) ->
          if conf.presentation &&
            (existsinrow l.pageno cl (fun l -> l.pagey != 0))
          then
            gotoghyll (clamp (pgscale ~-(state.winh)))
          else
            let decr =
              if l.pageno = state.pagecount - coverB
              then 1
              else c
            in
            let pageno = max 0 (l.pageno-decr) in
            gotoghyll (getpagey pageno)
      | Csplit (n, _) ->
          let y =
            if l.pagecol = 0
            then
              if l.pageno = 0
              then l.pagey
              else
                let pageno = max 0 (l.pageno-1) in
                let pagey, pageh = getpageyh pageno in
                pagey + (n-1)*pageh
            else
              let pagey, pageh = getpageyh l.pageno in
              pagey + pageh * (l.pagecol-1) - conf.interpagespace
          in
          gotoghyll y
;;

let viewkeyboard key mask =
  let enttext te =
    let mode = state.mode in
    state.mode <- Textentry (te, fun _ -> state.mode <- mode);
    state.text <- "";
    enttext ();
    G.postRedisplay "view:enttext"
  in
  let ctrl = Wsi.withctrl mask in
  let key =
    if key >= 0xffb0 && key < 0xffb9 then key - 0xffb0 + 48 else key
  in
  match key with
  | 81 ->                               (* Q *)
      exit 0

  | 0xff63 ->                           (* insert *)
      if conf.angle mod 360 = 0 && not (isbirdseye state.mode)
      then (
        state.mode <- LinkNav (Ltgendir 0);
        gotoy state.y;
      )
      else showtext '!' "Keyboard link navigation does not work under rotation"

  | 0xff1b | 113 ->                     (* escape / q *)
      begin match state.mstate with
      | Mzoomrect _ ->
          state.mstate <- Mnone;
          Wsi.setcursor Wsi.CURSOR_INHERIT;
          G.postRedisplay "kill zoom rect";
      | _ ->
          begin match state.mode with
          | LinkNav _ ->
              state.mode <- View;
              G.postRedisplay "esc leave linknav"
          | _ ->
              match state.ranchors with
              | [] -> raise Quit
              | (path, password, anchor, origin) :: rest ->
                  state.ranchors <- rest;
                  state.anchor <- anchor;
                  state.origin <- origin;
                  opendoc path password
          end;
      end;

  | 0xff08 ->                           (* backspace *)
      gotoghyll (getnav ~-1)

  | 111 ->                              (* o *)
      enteroutlinemode ()

  | 117 ->                              (* u *)
      state.rects <- [];
      state.text <- "";
      G.postRedisplay "dehighlight";

  | 47 | 63 ->                          (* / ? *)
      let ondone isforw s =
        cbput state.hists.pat s;
        state.searchpattern <- s;
        search s isforw
      in
      let s = String.create 1 in
      s.[0] <- Char.chr key;
      enttext (s, "", Some (onhist state.hists.pat),
              textentry, ondone (key = 47), true)

  | 43 | 0xffab | 61 when ctrl ->       (* ctrl-+ or ctrl-= *)
      let incr = if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01 in
      setzoom (conf.zoom +. incr)

  | 43 | 0xffab ->                      (* + *)
      let ondone s =
        let n =
          try int_of_string s with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s (exntos exc);
            max_int
        in
        if n != max_int
        then (
          conf.pagebias <- n;
          state.text <- "page bias is now " ^ string_of_int n;
        )
      in
      enttext ("page bias: ", "", None, intentry, ondone, true)

  | 45 | 0xffad when ctrl ->            (* ctrl-- *)
      let decr = if conf.zoom -. 0.1 < 0.1 then 0.01 else 0.1 in
      setzoom (max 0.01 (conf.zoom -. decr))

  | 45 | 0xffad ->                      (* - *)
      let ondone msg = state.text <- msg in
      enttext (
        "option [acfhilpstvxACFPRSZTISM]: ", "", None,
        optentry state.mode, ondone, true
      )

  | 48 when ctrl ->                     (* ctrl-0 *)
      if conf.zoom = 1.0
      then (
        state.x <- 0;
        gotoy state.y
      )
      else setzoom 1.0

  | (49 | 50) when ctrl && conf.fitmodel != FitPage -> (* ctrl-1/2 *)
      let cols =
        match conf.columns with
        | Csingle _ | Cmulti _ -> 1
        | Csplit (n, _) -> n
      in
      let h = state.winh -
        conf.interpagespace lsl (if conf.presentation then 1 else 0)
      in
      let zoom = zoomforh state.winw h (vscrollw ()) cols in
      if zoom > 0.0 && (key = 50 || zoom < 1.0)
      then setzoom zoom

  | 51 when ctrl ->                     (* ctrl-3 *)
      let fm =
        match conf.fitmodel with
        | FitWidth -> FitProportional
        | FitProportional -> FitPage
        | FitPage -> FitWidth
      in
      state.text <- "fit model: " ^ fitmodel_to_string fm;
      reqlayout conf.angle fm

  | 0xffc6 ->                           (* f9 *)
      togglebirdseye ()

  | 57 when ctrl ->                     (* ctrl-9 *)
      togglebirdseye ()

  | (48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57)
      when not ctrl ->                  (* 0..9 *)
      let ondone s =
        let n =
          try int_of_string s with exc ->
            state.text <- Printf.sprintf "bad integer `%s': %s" s (exntos exc);
            -1
        in
        if n >= 0
        then (
          addnav ();
          cbput state.hists.pag (string_of_int n);
          gotopage1 (n + conf.pagebias - 1) 0;
        )
      in
      let pageentry text key =
        match Char.unsafe_chr key with
        | 'g' -> TEdone text
        | _ -> intentry text key
      in
      let text = "x" in text.[0] <- Char.chr key;
      enttext (":", text, Some (onhist state.hists.pag), pageentry, ondone, true)

  | 98 ->                               (* b *)
      conf.scrollb <- if conf.scrollb = 0 then (scrollbvv lor scrollbhv) else 0;
      reshape state.winw state.winh;

  | 108 ->                              (* l *)
      conf.hlinks <- not conf.hlinks;
      state.text <- "highlightlinks " ^ if conf.hlinks then "on" else "off";
      G.postRedisplay "toggle highlightlinks";

  | 70 ->                               (* F *)
      state.glinks <- true;
      let mode = state.mode in
      state.mode <- Textentry (
        (":", "", None, linknentry, linkndone gotounder, false),
        (fun _ ->
          state.glinks <- false;
          state.mode <- mode)
      );
      state.text <- "";
      G.postRedisplay "view:linkent(F)"

  | 121 ->                              (* y *)
      state.glinks <- true;
      let mode = state.mode in
      state.mode <- Textentry (
        (
          ":", "", None, linknentry, linkndone (fun under ->
            selstring (undertext under);
          ), false
        ),
        fun _ ->
          state.glinks <- false;
          state.mode <- mode
      );
      state.text <- "";
      G.postRedisplay "view:linkent"

  | 97 ->                               (* a *)
      begin match state.autoscroll with
      | Some step ->
          conf.autoscrollstep <- step;
          state.autoscroll <- None
      | None ->
          if conf.autoscrollstep = 0
          then state.autoscroll <- Some 1
          else state.autoscroll <- Some conf.autoscrollstep
      end

  | 112 when ctrl ->                    (* ctrl-p *)
      launchpath ()

  | 80 ->                               (* P *)
      setpresentationmode (not conf.presentation);
      showtext ' ' ("presentation mode " ^
                       if conf.presentation then "on" else "off");

  | 102 ->                              (* f *)
      if List.mem Wsi.Fullscreen state.winstate
      then Wsi.reshape conf.cwinw conf.cwinh
      else Wsi.fullscreen ()

  | 112 | 78 ->                         (* p|N *)
      search state.searchpattern false

  | 110 | 0xffc0 ->                     (* n|F3 *)
      search state.searchpattern true

  | 116 ->                              (* t *)
      begin match state.layout with
      | [] -> ()
      | l :: _ ->
          gotoghyll (getpagey l.pageno)
      end

  | 32 ->                               (* space *)
      nextpage ()

  | 0xff9f | 0xffff ->                  (* delete *)
      prevpage ()

  | 61 ->                               (* = *)
      showtext ' ' (describe_location ());

  | 119 ->                              (* w *)
      begin match state.layout with
      | [] -> ()
      | l :: _ ->
          Wsi.reshape (l.pagew + vscrollw ()) l.pageh;
          G.postRedisplay "w"
      end

  | 39 ->                               (* ' *)
      enterbookmarkmode ()

  | 104 | 0xffbe ->                     (* h|F1 *)
      enterhelpmode ()

  | 105 ->                              (* i *)
      enterinfomode ()

  | 101 when Buffer.length state.errmsgs > 0 -> (* e *)
      entermsgsmode ()

  | 109 ->                              (* m *)
      let ondone s =
        match state.layout with
        | l :: _ ->
            if String.length s > 0
            then
              state.bookmarks <- (s, 0, getanchor1 l) :: state.bookmarks
        | _ -> ()
      in
      enttext ("bookmark: ", "", None, textentry, ondone, true)

  | 126 ->                              (* ~ *)
      quickbookmark ();
      showtext ' ' "Quick bookmark added";

  | 122 ->                              (* z *)
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
          let w = truncate ((float w)*.conf.zoom)
          and h = truncate ((float h)*.conf.zoom) in
          if w != 0 && h != 0
          then (
            state.anchor <- getanchor ();
            Wsi.reshape (w + vscrollw ()) (h + conf.interpagespace)
          );
          G.postRedisplay "z";

      | [] -> ()
      end

  | 120 -> state.roam ()
  | 60 | 62 ->                          (* < > *)
      reqlayout (conf.angle + (if key = 62 then 30 else -30)) conf.fitmodel

  | 91 | 93 ->                          (* [ ] *)
      conf.colorscale <-
        bound (conf.colorscale +. (if key = 93 then 0.1 else -0.1)) 0.0 1.0
      ;
      G.postRedisplay "brightness";

  | 99 when state.mode = View ->        (* [alt]-c *)
      if Wsi.withalt mask
      then (
        if conf.zoom > 1.0
        then
          let m = (wadjsb state.winw - state.w) / 2 in
          state.x <- m;
          gotoy_and_clear_text state.y
      )
      else
        let (c, a, b), z =
          match state.prevcolumns with
          | None -> (1, 0, 0), 1.0
          | Some (columns, z) ->
              let cab =
                match columns with
                | Csplit (c, _) -> -c, 0, 0
                | Cmulti ((c, a, b), _) -> c, a, b
                | Csingle _ -> 1, 0, 0
              in
              cab, z
        in
        setcolumns View c a b;
        setzoom z

  | 0xff54 | 0xff52 when ctrl && Wsi.withshift mask ->
      setzoom state.prevzoom

  | 107 | 0xff52 | 0xff97 ->            (* k (kp) up *)
      begin match state.autoscroll with
      | None ->
          begin match state.mode with
          | Birdseye beye -> upbirdseye 1 beye
          | _ ->
              if ctrl
              then gotoy_and_clear_text (clamp ~-(state.winh/2))
              else (
                if not (Wsi.withshift mask) && conf.presentation
                then prevpage ()
                else gotoy_and_clear_text (clamp (-conf.scrollstep))
              )
          end
      | Some n ->
          setautoscrollspeed n false
      end

  | 106 | 0xff54 | 0xff99 ->            (* j (kp) down *)
      begin match state.autoscroll with
      | None ->
          begin match state.mode with
          | Birdseye beye -> downbirdseye 1 beye
          | _ ->
              if ctrl
              then gotoy_and_clear_text (clamp (state.winh/2))
              else (
                if not (Wsi.withshift mask) && conf.presentation
                then nextpage ()
                else gotoy_and_clear_text (clamp conf.scrollstep)
              )
          end
      | Some n ->
          setautoscrollspeed n true
      end

  | 0xff51 | 0xff53 | 0xff96 | 0xff98
        when not (Wsi.withalt mask) ->  (* (kp) left / right *)
      if canpan ()
      then
        let dx =
          if ctrl
          then state.winw / 2
          else conf.hscrollstep
        in
        let dx = if key = 0xff51 or key = 0xff96 then dx else -dx in
        state.x <- panbound (state.x + dx);
        gotoy_and_clear_text state.y
      else (
        state.text <- "";
        G.postRedisplay "left/right"
      )

  | 0xff55 | 0xff9a ->                  (* (kp) prior *)
      let y =
        if ctrl
        then
          match state.layout with
          | [] -> state.y
          | l :: _ -> state.y - l.pagey
        else
          clamp (pgscale (-state.winh))
      in
      gotoghyll y

  | 0xff56 | 0xff9b ->                  (* (kp) next *)
      let y =
        if ctrl
        then
          match List.rev state.layout with
          | [] -> state.y
          | l :: _ -> getpagey l.pageno
        else
          clamp (pgscale state.winh)
      in
      gotoghyll y

  | 103 | 0xff50 | 0xff95 ->            (* g (kp) home *)
      gotoghyll 0
  | 71 | 0xff57 | 0xff9c ->             (* G (kp) end *)
      gotoghyll (clamp state.maxy)

  | 0xff53 | 0xff98
        when Wsi.withalt mask ->        (* alt-(kp) right *)
      gotoghyll (getnav 1)
  | 0xff51 | 0xff96
        when Wsi.withalt mask ->        (* alt-(kp) left *)
      gotoghyll (getnav ~-1)

  | 114 ->                              (* r *)
      reload ()

  | 118 when conf.debug ->              (* v *)
      state.rects <- [];
      List.iter (fun l ->
        match getopaque l.pageno with
        | None -> ()
        | Some opaque ->
            let x0, y0, x1, y1 = pagebbox opaque in
            let a,b = float x0, float y0 in
            let c,d = float x1, float y0 in
            let e,f = float x1, float y1 in
            let h,j = float x0, float y1 in
            let rect = (a,b,c,d,e,f,h,j) in
            debugrect rect;
            state.rects <- (l.pageno, l.pageno mod 3, rect) :: state.rects;
      ) state.layout;
      G.postRedisplay "v";

  | _ ->
      vlog "huh? %s" (Wsi.keyname key)
;;

let linknavkeyboard key mask linknav =
  let getpage pageno =
    let rec loop = function
      | [] -> None
      | l :: _ when l.pageno = pageno -> Some l
      | _ :: rest -> loop rest
    in loop state.layout
  in
  let doexact (pageno, n) =
    match getopaque pageno, getpage pageno with
    | Some opaque, Some l ->
        if key = 0xff0d || key = 0xff8d (* (kp)enter *)
        then
          let under = getlink opaque n in
          G.postRedisplay "link gotounder";
          gotounder under;
          state.mode <- View;
        else
          let opt, dir =
            match key with
            | 0xff50 ->                 (* home *)
                Some (findlink opaque LDfirst), -1

            | 0xff57 ->                 (* end *)
                Some (findlink opaque LDlast), 1

            | 0xff51 ->                 (* left *)
                Some (findlink opaque (LDleft n)), -1

            | 0xff53 ->                 (* right *)
                Some (findlink opaque (LDright n)), 1

            | 0xff52 ->                 (* up *)
                Some (findlink opaque (LDup n)), -1

            | 0xff54 ->                 (* down *)
                Some (findlink opaque (LDdown n)), 1

            | _ -> None, 0
          in
          let pwl l dir =
            begin match findpwl l.pageno dir with
            | Pwlnotfound -> ()
            | Pwl pageno ->
                let notfound dir =
                  state.mode <- LinkNav (Ltgendir dir);
                  let y, h = getpageyh pageno in
                  let y =
                    if dir < 0
                    then y + h - state.winh
                    else y
                  in
                  gotoy y
                in
                begin match getopaque pageno, getpage pageno with
                | Some opaque, Some _ ->
                    let link =
                      let ld = if dir > 0 then LDfirst else LDlast in
                      findlink opaque ld
                    in
                    begin match link with
                    | Lfound m ->
                        showlinktype (getlink opaque m);
                        state.mode <- LinkNav (Ltexact (pageno, m));
                        G.postRedisplay "linknav jpage";
                    | _ -> notfound dir
                    end;
                | _ -> notfound dir
                end;
            end;
          in
          begin match opt with
          | Some Lnotfound -> pwl l dir;
          | Some (Lfound m) ->
              if m = n
              then pwl l dir
              else (
                let _, y0, _, y1 = getlinkrect opaque m in
                if y0 < l.pagey
                then gotopage1 l.pageno y0
                else (
                  let d = fstate.fontsize + 1 in
                  if y1 - l.pagey > l.pagevh - d
                  then gotopage1 l.pageno (y1 - state.winh - hscrollh () + d)
                  else G.postRedisplay "linknav";
                );
                showlinktype (getlink opaque m);
                state.mode <- LinkNav (Ltexact (l.pageno, m));
              )

          | None -> viewkeyboard key mask
          end;
    | _ -> viewkeyboard key mask
  in
  if key = 0xff63
  then (
    state.mode <- View;
    G.postRedisplay "leave linknav"
  )
  else
    match linknav with
    | Ltgendir _ -> viewkeyboard key mask
    | Ltexact exact -> doexact exact
;;

let keyboard key mask =
  if (key = 103 && Wsi.withctrl mask) && not (istextentry state.mode)
  then wcmd "interrupt"
  else state.uioh <- state.uioh#key key mask
;;

let birdseyekeyboard key mask
    ((oconf, leftx, pageno, hooverpageno, anchor) as beye) =
  let incr =
    match conf.columns with
    | Csingle _ -> 1
    | Cmulti ((c, _, _), _) -> c
    | Csplit _ -> failwith "bird's eye split mode"
  in
  let pgh layout = List.fold_left (fun m l -> max l.pageh m) state.winh layout in
  match key with
  | 108 when Wsi.withctrl mask ->      (* ctrl-l *)
      let y, h = getpageyh pageno in
      let top = (state.winh - h) / 2 in
      gotoy (max 0 (y - top))
  | 0xff0d                              (* enter *)
  | 0xff8d -> leavebirdseye beye false  (* kp enter *)
  | 0xff1b -> leavebirdseye beye true   (* escape *)
  | 0xff52 -> upbirdseye incr beye      (* up *)
  | 0xff54 -> downbirdseye incr beye    (* down *)
  | 0xff51 -> upbirdseye 1 beye         (* left *)
  | 0xff53 -> downbirdseye 1 beye       (* right *)

  | 0xff55 ->                           (* prior *)
      begin match state.layout with
      | l :: _ ->
          if l.pagey != 0
          then (
            state.mode <- Birdseye (
              oconf, leftx, l.pageno, hooverpageno, anchor
            );
            gotopage1 l.pageno 0;
          )
          else (
            let layout = layout (state.y-state.winh) (pgh state.layout) in
            match layout with
            | [] -> gotoy (clamp (-state.winh))
            | l :: _ ->
                state.mode <- Birdseye (
                  oconf, leftx, l.pageno, hooverpageno, anchor
                );
                gotopage1 l.pageno 0
          );

      | [] -> gotoy (clamp (-state.winh))
      end;

  | 0xff56 ->                           (* next *)
      begin match List.rev state.layout with
      | l :: _ ->
          let layout = layout (state.y + (pgh state.layout)) state.winh in
          begin match layout with
          | [] ->
              let incr = l.pageh - l.pagevh in
              if incr = 0
              then (
                state.mode <-
                  Birdseye (
                    oconf, leftx, state.pagecount - 1, hooverpageno, anchor
                  );
                G.postRedisplay "birdseye pagedown";
              )
              else gotoy (clamp (incr + conf.interpagespace*2));

          | l :: _ ->
              state.mode <-
                Birdseye (oconf, leftx, l.pageno, hooverpageno, anchor);
              gotopage1 l.pageno 0;
          end

      | [] -> gotoy (clamp state.winh)
      end;

  | 0xff50 ->                           (* home *)
      state.mode <- Birdseye (oconf, leftx, 0, hooverpageno, anchor);
      gotopage1 0 0

  | 0xff57 ->                           (* end *)
      let pageno = state.pagecount - 1 in
      state.mode <- Birdseye (oconf, leftx, pageno, hooverpageno, anchor);
      if not (pagevisible state.layout pageno)
      then
        let h =
          match List.rev state.pdims with
          | [] -> state.winh
          | (_, _, h, _) :: _ -> h
        in
        gotoy (max 0 (getpagey pageno - (state.winh - h - conf.interpagespace)))
      else G.postRedisplay "birdseye end";
  | _ -> viewkeyboard key mask
;;

let drawpage l =
  let color =
    match state.mode with
    | Textentry _ -> scalecolor 0.4
    | LinkNav _
    | View -> scalecolor 1.0
    | Birdseye (_, _, pageno, hooverpageno, _) ->
        if l.pageno = hooverpageno
        then scalecolor 0.9
        else (
          if l.pageno = pageno
          then scalecolor 1.0
          else scalecolor 0.8
        )
  in
  drawtiles l color;
;;

let postdrawpage l linkindexbase =
  match getopaque l.pageno with
  | Some opaque ->
      if tileready l l.pagex l.pagey
      then
        let x = l.pagedispx - l.pagex
        and y = l.pagedispy - l.pagey in
        let hlmask =
          match conf.columns with
          | Csingle _ | Cmulti _ ->
              (if conf.hlinks then 1 else 0)
              + (if state.glinks
                  && not (isbirdseye state.mode) then 2 else 0)
          | _ -> 0
        in
        let s =
          match state.mode with
          | Textentry ((_, s, _, _, _, _), _) when state.glinks -> s
          | _ -> ""
        in
        postprocess opaque hlmask x y (linkindexbase, s, conf.hfsize);
      else 0
  | _ -> 0
;;

let scrollindicator () =
  let sbw, ph, sh = state.uioh#scrollph in
  let sbh, pw, sw = state.uioh#scrollpw in

  GlDraw.color (0.64, 0.64, 0.64);
  GlDraw.rect
    (float (state.winw - sbw), 0.)
    (float state.winw, float state.winh)
  ;
  GlDraw.rect
    (0., float (state.winh - sbh))
    (float (wadjsb state.winw - 1), float state.winh)
  ;
  GlDraw.color (0.0, 0.0, 0.0);

  GlDraw.rect
    (float (state.winw - sbw), ph)
    (float state.winw, ph +. sh)
  ;
  GlDraw.rect
    (pw, float (state.winh - sbh))
    (pw +. sw, float state.winh)
  ;
;;

let showsel () =
  match state.mstate with
  | Mnone | Mscrolly | Mscrollx | Mpan _ | Mzoom _ | Mzoomrect _ ->
      ()

  | Msel ((x0, y0), (x1, y1)) ->
      let rec loop = function
        | l :: ls ->
            if ((y0 >= l.pagedispy && y0 <= (l.pagedispy + l.pagevh))
                 || ((y1 >= l.pagedispy && y1 <= (l.pagedispy + l.pagevh))))
              && ((x0 >= l.pagedispx && x0 <= (l.pagedispx + l.pagevw))
                   || ((x1 >= l.pagedispx && x1 <= (l.pagedispx + l.pagevw))))
            then
              match getopaque l.pageno with
              | Some opaque ->
                  let x0, y0 = pagetranslatepoint l x0 y0 in
                  let x1, y1 = pagetranslatepoint l x1 y1 in
                  seltext opaque (x0, y0, x1, y1);
              | _ -> ()
            else loop ls
        | [] -> ()
      in
      loop state.layout
;;

let showrects rects =
  Gl.enable `blend;
  GlDraw.color (0.0, 0.0, 1.0) ~alpha:0.5;
  GlDraw.polygon_mode `both `fill;
  GlFunc.blend_func `src_alpha `one_minus_src_alpha;
  List.iter
    (fun (pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)) ->
      List.iter (fun l ->
        if l.pageno = pageno
        then (
          let dx = float (l.pagedispx - l.pagex) in
          let dy = float (l.pagedispy - l.pagey) in
          GlDraw.color (0.0, 0.0, 1.0 /. float c) ~alpha:0.5;
          GlDraw.begins `quads;
          (
            GlDraw.vertex2 (x0+.dx, y0+.dy);
            GlDraw.vertex2 (x1+.dx, y1+.dy);
            GlDraw.vertex2 (x2+.dx, y2+.dy);
            GlDraw.vertex2 (x3+.dx, y3+.dy);
          );
          GlDraw.ends ();
        )
      ) state.layout
    ) rects
  ;
  Gl.disable `blend;
;;

let display () =
  GlClear.color (scalecolor2 conf.bgcolor);
  GlClear.clear [`color];
  List.iter drawpage state.layout;
  let rects =
    match state.mode with
    | LinkNav (Ltexact (pageno, linkno)) ->
        begin match getopaque pageno with
        | Some opaque ->
            let x0, y0, x1, y1 = getlinkrect opaque linkno in
            (pageno, 5, (
              float x0, float y0,
              float x1, float y0,
              float x1, float y1,
              float x0, float y1)
            ) :: state.rects
        | None -> state.rects
        end
    | _ -> state.rects
  in
  showrects rects;
  let rec postloop linkindexbase = function
    | l :: rest ->
        let linkindexbase = linkindexbase + postdrawpage l linkindexbase in
        postloop linkindexbase rest
    | [] -> ()
  in
  showsel ();
  postloop 0 state.layout;
  state.uioh#display;
  begin match state.mstate with
  | Mzoomrect ((x0, y0), (x1, y1)) ->
      Gl.enable `blend;
      GlDraw.color (0.3, 0.3, 0.3) ~alpha:0.5;
      GlFunc.blend_func `src_alpha `one_minus_src_alpha;
      GlDraw.rect (float x0, float y0)
        (float x1, float y1);
      Gl.disable `blend;
  | _ -> ()
  end;
  enttext ();
  scrollindicator ();
  Wsi.swapb ();
;;

let zoomrect x y x1 y1 =
  let x0 = min x x1
  and x1 = max x x1
  and y0 = min y y1 in
  gotoy (state.y + y0);
  state.anchor <- getanchor ();
  let zoom = (float state.w) /. float (x1 - x0) in
  let margin =
    match conf.fitmodel, conf.columns with
    | FitPage, Csplit _ ->
        onppundermouse (fun _ l _ _ -> Some l.pagedispx) x0 y0 x0

    | _, _ ->
        let adjw = wadjsb state.winw in
        if state.w < adjw
        then (adjw - state.w) / 2
        else 0
  in
  state.x <- (state.x + margin) - x0;
  setzoom zoom;
  Wsi.setcursor Wsi.CURSOR_INHERIT;
  state.mstate <- Mnone;
;;

let scrollx x =
  let winw = wadjsb state.winw - 1 in
  let s = float x /. float winw in
  let destx = truncate (float (state.w + winw) *. s) in
  state.x <- winw - destx;
  gotoy_and_clear_text state.y;
  state.mstate <- Mscrollx;
;;

let scrolly y =
  let s = float y /. float state.winh in
  let desty = truncate (float (state.maxy - state.winh) *. s) in
  gotoy_and_clear_text desty;
  state.mstate <- Mscrolly;
;;

let viewmouse button down x y mask =
  match button with
  | n when (n == 4 || n == 5) && not down ->
      if Wsi.withctrl mask
      then (
        match state.mstate with
        | Mzoom (oldn, i) ->
            if oldn = n
            then (
              if i = 2
              then
                let incr =
                  match n with
                  | 5 ->
                      if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01
                  | _ ->
                      if conf.zoom -. 0.1 < 0.1 then -0.01 else -0.1
                in
                let zoom = conf.zoom -. incr in
                setzoom zoom;
                state.mstate <- Mzoom (n, 0);
              else
                state.mstate <- Mzoom (n, i+1);
            )
            else state.mstate <- Mzoom (n, 0)

        | _ -> state.mstate <- Mzoom (n, 0)
      )
      else (
        match state.autoscroll with
        | Some step -> setautoscrollspeed step (n=4)
        | None ->
            if conf.wheelbypage || conf.presentation
            then (
              if n = 4
              then prevpage ()
              else nextpage ()
            )
            else
              let incr =
                if n = 4
                then -conf.scrollstep
                else conf.scrollstep
              in
              let incr = incr * 2 in
              let y = clamp incr in
              gotoy_and_clear_text y
      )

  | n when (n = 6 || n = 7) && not down && canpan () ->
      state.x <-
        panbound (state.x + (if n = 7 then -2 else 2) * conf.hscrollstep);
      gotoy_and_clear_text state.y

  | 1 when Wsi.withshift mask ->
      state.mstate <- Mnone;
      if not down
      then (
        match unproject x y with
        | Some (pageno, ux, uy) ->
            let cmd = Printf.sprintf
              "%s %s %d %d %d"
              conf.stcmd state.path pageno ux uy
            in
            popen cmd []
        | None -> ()
      )

  | 1 when Wsi.withctrl mask ->
      if down
      then (
        Wsi.setcursor Wsi.CURSOR_CROSSHAIR;
        state.mstate <- Mpan (x, y)
      )
      else
        state.mstate <- Mnone

  | 3 ->
      if down
      then (
        Wsi.setcursor Wsi.CURSOR_CYCLE;
        let p = (x, y) in
        state.mstate <- Mzoomrect (p, p)
      )
      else (
        match state.mstate with
        | Mzoomrect ((x0, y0), _) ->
            if abs (x-x0) > 10 && abs (y - y0) > 10
            then zoomrect x0 y0 x y
            else (
              state.mstate <- Mnone;
              Wsi.setcursor Wsi.CURSOR_INHERIT;
              G.postRedisplay "kill accidental zoom rect";
            )
        | _ ->
            Wsi.setcursor Wsi.CURSOR_INHERIT;
            state.mstate <- Mnone
      )

  | 1 when x > state.winw - vscrollw () ->
      if down
      then
        let _, position, sh = state.uioh#scrollph in
        if y > truncate position && y < truncate (position +. sh)
        then state.mstate <- Mscrolly
        else scrolly y
      else
        state.mstate <- Mnone

  | 1 when y > state.winh - hscrollh () ->
      if down
      then
        let _, position, sw = state.uioh#scrollpw in
        if x > truncate position && x < truncate (position +. sw)
        then state.mstate <- Mscrollx
        else scrollx x
      else
        state.mstate <- Mnone

  | 1 ->
      let dest = if down then getunder x y else Unone in
      begin match dest with
      | Ulinkgoto _
      | Ulinkuri _
      | Uremote _
      | Uunexpected _ | Ulaunch _ | Unamed _ ->
          gotounder dest

      | Unone when down ->
          Wsi.setcursor Wsi.CURSOR_CROSSHAIR;
          state.mstate <- Mpan (x, y);

      | Unone | Utext _ ->
          if down
          then (
            if conf.angle mod 360 = 0
            then (
              state.mstate <- Msel ((x, y), (x, y));
              G.postRedisplay "mouse select";
            )
          )
          else (
            match state.mstate with
            | Mnone -> ()

            | Mzoom _ | Mscrollx | Mscrolly ->
                state.mstate <- Mnone

            | Mzoomrect ((x0, y0), _) ->
                zoomrect x0 y0 x y

            | Mpan _ ->
                Wsi.setcursor Wsi.CURSOR_INHERIT;
                state.mstate <- Mnone

            | Msel ((x0, y0), (x1, y1)) ->
                let rec loop = function
                  | [] -> ()
                  | l :: rest ->
                      let inside =
                        let a0 = l.pagedispy in
                        let a1 = a0 + l.pagevh in
                        let b0 = l.pagedispx in
                        let b1 = b0 + l.pagevw in
                        ((y0 >= a0 && y0 <= a1) || (y1 >= a0 && y1 <= a1))
                        && ((x0 >= b0 && x0 <= b1) || (x1 >= b0 && x1 <= b1))
                      in
                      if inside
                      then
                        match getopaque l.pageno with
                        | Some opaque ->
                            begin
                              match Ne.pipe () with
                              | Ne.Exn exn ->
                                  showtext '!'
                                    (Printf.sprintf
                                        "can not create sel pipe: %s"
                                        (exntos exn));
                              | Ne.Res (r, w) ->
                                  let doclose what fd =
                                    Ne.clo fd (fun msg ->
                                      dolog "%s close failed: %s" what msg)
                                  in
                                  try
                                    popen conf.selcmd [r, 0; w, -1];
                                    copysel w opaque true;
                                    doclose "pipe/r" r;
                                    G.postRedisplay "copysel";
                                  with exn ->
                                    dolog "can not execute %S: %s"
                                      conf.selcmd (exntos exn);
                                    doclose "pipe/r" r;
                                    doclose "pipe/w" w;
                            end
                        | None -> ()
                      else loop rest
                in
                loop state.layout;
                Wsi.setcursor Wsi.CURSOR_INHERIT;
                state.mstate <- Mnone;
          )
      end

  | _ -> ()
;;

let birdseyemouse button down x y mask
    (conf, leftx, _, hooverpageno, anchor) =
  match button with
  | 1 when down ->
      let rec loop = function
        | [] -> ()
        | l :: rest ->
            if y > l.pagedispy && y < l.pagedispy + l.pagevh
              && x > l.pagedispx && x < l.pagedispx + l.pagevw
            then (
              leavebirdseye (conf, leftx, l.pageno, hooverpageno, anchor) false;
            )
            else loop rest
      in
      loop state.layout
  | 3 -> ()
  | _ -> viewmouse button down x y mask
;;

let mouse button down x y mask =
  state.uioh <- state.uioh#button button down x y mask;
;;

let motion ~x ~y =
  state.uioh <- state.uioh#motion x y
;;

let pmotion ~x ~y =
  state.uioh <- state.uioh#pmotion x y;
;;

let uioh = object
  method display = ()

  method key key mask =
    begin match state.mode with
    | Textentry textentry -> textentrykeyboard key mask textentry
    | Birdseye birdseye -> birdseyekeyboard key mask birdseye
    | View -> viewkeyboard key mask
    | LinkNav linknav -> linknavkeyboard key mask linknav
    end;
    state.uioh

  method button button bstate x y mask =
    begin match state.mode with
    | LinkNav _
    | View -> viewmouse button bstate x y mask
    | Birdseye beye -> birdseyemouse button bstate x y mask beye
    | Textentry _ -> ()
    end;
    state.uioh

  method motion x y =
    begin match state.mode with
    | Textentry _ -> ()
    | View | Birdseye _ | LinkNav _ ->
        match state.mstate with
        | Mzoom _ | Mnone -> ()

        | Mpan (x0, y0) ->
            let dx = x - x0
            and dy = y0 - y in
            state.mstate <- Mpan (x, y);
            if canpan ()
            then state.x <- panbound (state.x + dx);
            let y = clamp dy in
            gotoy_and_clear_text y

        | Msel (a, _) ->
            state.mstate <- Msel (a, (x, y));
            G.postRedisplay "motion select";

        | Mscrolly ->
            let y = min state.winh (max 0 y) in
            scrolly y

        | Mscrollx ->
            let x = min state.winw (max 0 x) in
            scrollx x

        | Mzoomrect (p0, _) ->
            state.mstate <- Mzoomrect (p0, (x, y));
            G.postRedisplay "motion zoomrect";
    end;
    state.uioh

  method pmotion x y =
    begin match state.mode with
    | Birdseye (conf, leftx, pageno, hooverpageno, anchor) ->
        let rec loop = function
          | [] ->
              if hooverpageno != -1
              then (
                state.mode <- Birdseye (conf, leftx, pageno, -1, anchor);
                G.postRedisplay "pmotion birdseye no hoover";
              )
          | l :: rest ->
              if y > l.pagedispy && y < l.pagedispy + l.pagevh
                && x > l.pagedispx && x < l.pagedispx + l.pagevw
              then (
                state.mode <- Birdseye (conf, leftx, pageno, l.pageno, anchor);
                G.postRedisplay "pmotion birdseye hoover";
              )
              else loop rest
        in
        loop state.layout

    | Textentry _ -> ()

    | LinkNav _
    | View ->
        match state.mstate with
        | Mpan _ | Msel _ | Mzoom _ | Mscrolly | Mscrollx | Mzoomrect _ ->
            ()
        | Mnone ->
            updateunder x y;
            match conf.pax with
            | None -> ()
            | Some r ->
                let past, _, _ = !r in
                let now = now () in
                let delta = now -. past in
                if delta > 0.01
                then paxunder x y
                else r := (now, x, y)
    end;
    state.uioh

  method infochanged _ = ()

  method scrollph =
    let maxy = state.maxy - (if conf.maxhfit then state.winh else 0) in
    let p, h =
      if maxy = 0
      then 0.0, float state.winh
      else scrollph state.y maxy
    in
    vscrollw (), p, h

  method scrollpw =
    let winw = wadjsb state.winw in
    let fwinw = float winw in
    let sw =
      let sw = fwinw /. float state.w in
      let sw = fwinw *. sw in
      max sw (float conf.scrollh)
    in
    let position =
      let maxx = state.w + winw in
      let x = winw - state.x in
      let percent = float x /. float maxx in
      (fwinw -. sw) *. percent
    in
    hscrollh (), position, sw

  method modehash =
    let modename =
      match state.mode with
      | LinkNav _ -> "links"
      | Textentry _ -> "textentry"
      | Birdseye _ -> "birdseye"
      | View -> "view"
    in
    findkeyhash conf modename

  method eformsgs = true
end;;

module Config =
struct
  open Parser

  let fontpath = ref "";;

  module KeyMap =
    Map.Make (struct type t = (int * int) let compare = compare end);;

  let unent s =
    let l = String.length s in
    let b = Buffer.create l in
    unent b s 0 l;
    Buffer.contents b;
  ;;

  let home =
    try Sys.getenv "HOME"
    with exn ->
      prerr_endline
        ("Can not determine home directory location: " ^ exntos exn);
      ""
  ;;

  let modifier_of_string = function
    | "alt" -> Wsi.altmask
    | "shift" -> Wsi.shiftmask
    | "ctrl" | "control" -> Wsi.ctrlmask
    | "meta" -> Wsi.metamask
    | _ -> 0
  ;;

  let key_of_string =
    let r = Str.regexp "-" in
    fun s ->
      let elems = Str.full_split r s in
      let f n k m =
        let g s =
          let m1 = modifier_of_string s in
          if m1 = 0
          then (Wsi.namekey s, m)
          else (k, m lor m1)
        in function
          | Str.Delim s when n land 1 = 0 -> g s
          | Str.Text s -> g s
          | Str.Delim _ -> (k, m)
      in
      let rec loop n k m = function
        | [] -> (k, m)
        | x :: xs ->
            let k, m = f n k m x in
            loop (n+1) k m xs
      in
      loop 0 0 0 elems
  ;;

  let keys_of_string =
    let r = Str.regexp "[ \t]" in
    fun s ->
      let elems = Str.split r s in
      List.map key_of_string elems
  ;;

  let copykeyhashes c =
    List.map (fun (k, v) -> k, Hashtbl.copy v) c.keyhashes;
  ;;

  let config_of c attrs =
    let apply c k v =
      try
        match k with
        | "scroll-bar-width" -> { c with scrollbw = max 0 (int_of_string v) }
        | "scroll-handle-height" -> { c with scrollh = max 0 (int_of_string v) }
        | "case-insensitive-search" -> { c with icase = bool_of_string v }
        | "preload" -> { c with preload = bool_of_string v }
        | "page-bias" -> { c with pagebias = int_of_string v }
        | "scroll-step" -> { c with scrollstep = max 1 (int_of_string v) }
        | "horizontal-scroll-step" ->
            { c with hscrollstep = max (int_of_string v) 1 }
        | "auto-scroll-step" ->
            { c with autoscrollstep = max 0 (int_of_string v) }
        | "max-height-fit" -> { c with maxhfit = bool_of_string v }
        | "crop-hack" -> { c with crophack = bool_of_string v }
        | "throttle" ->
            let mw =
              match String.lowercase v with
              | "true" -> Some infinity
              | "false" -> None
              | f -> Some (float_of_string f)
            in
            { c with maxwait = mw}
        | "highlight-links" -> { c with hlinks = bool_of_string v }
        | "under-cursor-info" -> { c with underinfo = bool_of_string v }
        | "vertical-margin" ->
            { c with interpagespace = max 0 (int_of_string v) }
        | "zoom" ->
            let zoom = float_of_string v /. 100. in
            let zoom = max zoom 0.0 in
            { c with zoom = zoom }
        | "presentation" -> { c with presentation = bool_of_string v }
        | "rotation-angle" -> { c with angle = int_of_string v }
        | "width" -> { c with cwinw = max 20 (int_of_string v) }
        | "height" -> { c with cwinh = max 20 (int_of_string v) }
        | "persistent-bookmarks" -> { c with savebmarks = bool_of_string v }
        | "proportional-display" ->
            let fm =
              if bool_of_string v
              then FitProportional
              else FitWidth
            in
            { c with fitmodel = fm }
        | "fit-model" -> { c with fitmodel = fitmodel_of_string v }
        | "pixmap-cache-size" ->
            { c with memlimit = max 2 (int_of_string_with_suffix v) }
        | "tex-count" -> { c with texcount = max 1 (int_of_string v) }
        | "slice-height" -> { c with sliceheight = max 2 (int_of_string v) }
        | "thumbnail-width" -> { c with thumbw = max 2 (int_of_string v) }
        | "persistent-location" -> { c with jumpback = bool_of_string v }
        | "background-color" -> { c with bgcolor = color_of_string v }
        | "tile-width" -> { c with tilew = max 2 (int_of_string v) }
        | "tile-height" -> { c with tileh = max 2 (int_of_string v) }
        | "mupdf-store-size" ->
            { c with mustoresize = max 1024 (int_of_string_with_suffix v) }
        | "checkers" -> { c with checkers = bool_of_string v }
        | "aalevel" -> { c with aalevel = max 0 (int_of_string v) }
        | "trim-margins" -> { c with trimmargins = bool_of_string v }
        | "trim-fuzz" -> { c with trimfuzz = irect_of_string v }
        | "uri-launcher" -> { c with urilauncher = unent v }
        | "path-launcher" -> { c with pathlauncher = unent v }
        | "color-space" -> { c with colorspace = colorspace_of_string v }
        | "invert-colors" -> { c with invert = bool_of_string v }
        | "brightness" -> { c with colorscale = float_of_string v }
        | "redirectstderr" -> { c with redirectstderr = bool_of_string v }
        | "ghyllscroll" ->
            { c with ghyllscroll = Some (ghyllscroll_of_string v) }
        | "columns" ->
            let (n, _, _) as nab = multicolumns_of_string v in
            if n < 0
            then { c with columns = Csplit (-n, [||]) }
            else { c with columns = Cmulti (nab, [||]) }
        | "birds-eye-columns" ->
            { c with beyecolumns = Some (max (int_of_string v) 2) }
        | "selection-command" -> { c with selcmd = unent v }
        | "synctex-command" -> { c with stcmd = unent v }
        | "pax-command" -> { c with paxcmd = unent v }
        | "update-cursor" -> { c with updatecurs = bool_of_string v }
        | "hint-font-size" -> { c with hfsize = bound (int_of_string v) 5 100 }
        | "page-scroll-scale" -> { c with pgscale = float_of_string v }
        | "use-pbo" -> { c with usepbo = bool_of_string v }
        | "wheel-scrolls-pages" -> { c with wheelbypage = bool_of_string v }
        | "horizontal-scrollbar-visible" ->
            let b =
              if bool_of_string v
              then c.scrollb lor scrollbhv
              else c.scrollb land (lnot scrollbhv)
            in
            { c with scrollb = b }
        | "vertical-scrollbar-visible" ->
            let b =
              if bool_of_string v
              then c.scrollb lor scrollbvv
              else c.scrollb land (lnot scrollbvv)
            in
            { c with scrollb = b }
        | "remote-in-a-new-instance" -> { c with riani = bool_of_string v }
        | "point-and-x" ->
            { c with pax =
                if bool_of_string v
                then Some (ref (0.0, 0, 0))
                else None }
        | _ -> c
      with exn ->
        prerr_endline ("Error processing attribute (`" ^
                          k ^ "'=`" ^ v ^ "'): " ^ exntos exn);
        c
    in
    let rec fold c = function
      | [] -> c
      | (k, v) :: rest ->
          let c = apply c k v in
          fold c rest
    in
    fold { c with keyhashes = copykeyhashes c } attrs;
  ;;

  let fromstring f pos n v d =
    try f v
    with exn ->
      dolog "Error processing attribute (%S=%S) at %d\n%s"
        n v pos (exntos exn)
      ;
      d
  ;;

  let bookmark_of attrs =
    let rec fold title page rely visy = function
      | ("title", v) :: rest -> fold v page rely visy rest
      | ("page", v) :: rest -> fold title v rely visy rest
      | ("rely", v) :: rest -> fold title page v visy rest
      | ("visy", v) :: rest -> fold title page rely v rest
      | _ :: rest -> fold title page rely visy rest
      | [] -> title, page, rely, visy
    in
    fold "invalid" "0" "0" "0" attrs
  ;;

  let doc_of attrs =
    let rec fold path page rely pan visy = function
      | ("path", v) :: rest -> fold v page rely pan visy rest
      | ("page", v) :: rest -> fold path v rely pan visy rest
      | ("rely", v) :: rest -> fold path page v pan visy rest
      | ("pan", v) :: rest -> fold path page rely v visy rest
      | ("visy", v) :: rest -> fold path page rely pan v rest
      | _ :: rest -> fold path page rely pan visy rest
      | [] -> path, page, rely, pan, visy
    in
    fold "" "0" "0" "0" "0" attrs
  ;;

  let map_of attrs =
    let rec fold rs ls = function
      | ("out", v) :: rest -> fold v ls rest
      | ("in", v) :: rest -> fold rs v rest
      | _ :: rest -> fold ls rs rest
      | [] -> ls, rs
    in
    fold "" "" attrs
  ;;

  let setconf dst src =
    dst.scrollbw       <- src.scrollbw;
    dst.scrollh        <- src.scrollh;
    dst.icase          <- src.icase;
    dst.preload        <- src.preload;
    dst.pagebias       <- src.pagebias;
    dst.verbose        <- src.verbose;
    dst.scrollstep     <- src.scrollstep;
    dst.maxhfit        <- src.maxhfit;
    dst.crophack       <- src.crophack;
    dst.autoscrollstep <- src.autoscrollstep;
    dst.maxwait        <- src.maxwait;
    dst.hlinks         <- src.hlinks;
    dst.underinfo      <- src.underinfo;
    dst.interpagespace <- src.interpagespace;
    dst.zoom           <- src.zoom;
    dst.presentation   <- src.presentation;
    dst.angle          <- src.angle;
    dst.cwinw          <- src.cwinw;
    dst.cwinh          <- src.cwinh;
    dst.savebmarks     <- src.savebmarks;
    dst.memlimit       <- src.memlimit;
    dst.fitmodel       <- src.fitmodel;
    dst.texcount       <- src.texcount;
    dst.sliceheight    <- src.sliceheight;
    dst.thumbw         <- src.thumbw;
    dst.jumpback       <- src.jumpback;
    dst.bgcolor        <- src.bgcolor;
    dst.tilew          <- src.tilew;
    dst.tileh          <- src.tileh;
    dst.mustoresize    <- src.mustoresize;
    dst.checkers       <- src.checkers;
    dst.aalevel        <- src.aalevel;
    dst.trimmargins    <- src.trimmargins;
    dst.trimfuzz       <- src.trimfuzz;
    dst.urilauncher    <- src.urilauncher;
    dst.colorspace     <- src.colorspace;
    dst.invert         <- src.invert;
    dst.colorscale     <- src.colorscale;
    dst.redirectstderr <- src.redirectstderr;
    dst.ghyllscroll    <- src.ghyllscroll;
    dst.columns        <- src.columns;
    dst.beyecolumns    <- src.beyecolumns;
    dst.selcmd         <- src.selcmd;
    dst.updatecurs     <- src.updatecurs;
    dst.pathlauncher   <- src.pathlauncher;
    dst.keyhashes      <- copykeyhashes src;
    dst.hfsize         <- src.hfsize;
    dst.hscrollstep    <- src.hscrollstep;
    dst.pgscale        <- src.pgscale;
    dst.usepbo         <- src.usepbo;
    dst.wheelbypage    <- src.wheelbypage;
    dst.stcmd          <- src.stcmd;
    dst.paxcmd         <- src.paxcmd;
    dst.scrollb        <- src.scrollb;
    dst.riani          <- src.riani;
    dst.pax            <-
      if src.pax = None
      then None
      else Some ((ref (0.0, 0, 0)));
  ;;

  let get s =
    let h = Hashtbl.create 10 in
    let dc = { defconf with angle = defconf.angle } in
    let rec toplevel v t spos _ =
      match t with
      | Vdata | Vcdata | Vend -> v
      | Vopen ("llppconfig", _, closed) ->
          if closed
          then v
          else { v with f = llppconfig }
      | Vopen _ ->
          error "unexpected subelement at top level" s spos
      | Vclose _ -> error "unexpected close at top level" s spos

    and llppconfig v t spos _ =
      match t with
      | Vdata | Vcdata -> v
      | Vend -> error "unexpected end of input in llppconfig" s spos
      | Vopen ("defaults", attrs, closed) ->
          let c = config_of dc attrs in
          setconf dc c;
          if closed
          then v
          else { v with f = defaults }

      | Vopen ("ui-font", attrs, closed) ->
          let rec getsize size = function
            | [] -> size
            | ("size", v) :: rest ->
                let size =
                  fromstring int_of_string spos "size" v fstate.fontsize in
                getsize size rest
            | l -> getsize size l
          in
          fstate.fontsize <- getsize fstate.fontsize attrs;
          if closed
          then v
          else { v with f = uifont (Buffer.create 10) }

      | Vopen ("doc", attrs, closed) ->
          let pathent, spage, srely, span, svisy = doc_of attrs in
          let path = unent pathent
          and pageno = fromstring int_of_string spos "page" spage 0
          and rely = fromstring float_of_string spos "rely" srely 0.0
          and pan = fromstring int_of_string spos "pan" span 0
          and visy = fromstring float_of_string spos "visy" svisy 0.0 in
          let c = config_of dc attrs in
          let anchor = (pageno, rely, visy) in
          if closed
          then (Hashtbl.add h path (c, [], pan, anchor); v)
          else { v with f = doc path pan anchor c [] }

      | Vopen _ ->
          error "unexpected subelement in llppconfig" s spos

      | Vclose "llppconfig" ->  { v with f = toplevel }
      | Vclose _ -> error "unexpected close in llppconfig" s spos

    and defaults v t spos _ =
      match t with
      | Vdata | Vcdata -> v
      | Vend -> error "unexpected end of input in defaults" s spos
      | Vopen ("keymap", attrs, closed) ->
          let modename =
            try List.assoc "mode" attrs
            with Not_found -> "global" in
          if closed
          then v
          else
            let ret keymap =
              let h = findkeyhash dc modename in
              KeyMap.iter (Hashtbl.replace h) keymap;
              defaults
            in
            { v with f = pkeymap ret KeyMap.empty }

      | Vopen (_, _, _) ->
          error "unexpected subelement in defaults" s spos

      | Vclose "defaults" ->
          { v with f = llppconfig }

      | Vclose _ -> error "unexpected close in defaults" s spos

    and uifont b v t spos epos =
      match t with
      | Vdata | Vcdata ->
          Buffer.add_substring b s spos (epos - spos);
          v
      | Vopen (_, _, _) ->
          error "unexpected subelement in ui-font" s spos
      | Vclose "ui-font" ->
          if String.length !fontpath = 0
          then fontpath := Buffer.contents b;
          { v with f = llppconfig }
      | Vclose _ -> error "unexpected close in ui-font" s spos
      | Vend -> error "unexpected end of input in ui-font" s spos

    and doc path pan anchor c bookmarks v t spos _ =
      match t with
      | Vdata | Vcdata -> v
      | Vend -> error "unexpected end of input in doc" s spos
      | Vopen ("bookmarks", _, closed) ->
          if closed
          then v
          else { v with f = pbookmarks path pan anchor c bookmarks }

      | Vopen ("keymap", attrs, closed) ->
          let modename =
            try List.assoc "mode" attrs
            with Not_found -> "global"
          in
          if closed
          then v
          else
            let ret keymap =
              let h = findkeyhash c modename in
              KeyMap.iter (Hashtbl.replace h) keymap;
              doc path pan anchor c bookmarks
            in
            { v with f = pkeymap ret KeyMap.empty }

      | Vopen (_, _, _) ->
          error "unexpected subelement in doc" s spos

      | Vclose "doc" ->
          Hashtbl.add h path (c, List.rev bookmarks, pan, anchor);
          { v with f = llppconfig }

      | Vclose _ -> error "unexpected close in doc" s spos

    and pkeymap ret keymap v t spos _ =
      match t with
      | Vdata | Vcdata -> v
      | Vend -> error "unexpected end of input in keymap" s spos
      | Vopen ("map", attrs, closed) ->
          let r, l = map_of attrs in
          let kss = fromstring keys_of_string spos "in" r [] in
          let lss = fromstring keys_of_string spos "out" l [] in
          let keymap =
            match kss with
            | [] -> keymap
            | ks :: [] -> KeyMap.add ks (KMinsrl lss) keymap
            | ks :: rest -> KeyMap.add ks (KMmulti (rest, lss)) keymap
          in
          if closed
          then { v with f = pkeymap ret keymap }
          else
            let f () = v in
            { v with f = skip "map" f }

      | Vopen _ ->
          error "unexpected subelement in keymap" s spos

      | Vclose "keymap" ->
          { v with f = ret keymap }

      | Vclose _ -> error "unexpected close in keymap" s spos

    and pbookmarks path pan anchor c bookmarks v t spos _ =
      match t with
      | Vdata | Vcdata -> v
      | Vend -> error "unexpected end of input in bookmarks" s spos
      | Vopen ("item", attrs, closed) ->
          let titleent, spage, srely, svisy = bookmark_of attrs in
          let page = fromstring int_of_string spos "page" spage 0
          and rely = fromstring float_of_string spos "rely" srely 0.0
          and visy = fromstring float_of_string spos "visy" svisy 0.0 in
          let bookmarks =
            (unent titleent, 0, (page, rely, visy)) :: bookmarks
          in
          if closed
          then { v with f = pbookmarks path pan anchor c bookmarks }
          else
            let f () = v in
            { v with f = skip "item" f }

      | Vopen _ ->
          error "unexpected subelement in bookmarks" s spos

      | Vclose "bookmarks" ->
          { v with f = doc path pan anchor c bookmarks }

      | Vclose _ -> error "unexpected close in bookmarks" s spos

    and skip tag f v t spos _ =
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
        failwith ("config load error: " ^ exntos exn)
  ;;

  let defconfpath =
    let dir =
      try
        let dir = Filename.concat home ".config" in
        if Sys.is_directory dir then dir else home
      with _ -> home
    in
    Filename.concat dir "llpp.conf"
  ;;

  let confpath = ref defconfpath;;

  let load1 f =
    if Sys.file_exists !confpath
    then
      match
        (try Some (open_in_bin !confpath)
          with exn ->
            prerr_endline
              ("Error opening configuration file `" ^ !confpath ^ "': " ^
                  exntos exn);
            None
        )
      with
      | Some ic ->
          let success =
            try
              f (do_load get ic)
            with exn ->
              prerr_endline
                ("Error loading configuration from `" ^ !confpath ^ "': " ^
                    exntos exn);
              false
          in
          close_in ic;
          success

      | None -> false
    else
      f (Hashtbl.create 0, defconf)
  ;;

  let load () =
    let f (h, dc) =
      let pc, pb, px, pa =
        try
          let key =
            if String.length state.origin = 0
            then state.path
            else state.origin
          in
          Hashtbl.find h (Filename.basename key)
        with Not_found -> dc, [], 0, emptyanchor
      in
      setconf defconf dc;
      setconf conf pc;
      state.bookmarks <- pb;
      state.x <- px;
      if conf.jumpback
      then state.anchor <- pa;
      cbput state.hists.nav pa;
      true
    in
    load1 f
  ;;

  let add_attrs bb always dc c =
    let ob s a b =
      if always || a != b
      then Printf.bprintf bb "\n    %s='%b'" s a
    and op s a b =
      if always || a <> b
      then Printf.bprintf bb "\n    %s='%b'" s (a != None)
    and oi s a b =
      if always || a != b
      then Printf.bprintf bb "\n    %s='%d'" s a
    and oI s a b =
      if always || a != b
      then Printf.bprintf bb "\n    %s='%s'" s (string_with_suffix_of_int a)
    and oz s a b =
      if always || a <> b
      then Printf.bprintf bb "\n    %s='%g'" s (a*.100.)
    and oF s a b =
      if always || a <> b
      then Printf.bprintf bb "\n    %s='%f'" s a
    and oc s a b =
      if always || a <> b
      then
        Printf.bprintf bb "\n    %s='%s'" s (color_to_string a)
    and oC s a b =
      if always || a <> b
      then
        Printf.bprintf bb "\n    %s='%s'" s (colorspace_to_string a)
    and oR s a b =
      if always || a <> b
      then
        Printf.bprintf bb "\n    %s='%s'" s (irect_to_string a)
    and os s a b =
      if always || a <> b
      then
        Printf.bprintf bb "\n    %s='%s'" s (enent a 0 (String.length a))
    and og s a b =
      if always || a <> b
      then
        match a with
        | None -> ()
        | Some (_N, _A, _B) ->
            Printf.bprintf bb "\n    %s='%u,%u,%u'" s _N _A _B
    and oW s a b =
      if always || a <> b
      then
        let v =
          match a with
          | None -> "false"
          | Some f ->
              if f = infinity
              then "true"
              else string_of_float f
        in
        Printf.bprintf bb "\n    %s='%s'" s v
    and oco s a b =
      if always || a <> b
      then
        match a with
        | Cmulti ((n, a, b), _) when n > 1 ->
            Printf.bprintf bb "\n    %s='%d,%d,%d'" s n a b
        | Csplit (n, _) when n > 1 ->
            Printf.bprintf bb "\n    %s='%d'" s ~-n
        | _ -> ()
    and obeco s a b =
      if always || a <> b
      then
        match a with
        | Some c when c > 1 -> Printf.bprintf bb "\n    %s='%d'" s c
        | _ -> ()
    and oFm s a b =
      if always || a <> b
      then
        Printf.bprintf bb "\n    %s='%s'" s (fitmodel_to_string a)
    and oSv s a b m =
      if always || a <> b
      then
        Printf.bprintf bb "\n    %s='%b'" s (a land m != 0)
    in
    oi "width" c.cwinw dc.cwinw;
    oi "height" c.cwinh dc.cwinh;
    oi "scroll-bar-width" c.scrollbw dc.scrollbw;
    oi "scroll-handle-height" c.scrollh dc.scrollh;
    oSv "horizontal-scrollbar-visible" c.scrollb dc.scrollb scrollbhv;
    oSv "vertical-scrollbar-visible" c.scrollb dc.scrollb scrollbvv;
    ob "case-insensitive-search" c.icase dc.icase;
    ob "preload" c.preload dc.preload;
    oi "page-bias" c.pagebias dc.pagebias;
    oi "scroll-step" c.scrollstep dc.scrollstep;
    oi "auto-scroll-step" c.autoscrollstep dc.autoscrollstep;
    ob "max-height-fit" c.maxhfit dc.maxhfit;
    ob "crop-hack" c.crophack dc.crophack;
    oW "throttle" c.maxwait dc.maxwait;
    ob "highlight-links" c.hlinks dc.hlinks;
    ob "under-cursor-info" c.underinfo dc.underinfo;
    oi "vertical-margin" c.interpagespace dc.interpagespace;
    oz "zoom" c.zoom dc.zoom;
    ob "presentation" c.presentation dc.presentation;
    oi "rotation-angle" c.angle dc.angle;
    ob "persistent-bookmarks" c.savebmarks dc.savebmarks;
    oFm "fit-model" c.fitmodel dc.fitmodel;
    oI "pixmap-cache-size" c.memlimit dc.memlimit;
    oi "tex-count" c.texcount dc.texcount;
    oi "slice-height" c.sliceheight dc.sliceheight;
    oi "thumbnail-width" c.thumbw dc.thumbw;
    ob "persistent-location" c.jumpback dc.jumpback;
    oc "background-color" c.bgcolor dc.bgcolor;
    oi "tile-width" c.tilew dc.tilew;
    oi "tile-height" c.tileh dc.tileh;
    oI "mupdf-store-size" c.mustoresize dc.mustoresize;
    ob "checkers" c.checkers dc.checkers;
    oi "aalevel" c.aalevel dc.aalevel;
    ob "trim-margins" c.trimmargins dc.trimmargins;
    oR "trim-fuzz" c.trimfuzz dc.trimfuzz;
    os "uri-launcher" c.urilauncher dc.urilauncher;
    os "path-launcher" c.pathlauncher dc.pathlauncher;
    oC "color-space" c.colorspace dc.colorspace;
    ob "invert-colors" c.invert dc.invert;
    oF "brightness" c.colorscale dc.colorscale;
    ob "redirectstderr" c.redirectstderr dc.redirectstderr;
    og "ghyllscroll" c.ghyllscroll dc.ghyllscroll;
    oco "columns" c.columns dc.columns;
    obeco "birds-eye-columns" c.beyecolumns dc.beyecolumns;
    os "selection-command" c.selcmd dc.selcmd;
    os "synctex-command" c.stcmd dc.stcmd;
    os "pax-command" c.paxcmd dc.paxcmd;
    ob "update-cursor" c.updatecurs dc.updatecurs;
    oi "hint-font-size" c.hfsize dc.hfsize;
    oi "horizontal-scroll-step" c.hscrollstep dc.hscrollstep;
    oF "page-scroll-scale" c.pgscale dc.pgscale;
    ob "use-pbo" c.usepbo dc.usepbo;
    ob "wheel-scrolls-pages" c.wheelbypage dc.wheelbypage;
    ob "remote-in-a-new-instance" c.riani dc.riani;
    op "point-and-x" c.pax dc.pax;
  ;;

  let keymapsbuf always dc c =
    let bb = Buffer.create 16 in
    let rec loop = function
      | [] -> ()
      | (modename, h) :: rest ->
          let dh = findkeyhash dc modename in
          if always || h <> dh
          then (
            if Hashtbl.length h > 0
            then (
              if Buffer.length bb > 0
              then Buffer.add_char bb '\n';
              Printf.bprintf bb "<keymap mode='%s'>\n" modename;
              Hashtbl.iter (fun i o ->
                let isdifferent = always ||
                  try
                    let dO = Hashtbl.find dh i in
                    dO <> o
                  with Not_found -> true
                in
                if isdifferent
                then
                  let addkm (k, m) =
                    if Wsi.withctrl m  then Buffer.add_string bb "ctrl-";
                    if Wsi.withalt m   then Buffer.add_string bb "alt-";
                    if Wsi.withshift m then Buffer.add_string bb "shift-";
                    if Wsi.withmeta m  then Buffer.add_string bb "meta-";
                    Buffer.add_string bb (Wsi.keyname k);
                  in
                  let addkms l =
                    let rec loop = function
                      | [] -> ()
                      | km :: [] -> addkm km
                      | km :: rest -> addkm km; Buffer.add_char bb ' '; loop rest
                    in
                    loop l
                  in
                  Buffer.add_string bb "<map in='";
                  addkm i;
                  match o with
                  | KMinsrt km ->
                      Buffer.add_string bb "' out='";
                      addkm km;
                      Buffer.add_string bb "'/>\n"

                  | KMinsrl kms ->
                      Buffer.add_string bb "' out='";
                      addkms kms;
                      Buffer.add_string bb "'/>\n"

                  | KMmulti (ins, kms) ->
                      Buffer.add_char bb ' ';
                      addkms ins;
                      Buffer.add_string bb "' out='";
                      addkms kms;
                      Buffer.add_string bb "'/>\n"
              ) h;
              Buffer.add_string bb "</keymap>";
            );
          );
          loop rest
    in
    loop c.keyhashes;
    bb;
  ;;

  let save () =
    let uifontsize = fstate.fontsize in
    let bb = Buffer.create 32768 in
    let relx = float state.x /. float state.winw in
    let w, h, x =
      let cx w = truncate (relx *. float w) in
      List.fold_left
        (fun (w, h, x) ws ->
          match ws with
          | Wsi.Fullscreen -> (conf.cwinw, conf.cwinh, cx conf.cwinw)
          | Wsi.MaxVert -> (w, conf.cwinh, x)
          | Wsi.MaxHorz -> (conf.cwinw, h, cx conf.cwinw)
        )
        (state.winw, state.winh, state.x) state.winstate
    in
    conf.cwinw <- w;
    conf.cwinh <- h;
    let f (h, dc) =
      let dc = if conf.bedefault then conf else dc in
      Buffer.add_string bb "<llppconfig>\n";

      if String.length !fontpath > 0
      then
        Printf.bprintf bb "<ui-font size='%d'><![CDATA[%s]]></ui-font>\n"
          uifontsize
          !fontpath
      else (
        if uifontsize <> 14
        then
          Printf.bprintf bb "<ui-font size='%d'/>\n" uifontsize
      );

      Buffer.add_string bb "<defaults ";
      add_attrs bb true dc dc;
      let kb = keymapsbuf true dc dc in
      if Buffer.length kb > 0
      then (
        Buffer.add_string bb ">\n";
        Buffer.add_buffer bb kb;
        Buffer.add_string bb "\n</defaults>\n";
      )
      else Buffer.add_string bb "/>\n";

      let adddoc path pan anchor c bookmarks =
        if bookmarks == [] && c = dc && anchor = emptyanchor
        then ()
        else (
          Printf.bprintf bb "<doc path='%s'"
            (enent path 0 (String.length path));

          if anchor <> emptyanchor
          then (
            let n, rely, visy = anchor in
            Printf.bprintf bb " page='%d'" n;
            if rely > 1e-6
            then
              Printf.bprintf bb " rely='%f'" rely
            ;
            if abs_float visy > 1e-6
            then
              Printf.bprintf bb " visy='%f'" visy
            ;
          );

          if pan != 0
          then Printf.bprintf bb " pan='%d'" pan;

          add_attrs bb false dc c;
          let kb = keymapsbuf false dc c in

          begin match bookmarks with
          | [] ->
              if Buffer.length kb > 0
              then (
                Buffer.add_string bb ">\n";
                Buffer.add_buffer bb kb;
                Buffer.add_string bb "\n</doc>\n";
              )
              else Buffer.add_string bb "/>\n"
          | _ ->
              Buffer.add_string bb ">\n<bookmarks>\n";
              List.iter (fun (title, _level, (page, rely, visy)) ->
                Printf.bprintf bb
                  "<item title='%s' page='%d'"
                  (enent title 0 (String.length title))
                  page
                ;
                if rely > 1e-6
                then
                  Printf.bprintf bb " rely='%f'" rely
                ;
                if abs_float visy > 1e-6
                then
                  Printf.bprintf bb " visy='%f'" visy
                ;
                Buffer.add_string bb "/>\n";
              ) bookmarks;
              Buffer.add_string bb "</bookmarks>";
              if Buffer.length kb > 0
              then (
                Buffer.add_string bb "\n";
                Buffer.add_buffer bb kb;
              );
              Buffer.add_string bb "\n</doc>\n";
          end;
        )
      in

      let pan, conf =
        match state.mode with
        | Birdseye (c, pan, _, _, _) ->
            let beyecolumns =
              match conf.columns with
              | Cmulti ((c, _, _), _) -> Some c
              | Csingle _ -> None
              | Csplit _ -> None
            and columns =
              match c.columns with
              | Cmulti (c, _) -> Cmulti (c, [||])
              | Csingle _ -> Csingle [||]
              | Csplit _ -> failwith "quit from bird's eye while split"
            in
            pan, { c with beyecolumns = beyecolumns; columns = columns }
        | _ -> x, conf
      in
      let basename = Filename.basename
        (if String.length state.origin = 0 then state.path else state.origin)
      in
      adddoc basename pan (getanchor ())
        (let conf =
          let autoscrollstep =
            match state.autoscroll with
            | Some step -> step
            | None -> conf.autoscrollstep
          in
          match state.mode with
          | Birdseye (bc, _, _, _, _) ->
              { conf with
                zoom = bc.zoom;
                presentation = bc.presentation;
                interpagespace = bc.interpagespace;
                maxwait = bc.maxwait;
                autoscrollstep = autoscrollstep }
          | _ -> { conf with autoscrollstep = autoscrollstep }
          in conf)
        (if conf.savebmarks then state.bookmarks else []);

      Hashtbl.iter (fun path (c, bookmarks, x, anchor) ->
        if basename <> path
        then adddoc path x anchor c bookmarks
      ) h;
      Buffer.add_string bb "</llppconfig>\n";
      true;
    in
    if load1 f && Buffer.length bb > 0
    then
      try
        let tmp = !confpath ^ ".tmp" in
        let oc = open_out_bin tmp in
        Buffer.output_buffer oc bb;
        close_out oc;
        Unix.rename tmp !confpath;
      with exn ->
        prerr_endline
          ("error while saving configuration: " ^ exntos exn)
  ;;
end;;

let adderrmsg src msg =
  Buffer.add_string state.errmsgs msg;
  state.newerrmsgs <- true;
  G.postRedisplay src
;;

let adderrfmt src fmt =
  Format.kprintf (fun s -> adderrmsg src s) fmt;
;;

let ract cmds =
  let cl = splitatspace cmds in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn ->
      adderrfmt "remote exec"
        "error processing '%S': %s\n" cmds (exntos exn)
  in
  match cl with
  | "reload" :: [] -> reload ()
  | "goto" :: args :: [] ->
      scan args "%u %f %f"
        (fun pageno x y ->
          let cmd, _ = state.geomcmds in
          if String.length cmd = 0
          then gotopagexy pageno x y
          else
            let f prevf () =
              gotopagexy pageno x y;
              prevf ()
            in
            state.reprf <- f state.reprf
        )
  | "goto1" :: args :: [] -> scan args "%u %f" gotopage
  | "rect" :: args :: [] ->
      scan args "%u %u %f %f %f %f"
        (fun pageno color x0 y0 x1 y1 ->
          onpagerect pageno (fun w h ->
            let _,w1,h1,_ = getpagedim pageno in
            let sw = float w1 /. w
            and sh = float h1 /. h in
            let x0s = x0 *. sw
            and x1s = x1 *. sw
            and y0s = y0 *. sh
            and y1s = y1 *. sh in
            let rect = (x0s,y0s,x1s,y0s,x1s,y1s,x0s,y1s) in
            debugrect rect;
            state.rects <- (pageno, color, rect) :: state.rects;
            G.postRedisplay "rect";
          )
        )
  | "activatewin" :: [] -> Wsi.activatewin ()
  | "quit" :: [] -> raise Quit
  | _ ->
      adderrfmt "remote command"
        "error processing remote command: %S\n" cmds;
;;

let remote =
  let scratch = String.create 80 in
  let buf = Buffer.create 80 in
  fun fd ->
    let rec tempfr () =
      try Some (Unix.read fd scratch 0 80)
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> None
      | Unix.Unix_error (Unix.EINTR, _, _) -> tempfr ()
      | exn -> raise exn
    in
    match tempfr () with
    | None -> Some fd
    | Some n ->
        if n = 0
        then (
          Unix.close fd;
          if Buffer.length buf > 0
          then (
            let s = Buffer.contents buf in
            Buffer.clear buf;
            ract s;
          );
          None
        )
        else
          let rec eat ppos =
            let nlpos =
              try
                let pos = String.index_from scratch ppos '\n' in
                if pos >= n then -1 else pos
              with Not_found -> -1
            in
            if nlpos >= 0
            then (
              Buffer.add_substring buf scratch ppos (nlpos-ppos);
              let s = Buffer.contents buf in
              Buffer.clear buf;
              ract s;
              eat (nlpos+1);
            )
            else (
              Buffer.add_substring buf scratch ppos (n-ppos);
              Some fd
            )
          in eat 0
;;

let remoteopen path =
  try Some (Unix.openfile path [Unix.O_NONBLOCK; Unix.O_RDONLY] 0o0)
  with exn ->
    adderrfmt "remoteopen" "error opening %S: %s" path (exntos exn);
    None
;;

let () =
  let trimcachepath = ref "" in
  let rcmdpath = ref "" in
  selfexec := Sys.executable_name;
  Arg.parse
    (Arg.align
        [("-p", Arg.String (fun s -> state.password <- s),
         "<password> Set password");

         ("-f", Arg.String
           (fun s ->
             Config.fontpath := s;
             selfexec := !selfexec ^ " -f " ^ Filename.quote s;
           ),
         "<path> Set path to the user interface font");

         ("-c", Arg.String
           (fun s ->
             selfexec := !selfexec ^ " -c " ^ Filename.quote s;
             Config.confpath := s),
         "<path> Set path to the configuration file");

         ("-tcf", Arg.String (fun s -> trimcachepath := s),
         "<path> Set path to the trim cache file");

         ("-dest", Arg.String (fun s -> state.nameddest <- s),
         "<named-destination> Set named destination");

         ("-wtmode", Arg.Set wtmode, " Operate in wt mode");

         ("-remote", Arg.String (fun s -> rcmdpath := s),
         "<path> Set path to the remote commands source");

         ("-origin", Arg.String (fun s -> state.origin <- s),
         "<original-path> Set original path");

         ("-v", Arg.Unit (fun () ->
           Printf.printf
             "%s\nconfiguration path: %s\n"
             (version ())
             Config.defconfpath
           ;
           exit 0), " Print version and exit");
        ]
    )
    (fun s -> state.path <- s)
    ("Usage: " ^ Sys.argv.(0) ^ " [options] some.pdf\nOptions:")
  ;
  if !wtmode
  then selfexec := !selfexec ^ " -wtmode";

  if String.length state.path = 0
  then (prerr_endline "file name missing"; exit 1);

  if not (Config.load ())
  then prerr_endline "failed to load configuration";

  let wsfd, winw, winh = Wsi.init (object
    val mutable m_hack = false
    method expose = if not m_hack then G.postRedisplay "expose"
    method visible = G.postRedisplay "visible"
    method display = m_hack <- false; display ()
    method reshape w h =
      m_hack <- w < state.winw && h < state.winh;
      reshape w h
    method mouse b d x y m = mouse b d x y m
    method motion x y = state.mpos <- (x, y); motion x y
    method pmotion x y = state.mpos <- (x, y); pmotion x y
    method key k m =
      let mascm = m land (
        Wsi.altmask + Wsi.shiftmask + Wsi.ctrlmask + Wsi.metamask
      ) in
      match state.keystate with
      | KSnone ->
          let km = k, mascm in
          begin
            match
              let modehash = state.uioh#modehash in
              try Hashtbl.find modehash km
              with Not_found ->
                try Hashtbl.find (findkeyhash conf "global") km
                with Not_found -> KMinsrt (k, m)
            with
            | KMinsrt (k, m) -> keyboard k m
            | KMinsrl l -> List.iter (fun (k, m) -> keyboard k m) l
            | KMmulti (l, r) -> state.keystate <- KSinto (l, r)
          end
      | KSinto ((k', m') :: [], insrt) when k'=k && m' land mascm = m' ->
          List.iter (fun (k, m) -> keyboard k m) insrt;
          state.keystate <- KSnone
      | KSinto ((k', m') :: keys, insrt) when k'=k && m' land mascm = m' ->
          state.keystate <- KSinto (keys, insrt)
      | _ ->
          state.keystate <- KSnone

    method enter x y = state.mpos <- (x, y); pmotion x y
    method leave = state.mpos <- (-1, -1)
    method winstate wsl = state.winstate <- wsl
    method quit = raise Quit
  end) conf.cwinw conf.cwinh (platform = Posx) in

  state.wsfd <- wsfd;

  if not (
    List.exists GlMisc.check_extension
      [ "GL_ARB_texture_rectangle"
      ; "GL_EXT_texture_recangle"
      ; "GL_NV_texture_rectangle" ]
  )
  then (prerr_endline "OpenGL does not suppport rectangular textures"; exit 1);

  if (
    let r = GlMisc.get_string `renderer in
    let p = "Mesa DRI Intel(" in
    let l = String.length p in
    String.length r > l && String.sub r 0 l = p
  )
  then defconf.sliceheight <- 1024;

  let cr, sw =
    match Ne.pipe () with
    | Ne.Exn exn ->
        Printf.eprintf "pipe/crsw failed: %s" (exntos exn);
        exit 1
    | Ne.Res rw -> rw
  and sr, cw =
    match Ne.pipe () with
    | Ne.Exn exn ->
        Printf.eprintf "pipe/srcw failed: %s" (exntos exn);
        exit 1
    | Ne.Res rw -> rw
  in

  cloexec cr;
  cloexec sw;
  cloexec sr;
  cloexec cw;

  setcheckers conf.checkers;
  redirectstderr ();

  init (cr, cw) (
    conf.angle, conf.fitmodel, (conf.trimmargins, conf.trimfuzz),
    conf.texcount, conf.sliceheight, conf.mustoresize, conf.colorspace,
    !Config.fontpath, !trimcachepath,
    GlMisc.check_extension "GL_ARB_pixel_buffer_object"
  );
  state.sr <- sr;
  state.sw <- sw;
  state.text <- "Opening " ^ (mbtoutf8 state.path);
  reshape winw winh;
  opendoc state.path state.password;
  state.uioh <- uioh;
  display ();
  Wsi.mapwin ();
  Sys.set_signal Sys.sighup (Sys.Signal_handle (fun _ -> reload ()));
  let optrfd =
    ref (
      if String.length !rcmdpath > 0
      then remoteopen !rcmdpath
      else None
    )
  in

  let rec loop deadline =
    let r =
      match state.errfd with
      | None -> [state.sr; state.wsfd]
      | Some fd -> [state.sr; state.wsfd; fd]
    in
    let r =
      match !optrfd with
      | None -> r
      | Some fd -> fd :: r
    in
    if state.redisplay
    then (
      state.redisplay <- false;
      display ();
    );
    let timeout =
      let now = now () in
      if deadline > now
      then (
        if deadline = infinity
        then ~-.1.0
        else max 0.0 (deadline -. now)
      )
      else 0.0
    in
    let r, _, _ =
      try Unix.select r [] [] timeout
      with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], []
    in
    begin match r with
    | [] ->
        state.ghyll None;
        let newdeadline =
          if state.ghyll == noghyll
          then
            match state.autoscroll with
            | Some step when step != 0 ->
                let y = state.y + step in
                let y =
                  if y < 0
                  then state.maxy
                  else if y >= state.maxy then 0 else y
                in
                gotoy y;
                if state.mode = View
                then state.text <- "";
                deadline +. 0.01
            | _ -> infinity
          else deadline +. 0.01
        in
        loop newdeadline

    | l ->
        let rec checkfds = function
          | [] -> ()
          | fd :: rest when fd = state.sr ->
              let cmd = readcmd state.sr in
              act cmd;
              checkfds rest

          | fd :: rest when fd = state.wsfd ->
              Wsi.readresp fd;
              checkfds rest

          | fd :: rest when Some fd = !optrfd ->
              begin match remote fd with
              | None -> optrfd := remoteopen !rcmdpath;
              | opt -> optrfd := opt
              end;
              checkfds rest

          | fd :: rest ->
              let s = String.create 80 in
              let n = tempfailureretry (Unix.read fd s 0) 80 in
              if conf.redirectstderr
              then (
                Buffer.add_substring state.errmsgs s 0 n;
                state.newerrmsgs <- true;
                state.redisplay <- true;
              )
              else (
                prerr_string (String.sub s 0 n);
                flush stderr;
              );
              checkfds rest
        in
        checkfds l;
        let newdeadline =
          let deadline1 =
            if deadline = infinity
            then now () +. 0.01
            else deadline
          in
          match state.autoscroll with
          | Some step when step != 0 -> deadline1
          | _ -> if state.ghyll == noghyll then infinity else deadline1
        in
        loop newdeadline
    end;
  in
  try
    loop infinity;
  with Quit ->
    Config.save ();
;;
