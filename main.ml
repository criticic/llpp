open Utils
open Config
open Uiutils

module U = struct
  let dopen         = '\023'
  let cs            = '\024'
  let freepage      = '\025'
  let freetile      = '\026'
  let search        = '\027'
  let geometry      = '\028'
  let reqlayout     = '\029'
  let page          = '\030'
  let tile          = '\031'
  let trimset       = '\032'
  let settrim       = '\033'
  let sliceh        = '\034'
  let interrupt     = '\035'
  let pgscale h     = truncate (float h *. conf.pgscale)
  let nogeomcmds    = function | s, [] -> emptystr s | _ -> false
  let maxy ()       = !S.maxy - if conf.maxhfit then !S.winh else 0
  let scalecolor c  = let c = c *. conf.colorscale in (c, c, c)
  let panbound x    = bound x (- !S.w) !S.winw
  let pagevisible layout n = List.exists (fun l -> l.pageno = n) layout
  let add_to_y_and_clamp inc = bound (!S.y + inc) 0 @@ maxy ()
end

let debugrect (x0, y0, x1, y1, x2, y2, x3, y3) =
  dolog {|rect {
         x0,y0=(% f, % f)
         x1,y1=(% f, % f)
         x2,y2=(% f, % f)
         x3,y3=(% f, % f)
         }|} x0 y0 x1 y1 x2 y2 x3 y3

let hscrollh () =
  if ((conf.scrollb land scrollbhv != 0) && (!S.w > !S.winw))
     || !S.uioh#alwaysscrolly
  then conf.scrollbw
  else 0

let setfontsize n =
  fstate.fontsize <- n;
  fstate.wwidth <- Ffi.measurestr fstate.fontsize "w";
  fstate.maxrows <- (!S.winh - fstate.fontsize - 1) / (fstate.fontsize + 1)

let showtext c s =
  S.text := Printf.sprintf "%c%s" c s;
  Glutils.postRedisplay "showtext"

let adderrmsg src msg =
  Buffer.add_string S.errmsgs msg;
  S.newerrmsgs := true;
  Glutils.postRedisplay src

let settextfmt fmt = Printf.ksprintf (fun s -> S.text := s) fmt
let impmsg fmt = Printf.ksprintf (fun s -> showtext '!' s) fmt
let adderrfmt src fmt = Printf.ksprintf (fun s -> adderrmsg src s) fmt

let launchpath () =
  if emptystr conf.pathlauncher
  then adderrmsg "path launcher" "command set"
  else
    let n =
      match !S.layout with
      | l :: _ -> string_of_int l.pageno
      | _ -> E.s
    in
    let cmd = Str.global_replace Re.percents !S.path conf.pathlauncher in
    let cmd =
      if nonemptystr n
      then Str.global_replace Re.percentp n cmd
      else cmd
    in
    match spawn cmd [] with
    | exception exn ->
       adderrfmt "spawn" "failed to execute `%s': %s" cmd @@ exntos exn
    | _pid -> ()

let getopaque pageno = Hashtbl.find S.pagemap (pageno, !S.gen)

let pagetranslatepoint l x y =
  let dy = y - l.pagedispy in
  let y = dy + l.pagey in
  let dx = x - l.pagedispx in
  let x = dx + l.pagex in
  (x, y)

let onppundermouse g x y d =
  let rec f = function
    | [] -> d
    | l :: rest ->
       match getopaque l.pageno with
       | exception Not_found -> f rest
       | opaque ->
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
  in
  f !S.layout

let getunder x y =
  let g opaque l px py =
    if !S.bzoom
    then (
      match Ffi.rectofblock opaque px py with
      | Some [|x0;x1;y0;y1|] ->
         let rect = (x0, y0, x1, y0, x1, y1, x0, y1) in
         let color = (0.0, 0.0, 1.0 /. (l.pageno mod 3 |> float), 0.5) in
         S.rects := [l.pageno, color, rect];
         Glutils.postRedisplay "getunder";
      | _ -> ()
    );
    let under = Ffi.whatsunder opaque px py in
    if under = Unone then None else Some under
  in
  onppundermouse g x y Unone

let unproject x y =
  let g opaque l x y =
    match Ffi.unproject opaque x y with
    | Some (x, y) -> Some (Some (opaque, l.pageno, x, y))
    | None -> None
  in
  onppundermouse g x y None

let pipesel opaque cmd =
  if Ffi.hassel opaque
  then
    pipef ~closew:false "pipesel"
      (fun w ->
        Ffi.copysel w opaque;
        Glutils.postRedisplay "pipesel"
      ) cmd

let paxunder x y =
  let g opaque l px py =
    if Ffi.markunder opaque px py conf.paxmark
    then
      Some (fun () ->
          match getopaque l.pageno with
          | exception Not_found -> ()
          | opaque -> pipesel opaque conf.paxcmd
        )
    else None
  in
  Glutils.postRedisplay "paxunder";
  if conf.paxmark = MarkPage
  then
    List.iter (fun l ->
        match getopaque l.pageno with
        | exception Not_found -> ()
        | opaque -> Ffi.clearmark opaque) !S.layout;
  S.roamf := onppundermouse g x y (fun () -> impmsg "whoopsie daisy")

let undertext = function
  | Unone -> "none"
  | Ulinkuri s -> s
  | Utext s -> "font: " ^ s
  | Utextannot (opaque, slinkindex) ->
     "text annotation: " ^ Ffi.gettextannot opaque slinkindex
  | Ufileannot (opaque, slinkindex) ->
     "file annotation: " ^ Ffi.getfileannot opaque slinkindex

let updateunder x y =
  match getunder x y with
  | Unone -> Wsi.setcursor Wsi.CURSOR_INHERIT
  | Ulinkuri uri ->
     if conf.underinfo then showtext 'u' ("ri: " ^ uri);
     Wsi.setcursor Wsi.CURSOR_INFO
  | Utext s ->
     if conf.underinfo then showtext 'f' ("ont: " ^ s);
     Wsi.setcursor Wsi.CURSOR_TEXT
  | Utextannot _ ->
     if conf.underinfo then showtext 't' "ext annotation";
     Wsi.setcursor Wsi.CURSOR_INFO
  | Ufileannot _ ->
     if conf.underinfo then showtext 'f' "ile annotation";
     Wsi.setcursor Wsi.CURSOR_INFO

let showlinktype under =
  if conf.underinfo && under != Unone
  then showtext ' ' @@ undertext under

let intentry_with_suffix text key =
  let text =
    match [@warning "-fragile-match"] key with
    | Keys.Ascii ('0'..'9' as c) -> addchar text c
    | Keys.Ascii ('k' | 'm' | 'g' | 'K' | 'M' | 'G' as c) ->
       addchar text @@ Char.lowercase_ascii c
    | _ ->
       S.text := "invalid key";
       text
  in
  TEcont text

let wcmd cmd fmt =
  let b = Buffer.create 16 in
  Printf.kbprintf
    (fun b ->
      Buffer.add_char b cmd;
      let b = Buffer.to_bytes b in
      Ffi.wcmd !S.ss b @@ Bytes.length b
    ) b fmt

let wcmd1 cmd opaque =
  let s = Opaque.to_string opaque in
  let l = String.length s in
  let b = Bytes.create (l+1) in
  Bytes.set b l cmd;
  Bytes.blit_string s 0 b 0 l;
  Ffi.wcmd !S.ss b @@ l + 1

let layoutN ((columns, coverA, coverB), b) x y sw sh =
  let rec fold accu n =
    if n = Array.length b
    then accu
    else
      let pdimno, dx, vy, (_, w, h, xoff) = b.(n) in
      if (vy - y) > sh
         && (n = coverA - 1
             || n = !S.pagecount - coverB
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
                if n = coverA - 1 || n = !S.pagecount - coverB
                then x + (sw - w) / 2
                else dx + xoff + x
              in
              if pdx < 0
              then 0, -pdx
              else pdx, 0
            in
            let pagevw =
              let vw = sw - pagedispx in
              let pw = w - pagex in
              min vw pw
            in
            let pagevh = min (h - pagey) (sh - pagedispy) in
            if pagevw > 0 && pagevh > 0
            then
              { pageno = n
              ; pagecol = 0 ; pagedimno = pdimno ; pagew = w ; pageh = h
              ; pagex ; pagey ; pagevw ; pagevh ; pagedispx ; pagedispy
              } :: accu
            else accu
          else accu
        in
        fold accu (n+1)
  in
  if Array.length b = 0
  then []
  else List.rev (fold [] (page_of_y y))

let layoutS (columns, b) x y sw sh =
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
            let x = xoff + x in
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
              if pagecolw < sw
              then pagedispx + ((sw - pagecolw) / 2)
              else pagedispx
            in
            let pagevw =
              let vw = sw - pagedispx in
              let pw = pagew - pagex in
              min vw pw
            in
            let pagevw = min pagevw pagecolw in
            let pagevh = min (pageh - pagey) (sh - pagedispy) in
            if pagevw > 0 && pagevh > 0
            then
              { pageno = n/columns
              ; pagedimno = pdimno
              ; pagecol = n mod columns
              ; pagew ; pageh ; pagex ; pagey ; pagedispx ; pagedispy
              ; pagevw ; pagevh
              } :: accu
            else accu
          else accu
        in
        fold accu (n+1)
  in
  List.rev (fold [] 0)

let layout x y sw sh =
  if U.nogeomcmds !S.geomcmds
  then
    match conf.columns with
    | Csingle b -> layoutN ((1, 0, 0), b) x y sw sh
    | Cmulti c -> layoutN c x y sw sh
    | Csplit s -> layoutS s x y sw sh
  else []

let itertiles l f =
  let tilex = l.pagex mod conf.tilew in
  let tiley = l.pagey mod conf.tileh in

  let col = l.pagex / conf.tilew in
  let row = l.pagey / conf.tileh in

  let rec rowloop row y0 dispy h =
    if h != 0
    then
      let dh = conf.tileh - y0 in
      let dh = min h dh in
      let rec colloop col x0 dispx w =
        if w != 0
        then
          let dw = conf.tilew - x0 in
          let dw = min w dw in
          f col row dispx dispy x0 y0 dw dh;
          colloop (col+1) 0 (dispx+dw) (w-dw)
      in
      colloop col tilex l.pagedispx l.pagevw;
      rowloop (row+1) 0 (dispy+dh) (h-dh)
  in
  if l.pagevw > 0 && l.pagevh > 0
  then rowloop row tiley l.pagedispy l.pagevh

let gettileopaque l col row =
  let key = l.pageno, !S.gen, conf.colorspace,
            conf.angle, l.pagew, l.pageh, col, row in
  Hashtbl.find_opt S.tilemap key

let puttileopaque l col row gen colorspace angle opaque size elapsed =
  let key = l.pageno, gen, colorspace, angle, l.pagew, l.pageh, col, row in
  Hashtbl.add S.tilemap key (opaque, size, elapsed)

let drawtiles l color =
  let texe e = if conf.invert then GlTex.env (`mode e) in
  GlDraw.color color;
  Ffi.begintiles ();
  let f col row x y tilex tiley w h =
    match gettileopaque l col row with
    | Some (opaque, _, t) ->
       let params = x, y, w, h, tilex, tiley in
       texe `blend;
       Ffi.drawtile params opaque;
       texe `modulate;
       if conf.debug
       then (
         Ffi.endtiles ();
         let s = Printf.sprintf "%d[%d,%d] %f sec" l.pageno col row t in
         let w = Ffi.measurestr fstate.fontsize s in
         GlDraw.color (0.0, 0.0, 0.0);
         Glutils.filledrect
           (float (x-2))
           (float (y-2))
           (float (x+2) +. w)
           (float (y + fstate.fontsize + 2));
         GlDraw.color color;
         Glutils.drawstring fstate.fontsize x (y + fstate.fontsize - 1) s;
         Ffi.begintiles ();
       );

    | None ->
       Ffi.endtiles ();
       let w = let lw = !S.winw - x in min lw w
       and h = let lh = !S.winh - y in min lh h in
       texe `blend;
       let c = if conf.invert then 0.2 else 0.8 in
       GlDraw.color (c, c, c);
       Glutils.filledrect (float x) (float y) (float (x+w)) (float (y+h));
       texe `modulate;
       if w > 128 && h > fstate.fontsize + 10
       then (
         let c = if conf.invert then 1.0 else 0.0 in
         GlDraw.color (c, c, c);
         let c, r =
           if conf.verbose
           then (col*conf.tilew, row*conf.tileh)
           else col, row
         in
         Glutils.drawstringf fstate.fontsize x y
           "Loading %d [%d,%d]" l.pageno c r;
       );
       GlDraw.color color;
       Ffi.begintiles ();
  in
  itertiles l f;
  Ffi.endtiles ()

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

let tilevisible layout n x y =
  let rec findpageinlayout m = function
    | l :: rest when l.pageno = n ->
       tilevisible1 l x y || (
        match conf.columns with
        | Csplit (c, _) when c > m -> findpageinlayout (m+1) rest
        | Csplit _ | Csingle _ | Cmulti _ -> false
      )
    | _ :: rest -> findpageinlayout 0 rest
    | [] -> false
  in
  findpageinlayout 0 layout

let tileready l x y =
  tilevisible1 l x y &&
    gettileopaque l (x/conf.tilew) (y/conf.tileh) != None

let tilepage n p layout =
  let rec loop = function
    | l :: rest ->
       if l.pageno = n
       then
         let f col row _ _ _ _ _ _ =
           if !S.currently = Idle
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
                wcmd U.tile "%s %d %d %d %d" (Opaque.to_string p) x y w h;
                S.currently :=
                  Tiling (
                      l, p, conf.colorspace, conf.angle,
                      !S.gen, col, row, conf.tilew, conf.tileh
                    );
         in
         itertiles l f;
       else loop rest

    | [] -> ()
  in
  if U.nogeomcmds !S.geomcmds
  then loop layout

let preloadlayout x y sw sh =
  let y = if y < sh then 0 else y - sh in
  let x = min 0 (x + sw) in
  let h = sh*3 in
  let w = sw*3 in
  layout x y w h

let load pages =
  let rec loop pages =
    if !S.currently = Idle
    then
      match pages with
      | l :: rest ->
         begin match getopaque l.pageno with
         | exception Not_found ->
            wcmd U.page "%d %d" l.pageno l.pagedimno;
            S.currently := Loading (l, !S.gen);
         | opaque ->
            tilepage l.pageno opaque pages;
            loop rest
         end
      | _ -> ()
  in
  if U.nogeomcmds !S.geomcmds
  then loop pages

let preload pages =
  load pages;
  if conf.preload && !S.currently = Idle
  then load (preloadlayout !S.x !S.y !S.winw !S.winh)

let alltilesrendered layout =
  let exception E in
  let rec fold ls =
    match ls with
    | [] -> true
    | l :: rest ->
       let foo col row _ _ _ _ _ _ =
         match gettileopaque l col row with
         | Some _ -> ()
         | None -> raise E
       in
       match itertiles l foo with
       | () -> fold rest
       | exception E -> false
  in
  fold layout

let gotoxy x y =
  let y = bound y 0 !S.maxy in
  let y, layout =
    let layout = layout x y !S.winw !S.winh in
    Glutils.postRedisplay "gotoxy ready";
    y, layout
  in
  S.x := x;
  S.y := y;
  S.layout := layout;
  begin match !S.mode with
  | LinkNav ln ->
     begin match ln with
     | Ltexact (pageno, linkno) ->
        let rec loop = function
          | [] ->
             S.lnava := Some (pageno, linkno);
             S.mode := LinkNav (Ltgendir 0)
          | l :: _ when l.pageno = pageno ->
             begin match getopaque pageno with
             | exception Not_found ->
                S.mode := LinkNav (Ltnotready (pageno, 0))
             | opaque ->
                let x0, y0, x1, y1 = Ffi.getlinkrect opaque linkno in
                if not (x0 >= l.pagex && x1 <= l.pagex + l.pagevw
                        && y0 >= l.pagey && y1 <= l.pagey + l.pagevh)
                then S.mode := LinkNav (Ltgendir 0)
             end
          | _ :: rest -> loop rest
        in
        loop layout
     | Ltnotready _ | Ltgendir _ -> ()
     end
  | Birdseye _ | Textentry _ | View -> ()
  end;
  begin match !S.mode with
  | Birdseye (conf, leftx, pageno, hooverpageno, anchor) ->
     if not (U.pagevisible layout pageno)
     then (
       match !S.layout with
       | [] -> ()
       | l :: _ ->
          S.mode := Birdseye (conf, leftx, l.pageno, hooverpageno, anchor)
     )
  | LinkNav lt ->
     begin match lt with
     | Ltnotready (_, dir)
     | Ltgendir dir ->
        let linknav =
          let rec loop = function
            | [] -> lt
            | l :: rest ->
               match getopaque l.pageno with
               | exception Not_found -> Ltnotready (l.pageno, dir)
               | opaque ->
                  let link =
                    let ld =
                      if dir = 0
                      then LDfirstvisible (l.pagex, l.pagey, dir)
                      else if dir > 0 then LDfirst else LDlast
                    in
                    Ffi.findlink opaque ld
                  in
                  match link with
                  | Lnotfound -> loop rest
                  | Lfound n ->
                     showlinktype (Ffi.getlink opaque n);
                     Ltexact (l.pageno, n)
          in
          loop !S.layout
        in
        S.mode := LinkNav linknav
     | Ltexact _ -> ()
     end
  | Textentry _ | View -> ()
  end;
  preload layout;
  if conf.updatecurs
  then (
    let mx, my = !S.mpos in
    updateunder mx my;
  )

let conttiling pageno opaque =
  tilepage pageno opaque
    (if conf.preload
     then preloadlayout !S.x !S.y !S.winw !S.winh
     else !S.layout)

let gotoxy x y =
  if not conf.verbose then S.text := E.s;
  gotoxy x y

let getanchory (n, top, dtop) =
  let y, h = getpageyh n in
  if conf.presentation
  then
    let ips = calcips h in
    y + truncate (top*.float h -. dtop*.float ips) + ips;
  else y + truncate (top*.float h -. dtop*.float conf.interpagespace)

let addnav () = S.nav := { past = getanchor () :: !S.nav.past; future = []; }

let gotopage n top =
  let y, h = getpageyh n in
  let y = y + (truncate (top *. float h)) in
  gotoxy !S.x y

let gotopage1 n top =
  let y = getpagey n in
  let y = y + top in
  gotoxy !S.x y

let invalidate s f =
  Glutils.redisplay := false;
  S.layout := [];
  S.pdims := [];
  S.rects := [];
  S.rects1 := [];
  match !S.geomcmds with
  | ps, [] when emptystr ps ->
     f ();
     S.geomcmds := s, [];
  | ps, [] -> S.geomcmds := ps, [s, f];
  | ps, (s', _) :: rest when s' = s -> S.geomcmds := ps, ((s, f) :: rest);
  | ps, cmds -> S.geomcmds := ps, ((s, f) :: cmds)

let flushpages () =
  Hashtbl.iter (fun _ opaque -> wcmd1 U.freepage opaque) S.pagemap;
  Hashtbl.clear S.pagemap

let flushtiles () =
  if not (Queue.is_empty S.tilelru)
  then (
    Queue.iter (fun (k, p, s) ->
        wcmd1 U.freetile p;
        S.memused := !S.memused - s;
        Hashtbl.remove S.tilemap k;
      ) S.tilelru;
    !S.uioh#infochanged Memused;
    Queue.clear S.tilelru;
  );
  load !S.layout

let stateh h =
  let h = truncate (float h*.conf.zoom) in
  let d = conf.interpagespace lsl (if conf.presentation then 1 else 0) in
  h - d

let fillhelp () =
  S.help :=
    let sl = keystostrlist conf in
    let rec loop accu =
      function | [] -> accu
               | s :: rest -> loop ((s, 0, None) :: accu) rest
    in Help.makehelp conf.urilauncher
       @ (("", 0, None) :: loop [] sl) |> Array.of_list

let titlify path =
  if emptystr path
  then path
  else
    (if emptystr !S.origin then path else !S.origin)
    |> Filename.basename |> Ffi.mbtoutf8

let settitle title =
  conf.title <- title;
  if not !S.ignoredoctitlte
  then Wsi.settitle @@ title ^ " - llpp"

let opendoc path mimetype password =
  S.path := path;
  S.mimetype := mimetype;
  S.password := password;
  S.gen := !S.gen + 1;
  S.docinfo := [];
  S.outlines := [||];

  flushpages ();
  Ffi.setaalevel conf.aalevel;
  Ffi.setpapercolor conf.papercolor;
  Ffi.setdcf conf.dcf;

  settitle @@ titlify path;
  wcmd U.dopen "%d %d %d %d %s\000%s\000%s\000%s\000"
    (btod conf.usedoccss) conf.rlw conf.rlh conf.rlem
    path mimetype password conf.css;
  invalidate "reqlayout"
    (fun () ->
      wcmd U.reqlayout " %d %d %d %s\000"
        conf.angle (FMTE.to_int conf.fitmodel)
        (stateh !S.winh) !S.nameddest
    );
  fillhelp ()

let reload () =
  S.anchor := getanchor ();
  S.reload := Some (!S.x, !S.y, now ());
  opendoc !S.path !S.mimetype !S.password

let docolumns columns =
  match columns with
  | Csingle _ ->
     let a = Array.make !S.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
     let rec loop pageno pdimno pdim y ph pdims =
       if pageno != !S.pagecount
       then
         let pdimno, ((_, w, h, xoff) as pdim), pdims =
           match pdims with
           | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
              pdimno+1, pdim, rest
           | _ ->
              pdimno, pdim, pdims
         in
         let x = max 0 (((!S.winw - w) / 2) - xoff) in
         let y =
           y + (if conf.presentation
                then (if pageno = 0 then calcips h else calcips ph + calcips h)
                else (if pageno = 0 then 0 else conf.interpagespace))
         in
         a.(pageno) <- (pdimno, x, y, pdim);
         loop (pageno+1) pdimno pdim (y + h) h pdims
     in
     loop 0 ~-1 (-1,-1,-1,-1) 0 0 !S.pdims;
     conf.columns <- Csingle a;

  | Cmulti ((columns, coverA, coverB), _) ->
     let a = Array.make !S.pagecount (-1, -1, -1, (-1, -1, -1, -1)) in
     let rec loop pageno pdimno pdim x y rowh pdims =
       let rec fixrow m =
         if m >= pageno
         then
           let (pdimno, x, y, ((_, _, h, _) as pdim)) = a.(m) in
           if h < rowh
           then a.(m) <- (pdimno, x, y + (rowh - h) / 2, pdim);
           fixrow (m+1)
       in
       if pageno = !S.pagecount
       then fixrow (((pageno - 1) / columns) * columns)
       else
         let pdimno, ((_, w, h, xoff) as pdim), pdims =
           match pdims with
           | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
              pdimno+1, pdim, rest
           | _ -> pdimno, pdim, pdims
         in
         let x, y, rowh' =
           if pageno = coverA - 1 || pageno = !S.pagecount - coverB
           then (
             let x = (!S.winw - w) / 2 in
             let ips =
               if conf.presentation then calcips h else conf.interpagespace in
             x, y + ips + rowh, h
           )
           else (
             if (pageno - coverA) mod columns = 0
             then (
               let x = max 0 (!S.winw - !S.w) / 2 in
               let y =
                 if conf.presentation
                 then
                   let ips = calcips h in
                   y + (if pageno = 0 then 0 else calcips rowh + ips)
                 else y + (if pageno = 0 then 0 else conf.interpagespace)
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
     loop 0 ~-1 (-1,-1,-1,-1) 0 0 0 !S.pdims;
     conf.columns <- Cmulti ((columns, coverA, coverB), a);

  | Csplit (c, _) ->
     let a = Array.make (!S.pagecount*c) (-1, -1, -1, (-1, -1, -1, -1)) in
     let rec loop pageno pdimno pdim y pdims =
       if pageno != !S.pagecount
       then
         let pdimno, ((_, w, h, _) as pdim), pdims =
           match pdims with
           | ((pageno', _, _, _) as pdim) :: rest when pageno' = pageno ->
              pdimno+1, pdim, rest
           | _ -> pdimno, pdim, pdims
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
     loop 0 ~-1 (-1,-1,-1,-1) 0 !S.pdims;
     conf.columns <- Csplit (c, a)

let represent () =
  docolumns conf.columns;
  S.maxy := calcheight ();
  if !S.reprf == noreprf
  then (
    match !S.mode with
    | Birdseye (_, _, pageno, _, _) ->
       let y, h = getpageyh pageno in
       let top = (!S.winh - h) / 2 in
       gotoxy !S.x (max 0 (y - top))
    | Textentry _ | View | LinkNav _ ->
       let y = getanchory !S.anchor in
       let y = min y (!S.maxy - !S.winh) in
       gotoxy !S.x y;
  )
  else (
    !S.reprf ();
    S.reprf := noreprf;
  )

let reshape ?(firsttime=false) w h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  if not firsttime && U.nogeomcmds !S.geomcmds
  then S.anchor := getanchor ();

  S.winw := w;
  let w = truncate (float w *. conf.zoom) in
  let w = max w 2 in
  S.winh := h;
  setfontsize fstate.fontsize;
  GlMat.mode `modelview;
  GlMat.load_identity ();

  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.rotate ~x:1.0 ~angle:180.0 ();
  GlMat.translate ~x:~-.1.0 ~y:~-.1.0 ();
  GlMat.scale3 (2.0 /. float !S.winw, 2.0 /. float !S.winh, 1.0);

  let relx =
    if conf.zoom <= 1.0
    then 0.0
    else float !S.x /. float !S.w
  in
  invalidate "geometry"
    (fun () ->
      S.w := w;
      if not firsttime
      then S.x := truncate (relx *. float w);
      let w =
        match conf.columns with
        | Csingle _ -> w
        | Cmulti ((c, _, _), _) -> (w - (c-1)*conf.interpagespace) / c
        | Csplit (c, _) -> w * c
      in
      wcmd U.geometry "%d %d %d" w (stateh h) (FMTE.to_int conf.fitmodel)
    )

let gctilesnotinlayout layout =
  let len = Queue.length S.tilelru in
  let rec loop qpos =
    if !S.memused > conf.memlimit
    then (
      if qpos < len
      then
        let (k, p, s) as lruitem = Queue.pop S.tilelru in
        let n, gen, colorspace, angle, pagew, pageh, col, row = k in
        let (_, pw, ph, _) = getpagedim n in
        if   gen = !S.gen
             && colorspace = conf.colorspace
             && angle = conf.angle
             && pagew = pw
             && pageh = ph
             && (
               let x = col*conf.tilew and y = row*conf.tileh in
               tilevisible layout n x y
             )
        then Queue.push lruitem S.tilelru
        else (
          wcmd1 U.freetile p;
          S.memused := !S.memused - s;
          !S.uioh#infochanged Memused;
          Hashtbl.remove S.tilemap k;
        );
        loop (qpos+1)
    )
  in
  loop 0

let onpagerect pageno f =
  let b =
    match conf.columns with
    | Cmulti (_, b) -> b
    | Csingle b -> b
    | Csplit (_, b) -> b
  in
  if pageno >= 0 && pageno < Array.length b
  then
    let (_, _, _, (_, w, h, _)) = b.(pageno) in
    f w h

let gotopagexy1 pageno x y =
  let _,w1,h1,leftx = getpagedim pageno in
  let top = y /. (float h1) in
  let left = x /. (float w1) in
  let py, w, h = getpageywh pageno in
  let wh = !S.winh in
  let x = left *. (float w) in
  let x = leftx + !S.x + truncate x in
  let sx =
    if x < 0 || x >= !S.winw
    then !S.x - x
    else !S.x
  in
  let pdy = truncate (top *. float h) in
  let y' = py + pdy in
  let dy = y' - !S.y in
  let sy =
    if x != !S.x || not (dy > 0 && dy < wh)
    then (
      if conf.presentation
      then
        if abs (py - y') > wh
        then y'
        else py
      else y';
    )
    else !S.y
  in
  if !S.x != sx || !S.y != sy
  then gotoxy sx sy
  else gotoxy !S.x !S.y

let gotopagexy pageno x y =
  match !S.mode with
  | Birdseye _ -> gotopage pageno 0.0
  | Textentry _ | View | LinkNav _ -> gotopagexy1 pageno x y

let getpassword () =
  let passcmd = getenvdef "LLPP_ASKPASS" conf.passcmd in
  if emptystr passcmd
  then (adderrmsg "askpass" "ask password program not set"; E.s)
  else getcmdoutput (adderrfmt passcmd "failed to obrain password: %s") passcmd

let pgoto opaque pageno x y =
  let pdimno = getpdimno pageno in
  let x, y = Ffi.project opaque pageno pdimno x y in
  gotopagexy pageno x y

let act cmds =
  (* dolog "%S" cmds; *)
  let spl = splitatchar cmds ' ' in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn ->
      dolog "error scanning %S: %s" cmds @@ exntos exn;
      exit 1
  in
  let addoutline outline =
    match !S.currently with
    | Outlining outlines -> S.currently := Outlining (outline :: outlines)
    | Idle -> S.currently := Outlining [outline]
    | Loading _ | Tiling _ ->
       dolog "Invalid outlining state";
       logcurrently !S.currently
  in
  match spl with
  | "clear", "" ->
     S.pdims := [];
     !S.uioh#infochanged Pdim;

  | "clearrects", "" ->
     S.rects := !S.rects1;
     Glutils.postRedisplay "clearrects";

  | "continue", args ->
     let n = scan args "%u" (fun n -> n) in
     S.pagecount := n;
     begin match !S.currently with
     | Outlining l ->
        S.currently := Idle;
        S.outlines := Array.of_list (List.rev l)
     | Idle | Loading _ | Tiling _ -> ()
     end;

     let cur, cmds = !S.geomcmds in
     if emptystr cur then error "empty geomcmd";

     begin match List.rev cmds with
     | [] ->
        S.geomcmds := E.s, [];
        represent ();
     | (s, f) :: rest ->
        f ();
        S.geomcmds := s, List.rev rest;
     end;
     Glutils.postRedisplay "continue";

  | "vmsg", args ->
     if conf.verbose then showtext ' ' args

  | "emsg", args ->
     if not !S.redirstderr
     then Format.eprintf "%s@." args
     else (
       Buffer.add_string S.errmsgs args;
       Buffer.add_char S.errmsgs '\n';
       if not !S.newerrmsgs
       then (
         S.newerrmsgs := true;
         Glutils.postRedisplay "error message";
       )
     );

  | "progress", args ->
     let progress, text =
       scan args "%f %n"
         (fun f pos -> f, String.sub args pos (String.length args - pos))
     in
     S.text := text;
     S.progress := progress;
     Glutils.postRedisplay "progress"

  | "match", args ->
     let pageno, n, x0, y0, x1, y1, x2, y2, x3, y3 =
       scan args "%u %d %f %f %f %f %f %f %f %f"
         (fun p n x0 y0 x1 y1 x2 y2 x3 y3 ->
           (p, n, x0, y0, x1, y1, x2, y2, x3, y3))
     in
     if n = 0
     then (
       let y = (getpagey pageno) + truncate y0 in
       let x =
         if (!S.x < - truncate x0) || (!S.x > !S.winw - truncate x1)
         then !S.winw/2 - truncate (x0 /. 2. +. x1 /. 2.)
         else !S.x
       in
       addnav ();
       gotoxy x y;
     );
     let color = (0.0, 0.0, (if n = 0 then 1.0 else 0.5), 0.5) in
     S.rects1 :=
       (pageno, color, (x0, y0, x1, y1, x2, y2, x3, y3)) :: !S.rects1

  | "page", args ->
     let pageopaques, t = scan args "%s %f" (fun p t -> p, t) in
     let pageopaque = Opaque.of_string pageopaques in
     begin match !S.currently with
     | Loading (l, gen) ->
        vlog "page %d took %f sec" l.pageno t;
        Hashtbl.replace S.pagemap (l.pageno, gen) pageopaque;
        let preloadedpages =
          if conf.preload
          then preloadlayout !S.x !S.y !S.winw !S.winh
          else !S.layout
        in
        let evict () =
          let set = List.fold_left (fun s l -> IntSet.add l.pageno s)
                      IntSet.empty preloadedpages
          in
          let evictedpages =
            Hashtbl.fold (fun ((pageno, _) as key) opaque accu ->
                if not (IntSet.mem pageno set)
                then (
                  wcmd1 U.freepage opaque;
                  key :: accu
                )
                else accu
              ) S.pagemap []
          in
          List.iter (Hashtbl.remove S.pagemap) evictedpages;
        in
        evict ();
        S.currently := Idle;
        if gen = !S.gen
        then (
          tilepage l.pageno pageopaque !S.layout;
          load !S.layout;
          load preloadedpages;
          let visible = U.pagevisible !S.layout l.pageno in
          if visible
          then (
            match !S.mode with
            | LinkNav (Ltnotready (pageno, dir)) ->
               if pageno = l.pageno
               then (
                 let link =
                   let ld =
                     if dir = 0
                     then LDfirstvisible (l.pagex, l.pagey, dir)
                     else if dir > 0 then LDfirst else LDlast
                   in
                   Ffi.findlink pageopaque ld
                 in
                 match link with
                 | Lnotfound -> ()
                 | Lfound n ->
                    showlinktype (Ffi.getlink pageopaque n);
                    S.mode := LinkNav (Ltexact (l.pageno, n))
               )
            | LinkNav (Ltgendir _)
            | LinkNav (Ltexact _)
            | View
            | Birdseye _
            | Textentry _ -> ()
          );

          if visible && alltilesrendered !S.layout
          then Glutils.postRedisplay "page";
        )

     | Idle | Tiling _ | Outlining _ ->
        dolog "Inconsistent loading state";
        logcurrently !S.currently;
        exit 1
     end

  | "tile" , args ->
     (*
       C part is notifying us that it has finished rendering a tile
       valid = the tile fits current config (i.e. the settings with which
       the tile has been rendered match current ones)

       if the tile is not valid free it and issue loading/rendering commands
       for the current layout

       evict all the tiles that aren't part of preloadlayout
       if tile is visible post redisplay
       continue tiling
      *)
     let (x, y, opaques, size, t) =
       scan args "%u %u %s %u %f" (fun x y p size t -> (x, y, p, size, t))
     in
     let opaque = Opaque.of_string opaques in
     begin match !S.currently with
     | Tiling (l, pageopaque, cs, angle, gen, col, row, tilew, tileh) ->
        vlog "tile %d [%d,%d] took %f sec" l.pageno col row t;
        let layout =
          if conf.preload && alltilesrendered !S.layout
          then preloadlayout !S.x !S.y !S.winw !S.winh
          else !S.layout
        in
        if tilew != conf.tilew || tileh != conf.tileh
        then (
          wcmd1 U.freetile opaque;
          S.currently := Idle;
          load layout;
        )
        else (
          puttileopaque l col row gen cs angle opaque size t;
          S.memused := !S.memused + size;
          !S.uioh#infochanged Memused;
          gctilesnotinlayout layout;
          Queue.push ((l.pageno, gen, cs, angle, l.pagew, l.pageh, col, row),
                      opaque, size) S.tilelru;

          S.currently := Idle;
          let visible = tilevisible layout l.pageno x y in
          let cont = gen = !S.gen && conf.colorspace = cs
                     && conf.angle = angle && visible
          in

          if cont
          then conttiling l.pageno pageopaque;
          preload layout;
          if cont
          then Glutils.postRedisplay "tile nothrottle";
        )

     | Idle | Loading _ | Outlining _ ->
        dolog "Inconsistent tiling state";
        logcurrently !S.currently;
        exit 1
     end

  | "pdim", args ->
     let (n, w, h, _) as pdim =
       scan args "%u %d %d %d" (fun n x w h -> n, w, h, x)
     in
     let pdim =
       match conf.fitmodel with
       | FitWidth -> pdim
       | FitPage | FitProportional ->
          match conf.columns with
          | Csplit _ ->  (n, w, h, 0)
          | Csingle _ | Cmulti _ -> pdim
     in
     S.pdims := pdim :: !S.pdims;
     !S.uioh#infochanged Pdim

  | "o", args ->
     let (l, n, t, h, pos) =
       scan args "%u %u %d %u %n" (fun l n t h pos -> l, n, t, h, pos)
     in
     let s = String.sub args pos (String.length args - pos) in
     addoutline (s, l, Oanchor (n, float t /. float h, 0.0))

  | "ou", args ->
     let (l, len, pos) = scan args "%u %u %n" (fun l len pos -> l, len, pos) in
     let s = String.sub args pos len in
     let pos2 = pos + len + 1 in
     let uri = String.sub args pos2 (String.length args - pos2) in
     addoutline (s, l, Ouri uri)

  | "on", args ->
     let (l, pos) = scan args "%u %n" (fun l pos -> l, pos) in
     let s = String.sub args pos (String.length args - pos) in
     addoutline (s, l, Onone)

  | "a", args ->
     let (n, l, t) = scan args "%u %d %d" (fun n l t -> n, l, t) in
     S.reprf := (fun () -> gotopagexy n (float l) (float t))

  | "info", args ->
     let s =
       match splitatchar args '\t' with
       | "Title", "" ->
          settitle @@ Filename.basename !S.path;
          E.s
       | "Title", v ->
          settitle v;
          args
       | _, "" -> E.s
       | c, v ->
          if let len = String.length c in
             len > 6 && ((String.sub c (len-4) 4) = "date")
          then (
            if String.length v >= 7 && v.[0] = 'D' && v.[1] = ':'
            then
              let b = Buffer.create 10 in
              Printf.bprintf b "%s\t" c;
              let sub p l c =
                try
                  Buffer.add_substring b v p l;
                  Buffer.add_char b c;
                with exn -> Buffer.add_string b @@ exntos exn
              in
              sub 2 4 '/';
              sub 6 2 '/';
              sub 8 2 ' ';
              sub 10 2 ':';
              sub 12 2 ':';
              sub 14 2 ' ';
              Printf.bprintf b "[%s]" v;
              Buffer.contents b
            else args
          )
          else args
     in
     if nonemptystr s then S.docinfo := (1, s) :: !S.docinfo

  | "infoend", "" ->
     S.docinfo := List.rev !S.docinfo;
     !S.uioh#infochanged Docinfo

  | "pass", args ->
     if args = "fail"
     then adderrmsg "pass" "Wrong password";
     let password = getpassword () in
     if emptystr password
     then error "document is password protected"
     else opendoc !S.path !S.mimetype password

  | _ -> error "unknown cmd `%S'" cmds

let onhist cb =
  let rc = cb.rc in
  let action = function
    | HCprev  -> cbget cb ~-1
    | HCnext  -> cbget cb 1
    | HCfirst -> cbget cb ~-(cb.rc)
    | HClast  -> cbget cb (cb.len - 1 - cb.rc)
  and cancel () = cb.rc <- rc
  in (action, cancel)

let search pattern forward =
  match conf.columns with
  | Csplit _ ->
     impmsg "searching while in split columns mode is not implemented"
  | Csingle _ | Cmulti _ ->
     if nonemptystr pattern
     then
       let pn, py =
         match !S.layout with
         | [] -> 0, 0
         | l :: _ -> l.pageno, (l.pagey + if forward then 0 else 0*l.pagevh)
       in
       S.rects1 := [];
       wcmd U.search "%d %d %d %d,%s\000"
         (btod conf.icase) pn py (btod forward) pattern

let intentry text key =
  let text =
    if emptystr text && key = Keys.Ascii '-'
    then addchar text '-'
    else
      match [@warning "-fragile-match"] key with
      | Keys.Ascii ('0'..'9' as c) -> addchar text c
      | _ ->
         S.text := "invalid key";
         text
  in
  TEcont text

let linknact f s =
  if nonemptystr s
  then
    let rec loop off = function
      | [] -> ()
      | l :: rest ->
         match getopaque l.pageno with
         | exception Not_found -> loop off rest
         | opaque ->
            let n = Ffi.getlinkn opaque conf.hcs s off in
            if n <= 0
            then loop n rest
            else Ffi.getlink opaque (n-1) |> f
    in
    loop 0 !S.layout

let linknentry text = function [@warning "-fragile-match"]
  | Keys.Ascii c  ->
     let text = addchar text c in
     linknact (fun under -> S.text := undertext under) text;
     TEcont text
  | key ->
     settextfmt "invalid key %s" @@ Keys.to_string key;
     TEcont text

let textentry text key = match [@warning "-fragile-match"] key with
  | Keys.Ascii c -> TEcont (addchar text c)
  | Keys.Code c -> TEcont (text ^ Ffi.toutf8 c)
  | _ -> TEcont text

let reqlayout angle fitmodel =
  if U.nogeomcmds !S.geomcmds
  then S.anchor := getanchor ();
  conf.angle <- angle mod 360;
  if conf.angle != 0
  then (
    match !S.mode with
    | LinkNav _ -> S.mode := View
    | Birdseye _ | Textentry _ | View -> ()
  );
  conf.fitmodel <- fitmodel;
  invalidate "reqlayout"
    (fun () -> wcmd U.reqlayout "%d %d %d"
                 conf.angle (FMTE.to_int conf.fitmodel) (stateh !S.winh))

let settrim trimmargins trimfuzz =
  if U.nogeomcmds !S.geomcmds
  then S.anchor := getanchor ();
  conf.trimmargins <- trimmargins;
  conf.trimfuzz <- trimfuzz;
  let x0, y0, x1, y1 = trimfuzz in
  invalidate "settrim"
    (fun () -> wcmd U.settrim "%d %d %d %d %d"
                 (btod conf.trimmargins) x0 y0 x1 y1);
  flushpages ()

let setzoom zoom =
  let zoom = max 0.0001 zoom in
  if zoom <> conf.zoom
  then (
    S.prevzoom := (conf.zoom, !S.x);
    conf.zoom <- zoom;
    reshape !S.winw !S.winh;
    settextfmt "zoom is now %-5.2f" (zoom *. 100.0);
  )

let pivotzoom ?(vw=min !S.w !S.winw)
      ?(vh=min (!S.maxy - !S.y) !S.winh)
      ?(x=vw/2) ?(y=vh/2) zoom =
  let w = float !S.w /. zoom in
  let hw = w /. 2.0 in
  let ratio = float vh /. float vw in
  let hh = hw *. ratio in
  let x0 = float x -. hw +. !S.xf and y0 = float y -. hh +. !S.yf in
  let xf, xr = modf x0 and yf, yr = modf y0 in
  S.xf := xf;
  S.yf := yf;
  gotoxy (!S.x - truncate xr) (!S.y + truncate yr);
  setzoom zoom

let pivotzoom ?vw ?vh ?x ?y zoom =
  if U.nogeomcmds !S.geomcmds
  then
    if zoom > 1.0
    then pivotzoom ?vw ?vh ?x ?y zoom
    else setzoom zoom

let setcolumns mode columns coverA coverB =
  S.prevcolumns := Some (conf.columns, conf.zoom);
  if columns < 0
  then (
    if isbirdseye mode
    then impmsg "split mode doesn't work in bird's eye"
    else (
      conf.columns <- Csplit (-columns, E.a);
      S.x := 0;
      conf.zoom <- 1.0;
    );
  )
  else (
    if columns < 2
    then (
      conf.columns <- Csingle E.a;
      S.x := 0;
      setzoom 1.0;
    )
    else (
      conf.columns <- Cmulti ((columns, coverA, coverB), E.a);
      conf.zoom <- 1.0;
    );
  );
  reshape !S.winw !S.winh

let resetmstate () =
  S.mstate := Mnone;
  Wsi.setcursor Wsi.CURSOR_INHERIT

let enterbirdseye () =
  let zoom = float conf.thumbw /. float !S.winw in
  let birdseyepageno =
    let cy = !S.winh / 2 in
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
    fold !S.layout
  in
  S.mode :=
    Birdseye (
        { conf with zoom = conf.zoom },
        !S.x, birdseyepageno, -1, getanchor ()
      );
  resetmstate ();
  conf.zoom <- zoom;
  conf.presentation <- false;
  conf.interpagespace <- 10;
  conf.hlinks <- false;
  conf.fitmodel <- FitPage;
  S.x := 0;
  conf.columns <- (
    match conf.beyecolumns with
    | Some c ->
       conf.zoom <- 1.0;
       Cmulti ((c, 0, 0), E.a)
    | None -> Csingle E.a
  );
  if conf.verbose
  then settextfmt "birds eye on (zoom %3.1f%%)" (100.0*.zoom);
  reshape !S.winw !S.winh

let leavebirdseye (c, leftx, pageno, _, anchor) goback =
  S.mode := View;
  conf.zoom <- c.zoom;
  conf.presentation <- c.presentation;
  conf.interpagespace <- c.interpagespace;
  conf.hlinks <- c.hlinks;
  conf.fitmodel <- c.fitmodel;
  conf.beyecolumns <- (
    match conf.columns with
    | Cmulti ((c, _, _), _) -> Some c
    | Csingle _ -> None
    | Csplit _ -> error "leaving bird's eye split mode"
  );
  conf.columns <- (
    match c.columns with
    | Cmulti (c, _) -> Cmulti (c, E.a)
    | Csingle _ -> Csingle E.a
    | Csplit (c, _) -> Csplit (c, E.a)
  );
  if conf.verbose
  then settextfmt "bird's eye off (zoom %3.1f%%)" (100.0*.conf.zoom);
  reshape !S.winw !S.winh;
  S.anchor := if goback then anchor else (pageno, 0.0, 1.0);
  S.x := leftx

let togglebirdseye () =
  match !S.mode with
  | Birdseye vals -> leavebirdseye vals true
  | View -> enterbirdseye ()
  | Textentry _ | LinkNav _ -> ()

let upbirdseye incr (conf, leftx, pageno, hooverpageno, anchor) =
  let pageno = max 0 (pageno - incr) in
  let rec loop = function
    | [] -> gotopage1 pageno 0
    | l :: _ when l.pageno = pageno ->
       if l.pagedispy >= 0 && l.pagey = 0
       then Glutils.postRedisplay "upbirdseye"
       else gotopage1 pageno 0
    | _ :: rest -> loop rest
  in
  loop !S.layout;
  S.text := E.s;
  S.mode := Birdseye (conf, leftx, pageno, hooverpageno, anchor)

let downbirdseye incr (conf, leftx, pageno, hooverpageno, anchor) =
  let pageno = min (!S.pagecount - 1) (pageno + incr) in
  S.mode := Birdseye (conf, leftx, pageno, hooverpageno, anchor);
  let rec loop = function
    | [] ->
       let y, h = getpageyh pageno in
       let dy = (y - !S.y) - (!S.winh - h - conf.interpagespace) in
       gotoxy !S.x (U.add_to_y_and_clamp dy)
    | l :: _ when l.pageno = pageno ->
       if l.pagevh != l.pageh
       then
         let inc = l.pageh - l.pagevh + conf.interpagespace in
         gotoxy !S.x (U.add_to_y_and_clamp inc)
       else Glutils.postRedisplay "downbirdseye"
    | _ :: rest -> loop rest
  in
  loop !S.layout;
  S.text := E.s

let optentry mode _ key =
  match [@warning "-fragile-match"] key with
  | Keys.Ascii 'C' ->
     let ondone s =
       try
         let n, a, b = multicolumns_of_string s in
         setcolumns mode n a b;
       with exn -> settextfmt "bad columns `%s': %s" s @@ exntos exn
     in
     TEswitch ("columns: ", E.s, None, textentry, ondone, true)

  | Keys.Ascii 'Z' ->
     let ondone s =
       try
         let zoom = float (int_of_string s) /. 100.0 in
         pivotzoom zoom
       with exn -> settextfmt "bad integer `%s': %s" s @@ exntos exn
     in
     TEswitch ("zoom: ", E.s, None, intentry, ondone, true)

  | Keys.Ascii 'i' ->
     conf.icase <- not conf.icase;
     TEdone ("case insensitive search " ^ (onoffs conf.icase))

  | Keys.Ascii 'v' ->
     conf.verbose <- not conf.verbose;
     TEdone ("verbose " ^ (onoffs conf.verbose))

  | Keys.Ascii 'd' ->
     conf.debug <- not conf.debug;
     TEdone ("debug " ^ (onoffs conf.debug))

  | Keys.Ascii 'f' ->
     conf.underinfo <- not conf.underinfo;
     TEdone ("underinfo " ^ onoffs conf.underinfo)

  | Keys.Ascii 'T' ->
     settrim (not conf.trimmargins) conf.trimfuzz;
     TEdone ("trim margins " ^ onoffs conf.trimmargins)

  | Keys.Ascii 'I' ->
     conf.invert <- not conf.invert;
     TEdone ("invert colors " ^ onoffs conf.invert)

  | Keys.Ascii 'x' ->
     let ondone s =
       cbput !S.hists.sel s;
       conf.selcmd <- s;
     in
     TEswitch ("selection command: ", E.s, Some (onhist !S.hists.sel),
               textentry, ondone, true)

  | Keys.Ascii 'M' ->
     if conf.pax == None
     then conf.pax <- Some 0.0
     else conf.pax <- None;
     TEdone ("PAX " ^ onoffs (conf.pax != None))

  | (Keys.Ascii c) ->
     settextfmt "bad option %d `%c'" (Char.code c) c;
     TEstop

  | _ -> TEcont !S.text

class outlinelistview ~zebra ~source =
  let settext autonarrow s =
    S.text :=
      if autonarrow
      then
        let ss = source#statestr in
        if emptystr ss then "[" ^ s ^ "]" else "{" ^ ss ^ "} [" ^ s ^ "]"
      else s
  in
  object (self)
    inherit listview
              ~zebra
              ~helpmode:false
              ~source:(source :> lvsource)
              ~trusted:false
              ~modehash:(findkeyhash conf "outline")
            as super

    val m_autonarrow = false

    method! key key mask =
      let maxrows =
        if emptystr !S.text
        then fstate.maxrows
        else fstate.maxrows - 2
      in
      let calcfirst first active =
        if active > first
        then
          let rows = active - first in
          if rows > maxrows then active - maxrows else first
        else active
      in
      let navigate incr =
        let active = m_active + incr in
        let active = bound active 0 (source#getitemcount - 1) in
        let first = calcfirst m_first active in
        Glutils.postRedisplay "outline navigate";
        coe {< m_active = active; m_first = first >}
      in
      let navscroll first =
        let active =
          let dist = m_active - first in
          if dist < 0
          then first
          else (
            if dist < maxrows
            then m_active
            else first + maxrows
          )
        in
        Glutils.postRedisplay "outline navscroll";
        coe {< m_first = first; m_active = active >}
      in
      let ctrl = Wsi.withctrl mask in
      let open Keys in
      match Wsi.ks2kt key with
      | Ascii 'a' when ctrl ->
         let text =
           if m_autonarrow
           then (
             source#denarrow;
             E.s
           )
           else (
             let pattern = source#renarrow in
             if nonemptystr m_qsearch
             then (source#narrow m_qsearch; m_qsearch)
             else pattern
           )
         in
         settext (not m_autonarrow) text;
         Glutils.postRedisplay "toggle auto narrowing";
         coe {< m_first = 0; m_active = 0; m_autonarrow = not m_autonarrow >}
      | Ascii '/' when emptystr m_qsearch && not m_autonarrow ->
         settext true E.s;
         Glutils.postRedisplay "toggle auto narrowing";
         coe {< m_first = 0; m_active = 0; m_autonarrow = true >}
      | Ascii 'n' when ctrl ->
         source#narrow m_qsearch;
         if not m_autonarrow
         then source#add_narrow_pattern m_qsearch;
         Glutils.postRedisplay "outline ctrl-n";
         coe {< m_first = 0; m_active = 0 >}
      | Ascii 'S' when ctrl ->
         let active = source#calcactive (getanchor ()) in
         let first = firstof m_first active in
         Glutils.postRedisplay "outline ctrl-s";
         coe {< m_first = first; m_active = active >}
      | Ascii 'u' when ctrl ->
         Glutils.postRedisplay "outline ctrl-u";
         if m_autonarrow && nonemptystr m_qsearch
         then (
           ignore (source#renarrow);
           settext m_autonarrow E.s;
           coe {< m_first = 0; m_active = 0; m_qsearch = E.s >}
         )
         else (
           source#del_narrow_pattern;
           let pattern = source#renarrow in
           let text =
             if emptystr pattern then E.s else "Narrowed to " ^ pattern
           in
           settext m_autonarrow text;
           coe {< m_first = 0; m_active = 0; m_qsearch = E.s >}
         )
      | Ascii 'l' when ctrl ->
         let first = max 0 (m_active - (fstate.maxrows / 2)) in
         Glutils.postRedisplay "outline ctrl-l";
         coe {< m_first = first >}

      | Ascii '\t' when m_autonarrow ->
         if nonemptystr m_qsearch
         then (
           Glutils.postRedisplay "outline list view tab";
           source#add_narrow_pattern m_qsearch;
           settext true E.s;
           coe {< m_qsearch = E.s >}
         )
         else coe self
      | Escape when m_autonarrow ->
         if nonemptystr m_qsearch
         then source#add_narrow_pattern m_qsearch;
         super#key key mask
      | Enter when m_autonarrow ->
         if nonemptystr m_qsearch
         then source#add_narrow_pattern m_qsearch;
         super#key key mask
      | (Ascii _ | Code _) when m_autonarrow ->
         let pattern = m_qsearch ^ Ffi.toutf8 key in
         Glutils.postRedisplay "outlinelistview autonarrow add";
         source#narrow pattern;
         settext true pattern;
         coe {< m_first = 0; m_active = 0; m_qsearch = pattern >}
      | Backspace when m_autonarrow ->
         if emptystr m_qsearch
         then coe self
         else
           let pattern = withoutlastutf8 m_qsearch in
           Glutils.postRedisplay "outlinelistview autonarrow backspace";
           ignore (source#renarrow);
           source#narrow pattern;
           settext true pattern;
           coe {< m_first = 0; m_active = 0; m_qsearch = pattern >}
      | Up when ctrl -> navscroll (max 0 (m_first-1))
      | Down when ctrl -> navscroll (min (source#getitemcount-1) (m_first+1))
      | Up    -> navigate ~-1
      | Down  -> navigate 1
      | Prior -> navigate ~-(fstate.maxrows)
      | Next  -> navigate fstate.maxrows
      | Right ->
         (if ctrl
          then (
            Glutils.postRedisplay "outline ctrl right";
            {< m_pan = m_pan + 1 >}
          )
          else (
            if Wsi.withshift mask
            then self#nextcurlevel 1
            else self#updownlevel 1
         )) |> coe
      | Left ->
         (if ctrl
          then (
            Glutils.postRedisplay "outline ctrl left";
            {< m_pan = m_pan - 1 >}
          )
          else (
            if Wsi.withshift mask
            then self#nextcurlevel ~-1
            else self#updownlevel ~-1
         )) |> coe
      | Home ->
         Glutils.postRedisplay "outline home";
         coe {< m_first = 0; m_active = 0 >}
      | End ->
         let active = source#getitemcount - 1 in
         let first = max 0 (active - fstate.maxrows) in
         Glutils.postRedisplay "outline end";
         coe {< m_active = active; m_first = first >}
      | Delete|Escape|Insert|Enter|Ascii _|Code _|Ctrl _|Backspace|Fn _ ->
         super#key key mask
  end

let genhistoutlines () =
  Config.gethist ()
  |> List.sort (fun (_, c1, _, _, _, _) (_, c2, _, _, _, _) ->
         compare c2.lastvisit c1.lastvisit)
  |> List.map (fun ((path, c, _, _, _, origin) as hist) ->
         let path = if nonemptystr origin then origin else path in
         let base = Ffi.mbtoutf8 @@ Filename.basename path in
         (base ^ "\000" ^ c.title, 1, Ohistory hist)
       )

let gotohist (path, c, bookmarks, x, anchor, origin) =
  Config.save leavebirdseye;
  setconf conf c;
  let x0, y0, x1, y1 = conf.trimfuzz in
  wcmd U.trimset "%d %d %d %d %d" (btod conf.trimmargins) x0 y0 x1 y1;
  Wsi.reshape c.cwinw c.cwinh;
  opendoc path !S.mimetype origin;
  conf.zoom <- nan;
  setzoom c.zoom;
  S.anchor := anchor;
  S.bookmarks := bookmarks;
  S.origin := origin;
  S.x := x

let describe_layout layout =
  let d =
    match layout with
    | [] -> "Page 0"
    | l :: [] -> Printf.sprintf "Page %d" (l.pageno+1)
    | l :: rest ->
       let rangestr a b =
         if a.pageno = b.pageno then Printf.sprintf "%d" (a.pageno+1)
         else Printf.sprintf "%d%s%d" (a.pageno+1)
                (if a.pageno+1 = b.pageno then ", " else Utf8syms.ellipsis)
                (b.pageno+1)
       in
       let rec fold s la lb = function
         | [] -> Printf.sprintf "%s %s" s (rangestr la lb)
         | l :: rest when l.pageno = succ lb.pageno -> fold s la l rest
         | l :: rest -> fold (s ^ " " ^ rangestr la lb ^ ",") l l rest
       in
       fold "Pages" l l rest
  in
  let percent =
    let maxy = U.maxy () in
    if maxy <= 0
    then 100.
    else 100. *. (float !S.y /. float maxy)
  in
  Printf.sprintf "%s of %d [%.2f%%]" d !S.pagecount percent

let setpresentationmode v =
  let n = page_of_y !S.y in
  S.anchor := (n, 0.0, 1.0);
  conf.presentation <- v;
  if conf.fitmodel = FitPage
  then reqlayout conf.angle conf.fitmodel;
  represent ()

let infomenu =
  let modehash = lazy (findkeyhash conf "info") in (fun source ->
      S.text := E.s;
      new listview ~zebra:false ~helpmode:false ~source
        ~trusted:true ~modehash:(Lazy.force_val modehash) |> coe)

let enterinfomode =
  let btos b = if b then Utf8syms.radical else E.s in
  let showextended = ref false in
  let showcolors = ref false in
  let showcommands = ref false in
  let showrefl = ref false in
  let leave mode _ =  S.mode := mode in
  let src = object
      val mutable m_l = []
      val mutable m_a = E.a
      val mutable m_prev_uioh = nouioh
      val mutable m_prev_mode = View

      inherit lvsourcebase

      method reset prev_mode prev_uioh =
        m_a <- Array.of_list (List.rev m_l);
        m_l <- [];
        m_prev_mode <- prev_mode;
        m_prev_uioh <- prev_uioh;

      method int name get set =
        m_l <-
          (name, `int get, 1,
           Some (fun u ->
               let ondone s =
                 try set (int_of_string s)
                 with exn -> settextfmt "bad integer `%s': %s" s @@ exntos exn
               in
               S.text := E.s;
               let te = (name ^ ": ", E.s, None, intentry, ondone, true) in
               S.mode := Textentry (te, leave m_prev_mode);
               u
          )) :: m_l

      method int_with_suffix name get set =
        m_l <-
          (name, `intws get, 1,
           Some (fun u ->
               let ondone s =
                 try set (int_of_string_with_suffix s)
                 with exn -> settextfmt "bad integer `%s': %s" s @@ exntos exn
               in
               S.text := E.s;
               let te = (name ^ ": ", E.s, None, intentry_with_suffix,
                         ondone, true) in
               S.mode := Textentry (te, leave m_prev_mode);
               u
          )) :: m_l

      method bool ?(offset=1) ?(btos=btos) name get set =
        m_l <- (name, `bool (btos, get), offset,
                Some (fun u -> set (not (get ())); u)) :: m_l

      method color name get set =
        m_l <-
          (name, `color get, 1,
           Some (fun u ->
               let invalid = (nan, nan, nan) in
               let ondone s =
                 let c =
                   try color_of_string s
                   with exn -> settextfmt "bad color `%s': %s" s @@ exntos exn;
                               invalid
                 in
                 if c <> invalid
                 then set c;
               in
               let te = (name ^ ": ", E.s, None, textentry, ondone, true) in
               S.text := color_to_string (get ());
               S.mode := Textentry (te, leave m_prev_mode);
               u
          )) :: m_l

      method string name get set =
        m_l <-
          (name, `string get, 1,
           Some (fun u ->
               let ondone s = set s in
               let te = (String.trim name ^ ": ", E.s, None,
                         textentry, ondone, true) in
               S.mode := Textentry (te, leave m_prev_mode);
               u
          )) :: m_l

      method colorspace name get set =
        m_l <-
          (name, `string get, 1,
           Some (fun _ ->
               let source = object
                   inherit lvsourcebase

                   initializer
                      m_active <- CSTE.to_int conf.colorspace;
                      m_first <- 0;

                    method getitemcount =
                      Array.length CSTE.names
                    method getitem n =
                      (CSTE.names.(n), 0)
                    method exit ~uioh ~cancel ~active ~first ~pan =
                      ignore (uioh, first, pan);
                      if not cancel then set active;
                      None
                    method hasaction _ = true
                  end
               in
               infomenu source
          )) :: m_l

      method paxmark name get set =
        m_l <-
          (name, `string get, 1,
           Some (fun _ ->
               let source = object
                   inherit lvsourcebase

                    initializer
                      m_active <- MTE.to_int conf.paxmark;
                      m_first <- 0;

                    method getitemcount = Array.length MTE.names
                    method getitem n = (MTE.names.(n), 0)
                    method exit ~uioh ~cancel ~active ~first ~pan =
                      ignore (uioh, first, pan);
                      if not cancel then set active;
                      None
                    method hasaction _ = true
                 end
               in
               infomenu source
          )) :: m_l

      method fitmodel name get set =
        m_l <-
          (name, `string get, 1,
           Some (fun _ ->
               let source = object
                   inherit lvsourcebase

                   initializer
                      m_active <- FMTE.to_int conf.fitmodel;
                      m_first <- 0;

                    method getitemcount = Array.length FMTE.names
                    method getitem n = (FMTE.names.(n), 0)
                    method exit ~uioh ~cancel ~active ~first ~pan =
                      ignore (uioh, first, pan);
                      if not cancel then set active;
                      None
                    method hasaction _ = true
                  end
               in
               infomenu source
          )) :: m_l

      method caption s offset =
        m_l <- (s, `empty, offset, None) :: m_l

      method caption2 s f offset =
        m_l <- (s, `string f, offset, None) :: m_l

      method getitemcount = Array.length m_a

      method getitem n =
        let tostr = function
          | `int f -> string_of_int (f ())
          | `intws f -> string_with_suffix_of_int (f ())
          | `string f -> f ()
          | `color f -> color_to_string (f ())
          | `bool (btos, f) -> btos (f ())
          | `empty -> E.s
        in
        let name, t, offset, _ = m_a.(n) in
        ((let s = tostr t in
          if nonemptystr s
          then Printf.sprintf "%s\t%s" name s
          else name),
         offset)

      method exit ~uioh ~cancel ~active ~first ~pan =
        let uiohopt =
          if not cancel
          then (
            let uioh =
              match m_a.(active) with
              | _, _, _, Some f -> f uioh
              | _, _, _, None -> uioh
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
        | _, _, _, Some _ -> true
        | _, _, _, None -> false

      initializer m_active <- 1
    end
  in
  let rec fillsrc prevmode prevuioh =
    let sep () = src#caption E.s 0 in
    let bad v exn = settextfmt "bad color `%s': %s" v @@ exntos exn in
    let colorp name get set =
      src#string name
        (fun () -> color_to_string (get ()))
        (fun v ->
          try set @@ color_of_string v
          with exn -> bad v exn
        )
    in
    let rgba name get set =
      src#string name
        (fun () -> get () |> rgba_to_string)
        (fun v ->
          try set @@ rgba_of_string v
          with exn -> bad v exn
        )
    in
    let oldmode = !S.mode in
    let birdseye = isbirdseye !S.mode in

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

    src#fitmodel "fit model"
      (fun () -> FMTE.to_string conf.fitmodel)
      (fun v -> reqlayout conf.angle (FMTE.of_int v));

    src#bool "trim margins"
      (fun () -> conf.trimmargins)
      (fun v -> settrim v conf.trimfuzz; fillsrc prevmode prevuioh);

    sep ();
    src#int "inter-page space"
      (fun () -> conf.interpagespace)
      (fun n ->
        conf.interpagespace <- n;
        docolumns conf.columns;
        let pageno, py =
          match !S.layout with
          | [] -> 0, 0
          | l :: _ -> l.pageno, l.pagey
        in
        S.maxy :=- calcheight ();
        gotoxy !S.x (py + getpagey pageno)
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
        match !S.autoscroll with
        | Some step -> step
        | _ -> conf.autoscrollstep)
      (fun n ->
        let n = boundastep !S.winh n in
        if !S.autoscroll <> None
        then S.autoscroll := Some n;
        conf.autoscrollstep <- n);

    src#int "zoom"
      (fun () -> truncate (conf.zoom *. 100.))
      (fun v -> pivotzoom ((float v) /. 100.));

    src#int "rotation"
      (fun () -> conf.angle)
      (fun v -> reqlayout v conf.fitmodel);

    src#int "scroll bar width"
      (fun () -> conf.scrollbw)
      (fun v ->
        conf.scrollbw <- v;
        reshape !S.winw !S.winh;
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
        | Textentry _ | View | LinkNav _ -> ()
      );

    let mode = !S.mode in
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
      (fun () ->
        Printf.sprintf "%s bytes, %d tiles"
          (string_with_suffix_of_int !S.memused)
          (Hashtbl.length S.tilemap)) 1;

    sep ();
    src#caption "Layout" 0;
    src#caption2 "Dimension"
      (fun () -> Printf.sprintf "%dx%d (virtual %dx%d)"
                   !S.winw !S.winh
                   !S.w !S.maxy)
      1;
    if conf.debug
    then src#caption2 "Position" (fun () ->
             Printf.sprintf "%dx%d" !S.x !S.y
           ) 1
    else src#caption2 "Position" (fun () -> describe_layout !S.layout) 1;

    sep ();
    let btos b = Utf8syms.(if b then lguillemet else rguillemet) in
    src#bool ~offset:0 ~btos "Extended parameters"
      (fun () -> !showextended)
      (fun v -> showextended := v; fillsrc prevmode prevuioh);
    if !showextended
    then (
      src#bool "update cursor"
        (fun () -> conf.updatecurs)
        (fun v -> conf.updatecurs <- v);
      src#bool "scroll-bar on the left"
        (fun () -> conf.leftscroll)
        (fun v -> conf.leftscroll <- v);
      src#bool "verbose"
        (fun () -> conf.verbose)
        (fun v -> conf.verbose <- v);
      src#bool "invert colors"
        (fun () -> conf.invert)
        (fun v -> conf.invert <- v);
      src#bool "max fit"
        (fun () -> conf.maxhfit)
        (fun v -> conf.maxhfit <- v);
      src#bool "pax mode"
        (fun () -> conf.pax != None)
        (fun v ->
          if v
          then conf.pax <- Some (now ())
          else conf.pax <- None);
      src#string "tile size"
        (fun () -> Printf.sprintf "%dx%d" conf.tilew conf.tileh)
        (fun v ->
          try
            let w, h = Scanf.sscanf v "%dx%d" (fun w h -> w, h) in
            conf.tilew <- max 64 w;
            conf.tileh <- max 64 h;
            flushtiles ();
          with exn -> settextfmt  "bad tile size `%s': %s" v @@ exntos exn);
      src#int "texture count"
        (fun () -> conf.texcount)
        (fun v ->
          if Ffi.realloctexts v
          then conf.texcount <- v
          else impmsg "failed to set texture count please retry later");
      src#int "slice height"
        (fun () -> conf.sliceheight)
        (fun v ->
          conf.sliceheight <- v;
          wcmd U.sliceh "%d" conf.sliceheight);
      src#int "anti-aliasing level"
        (fun () -> conf.aalevel)
        (fun v ->
          conf.aalevel <- bound v 0 8;
          S.anchor := getanchor ();
          opendoc !S.path !S.mimetype !S.password);
      src#string "page scroll scaling factor"
        (fun () -> string_of_float conf.pgscale)
        (fun v ->
          try conf.pgscale <- float_of_string v
          with exn ->
            S.text :=
              Printf.sprintf "bad page scroll scaling factor `%s': %s" v
              @@ exntos exn);
      src#int "ui font size"
        (fun () -> fstate.fontsize)
        (fun v -> setfontsize (bound v 5 100));
      src#int "hint font size"
        (fun () -> conf.hfsize)
        (fun v -> conf.hfsize <- bound v 5 100);
      src#string "hint chars"
        (fun () -> conf.hcs)
        (fun v ->
          try
            validatehcs v;
            conf.hcs <- v
          with exn ->
            S.text :=
              Printf.sprintf "invalid hint chars %S: %s" v (exntos exn));
      src#string "trim fuzz"
        (fun () -> irect_to_string conf.trimfuzz)
        (fun v ->
          try
            conf.trimfuzz <- irect_of_string v;
            if conf.trimmargins
            then settrim true conf.trimfuzz;
          with exn -> settextfmt "bad irect `%s': %s" v @@ exntos exn);
      src#bool ~btos "external commands"
        (fun () -> !showcommands)
        (fun v -> showcommands := v; fillsrc prevmode prevuioh);
      if !showcommands
      then (
        src#string "  uri launcher"
          (fun () -> conf.urilauncher)
          (fun v -> conf.urilauncher <- v);
        src#string "  path launcher"
          (fun () -> conf.pathlauncher)
          (fun v -> conf.pathlauncher <- v);
        src#string "  selection"
          (fun () -> conf.selcmd)
          (fun v -> conf.selcmd <- v);
        src#string "  synctex"
          (fun () -> conf.stcmd)
          (fun v -> conf.stcmd <- v);
        src#string "  pax"
          (fun () -> conf.paxcmd)
          (fun v -> conf.paxcmd <- v);
        src#string "  ask password"
          (fun () -> conf.passcmd)
          (fun v -> conf.passcmd <- v);
        src#string "  save path"
          (fun () -> conf.savecmd)
          (fun v -> conf.savecmd <- v);
      );
      src#colorspace "color space"
        (fun () -> CSTE.to_string conf.colorspace)
        (fun v ->
          conf.colorspace <- CSTE.of_int v;
          wcmd U.cs "%d" v;
          load !S.layout);
      src#paxmark "pax mark method"
        (fun () -> MTE.to_string conf.paxmark)
        (fun v -> conf.paxmark <- MTE.of_int v);
      src#bool "mouse wheel scrolls pages"
        (fun () -> conf.wheelbypage)
        (fun v -> conf.wheelbypage <- v);
      src#bool "open remote links in a new instance"
        (fun () -> conf.riani)
        (fun v -> conf.riani <- v);
      src#bool "edit annotations inline"
        (fun () -> conf.annotinline)
        (fun v -> conf.annotinline <- v);
      src#bool "coarse positioning in presentation mode"
        (fun () -> conf.coarseprespos)
        (fun v -> conf.coarseprespos <- v);
      src#bool "use document CSS"
        (fun () -> conf.usedoccss)
        (fun v ->
          conf.usedoccss <- v;
          S.anchor := getanchor ();
          opendoc !S.path !S.mimetype !S.password);
      src#bool ~btos "colors"
        (fun () -> !showcolors)
        (fun v -> showcolors := v; fillsrc prevmode prevuioh);
      if !showcolors
      then (
        colorp "   background"
          (fun () -> conf.bgcolor)
          (fun v -> conf.bgcolor <- v);
        rgba "   paper"
          (fun () -> conf.papercolor)
          (fun v ->
            conf.papercolor <- v;
            Ffi.setpapercolor conf.papercolor;
            flushtiles ();
          );
        rgba "   scrollbar"
          (fun () -> conf.sbarcolor)
          (fun v -> conf.sbarcolor <- v);
        rgba "   scrollbar handle"
          (fun () -> conf.sbarhndlcolor)
          (fun v -> conf.sbarhndlcolor <- v);
        rgba "   texture"
          (fun () -> conf.texturecolor)
          (fun v ->
            GlTex.env (`color v);
            conf.texturecolor <- v;
          );
        src#string "   scale"
          (fun () -> string_of_float conf.colorscale)
          (fun v -> conf.colorscale <- bound (float_of_string v) 0.0 1.0);
      );
      src#bool ~btos "reflowable layout"
        (fun () -> !showrefl)
        (fun v -> showrefl := v; fillsrc prevmode prevuioh);
      if !showrefl
      then (
        src#int "  width"
          (fun () -> conf.rlw)
          (fun v -> conf.rlw <- v; reload ());
        src#int "  height"
          (fun () -> conf.rlh)
          (fun v -> conf.rlh <- v; reload ());
        src#int "  em"
          (fun () -> conf.rlem)
          (fun v -> conf.rlem <- v; reload ());
      );
    );

    sep ();
    src#caption "Document" 0;
    List.iter (fun (_, s) -> src#caption s 1) !S.docinfo;
    src#caption2 "Pages" (fun () ->  string_of_int !S.pagecount) 1;
    src#caption2 "Dimensions"
      (fun () -> string_of_int (List.length !S.pdims)) 1;
    if nonemptystr conf.css
    then src#caption2 "CSS" (fun () -> conf.css) 1;
    if conf.trimmargins
    then (
      sep ();
      src#caption "Trimmed margins" 0;
      src#caption2 "Dimensions"
        (fun () -> string_of_int (List.length !S.pdims)) 1;
    );

    sep ();
    src#caption "OpenGL" 0;
    src#caption ("Vendor\t" ^ GlMisc.get_string `vendor) 1;
    src#caption ("Renderer\t" ^ GlMisc.get_string `renderer) 1;

    sep ();
    src#caption "Location" 0;
    if nonemptystr !S.origin
    then src#caption ("Origin\t" ^ Ffi.mbtoutf8 !S.origin) 1;
    src#caption ("Path\t" ^ Ffi.mbtoutf8 !S.path) 1;
    if nonemptystr conf.dcf
    then src#caption ("DCF\t" ^ Ffi.mbtoutf8 conf.dcf) 1;

    src#reset prevmode prevuioh;
  in
  fun () -> (
    S.text := E.s;
    resetmstate ();
    let prevmode = !S.mode
    and prevuioh = !S.uioh in
    fillsrc prevmode prevuioh;
    let source = (src :> lvsource) in
    let modehash = findkeyhash conf "info" in
    object (self)
      inherit listview ~zebra:false ~helpmode:false
                ~source ~trusted:true ~modehash as super
      val mutable m_prevmemused = 0
      method! infochanged = function
        | Memused ->
           if m_prevmemused != !S.memused
           then (
             m_prevmemused <- !S.memused;
             Glutils.postRedisplay "memusedchanged";
           )
        | Pdim -> Glutils.postRedisplay "pdimchanged"
        | Docinfo -> fillsrc prevmode prevuioh
      method! key key mask =
        if not (Wsi.withctrl mask)
        then
          match [@warning "-fragile-match"] Wsi.ks2kt key with
          | Keys.Left  -> coe (self#updownlevel ~-1)
          | Keys.Right -> coe (self#updownlevel 1)
          | _ -> super#key key mask
        else super#key key mask
    end |> setuioh;
    Glutils.postRedisplay "info";
  )

let enterhelpmode =
  let source = object
      inherit lvsourcebase
      method getitemcount = Array.length !S.help
      method getitem n =
        let s, l, _ = !S.help.(n) in
        (s, l)

      method exit ~uioh ~cancel ~active ~first ~pan =
        let optuioh =
          if not cancel
          then (
            match !S.help.(active) with
            | _, _, Some f -> Some (f uioh)
            | _, _, None -> Some uioh
          )
          else None
        in
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        optuioh

      method hasaction n =
        match !S.help.(n) with
        | _, _, Some _ -> true
        | _, _, None -> false

      initializer m_active <- -1
    end
  in fun () ->
     let modehash = findkeyhash conf "help" in
     resetmstate ();
     new listview ~zebra:false ~helpmode:true
       ~source ~trusted:true ~modehash |> setuioh;
     Glutils.postRedisplay "help"

let entermsgsmode =
  let msgsource = object
      inherit lvsourcebase
      val mutable m_items = E.a

      method getitemcount = 1 + Array.length m_items

      method getitem n =
        if n = 0
        then "[Clear]", 0
        else m_items.(n-1), 0

      method exit ~uioh ~cancel ~active ~first ~pan =
        ignore uioh;
        if not cancel
        then (
          if active = 0
          then Buffer.clear S.errmsgs;
        );
        m_active <- active;
        m_first <- first;
        m_pan <- pan;
        None

      method hasaction n =
        n = 0

      method reset =
        S.newerrmsgs := false;
        let l = Str.split Re.crlf (Buffer.contents S.errmsgs) in
        m_items <- Array.of_list l

      initializer m_active <- 0
    end
  in
  fun () ->
  S.text := E.s;
  resetmstate ();
  msgsource#reset;
  let source = (msgsource :> lvsource) in
  let modehash = findkeyhash conf "listview" in
  object
    inherit listview ~zebra:false ~helpmode:false
              ~source ~trusted:false ~modehash as super
    method! display =
      if !S.newerrmsgs
      then msgsource#reset;
      super#display
  end |> setuioh;
  Glutils.postRedisplay "msgs"

let getusertext s =
  let editor = getenvdef "EDITOR" E.s in
  if emptystr editor
  then E.s
  else
    let tmppath = Filename.temp_file "llpp" "note" in
    if nonemptystr s
    then (
      let oc = open_out tmppath in
      output_string oc s;
      close_out oc;
    );
    let execstr = editor ^ " " ^ tmppath in
    let eret r = Printf.ksprintf (fun s -> adderrmsg "gtut:eret" s; r) in
    let s =
      match spawn execstr [] with
      | exception exn -> eret E.s "spawn(%S) failed: %s" execstr @@ exntos exn
      | pid ->
         match Unix.waitpid [] pid with
         | exception exn -> eret E.s "waitpid(%d) failed: %s" pid @@ exntos exn
         | (_pid, status) ->
            match status with
            | Unix.WEXITED 0 -> filecontents tmppath
            | Unix.WEXITED n ->
               eret E.s "editor process(%s) exited abnormally: %d" execstr n
            | Unix.WSIGNALED n ->
               eret E.s "editor process(%s) was killed by signal %d" execstr n
            | Unix.WSTOPPED n ->
               eret E.s "editor(%s) process was stopped by signal %d" execstr n
    in
    match Unix.unlink tmppath with
    | exception exn -> eret s "failed to ulink %S: %s" tmppath @@ exntos exn
    | () -> s

let enterannotmode opaque slinkindex =
  let msgsource = object
      inherit lvsourcebase
      val mutable m_text = E.s
      val mutable m_items = E.a

      method getitemcount = Array.length m_items

      method getitem n =
        let label, _func = m_items.(n) in
        label, 0

      method exit ~uioh ~cancel ~active ~first ~pan =
        ignore (uioh, first, pan);
        if not cancel
        then (
          let _label, func = m_items.(active) in
          func ()
        );
        None

      method hasaction n = nonemptystr @@ fst m_items.(n)

      method reset s =
        let rec split accu b i =
          let p = b+i in
          if p = String.length s
          then (String.sub s b (p-b), fun () -> ()) :: accu
          else
            if (i > 70 && s.[p] = ' ') || s.[p] = '\r' || s.[p] = '\n'
            then
              let ss = if i = 0 then E.s else String.sub s b i in
              split ((ss, fun () -> ())::accu) (p+1) 0
            else split accu b (i+1)
        in
        let cleanup () =
          wcmd1 U.freepage opaque;
          let keys =
            Hashtbl.fold (fun key opaque' accu ->
                if opaque' = opaque'
                then key :: accu else accu) S.pagemap []
          in
          List.iter (Hashtbl.remove S.pagemap) keys;
          flushtiles ();
          gotoxy !S.x !S.y
        in
        let dele () =
          Ffi.delannot opaque slinkindex;
          cleanup ();
        in
        let edit inline () =
          let update s =
            if emptystr s
            then dele ()
            else (
              Ffi.modannot opaque slinkindex s;
              cleanup ();
            )
          in
          if inline
          then
            let mode = !S.mode in
            let te = ("annotation: ", m_text, None, textentry, update, true) in
            S.mode := Textentry (te, fun _ -> S.mode := mode);
            S.text := E.s;
            enttext ();
          else getusertext m_text |> update
        in
        m_text <- s;
        m_items <-
          (   "[Copy]", fun () -> selstring conf.selcmd m_text)
          :: ("[Delete]", dele)
          :: ("[Edit]", edit conf.annotinline)
          :: (E.s, fun () -> ())
          :: split [] 0 0 |> List.rev |> Array.of_list

      initializer m_active <- 0
    end
  in
  S.text := E.s;
  let s = Ffi.gettextannot opaque slinkindex in
  resetmstate ();
  msgsource#reset s;
  let source = (msgsource :> lvsource) in
  let modehash = findkeyhash conf "listview" in
  object inherit listview ~zebra:false
                   ~helpmode:false ~source ~trusted:false ~modehash
  end |> setuioh;
  Glutils.postRedisplay "enterannotmode"

let gotoremote spec =
  let filename, dest = splitatchar spec '#' in
  let getpath filename =
    let path =
      if nonemptystr filename
      then
        if Filename.is_relative filename
        then
          let dir = Filename.dirname !S.path in
          let dir =
            if Filename.is_implicit dir
            then Filename.concat (Sys.getcwd ()) dir
            else dir
          in
          Filename.concat dir filename
        else filename
      else E.s
    in
    if Sys.file_exists path
    then path
    else E.s
  in
  let path = getpath filename in
  if emptystr path
  then adderrfmt "gotoremote/getpath" "failed getpath for %S\n" filename
  else
    let dospawn lcmd =
      if conf.riani
      then
        let cmd = Lazy.force_val lcmd in
        match spawn cmd with
        | exception exn -> dolog "failed to execute `%s': %s" cmd @@ exntos exn
        | _pid -> ()
      else
        let anchor = getanchor () in
        let ranchor = !S.path, !S.mimetype, !S.password, anchor, !S.origin in
        S.origin := E.s;
        S.ranchors := ranchor :: !S.ranchors;
        opendoc path E.s E.s;
    in
    if substratis spec 0 "page="
    then
      match Scanf.sscanf spec "page=%d" (fun n -> n) with
      | exception exn ->
         adderrfmt "error parsing remote destination" "%s %s" spec @@ exntos exn
      | pageno ->
         S.anchor := (pageno, 0.0, 0.0);
         dospawn @@ lazy (Printf.sprintf "%s -page %d %S"
                            !S.selfexec pageno path);
    else (
      S.nameddest := dest;
      dospawn @@ lazy (!S.selfexec ^ " " ^ path ^ " -dest " ^ dest)
    )

let gotounder = function
  | Ulinkuri s when Ffi.isexternallink s ->
     if substratis s 0 "file://"
     then gotoremote @@ String.sub s 7 (String.length s - 7)
     else Help.gotouri conf.urilauncher s
  | Ulinkuri s ->
     let pageno, x, y = Ffi.uritolocation s in
     addnav ();
     gotopagexy pageno x y
  | Utext _ | Unone -> ()
  | Utextannot (opaque, slinkindex) -> enterannotmode opaque slinkindex
  | Ufileannot (opaque, slinkindex) ->
     if emptystr conf.savecmd
     then adderrmsg "savepath-command is empty"
            "don't know where to save attachment"
     else
       let filename = Ffi.getfileannot opaque slinkindex in
       let savecmd = Str.global_replace Re.percents filename conf.savecmd in
       let path =
         getcmdoutput
           (adderrfmt savecmd
              "failed to obtain path to the saved attachment: %s") savecmd
       in
       Ffi.savefileannot opaque slinkindex path

let gotooutline (_, _, kind) =
  match kind with
  | Onone -> ()
  | Oanchor ((pageno, y, _) as anchor) ->
     addnav ();
     gotoxy !S.x @@
       getanchory (if conf.presentation then (pageno, y, 1.0) else anchor)
  | Ouri uri -> gotounder (Ulinkuri uri)
  | Olaunch cmd -> error "gotounder (Ulaunch %S)" cmd
  | Oremote (remote, pageno) ->
     error "gotounder (Uremote (%S,%d) )" remote pageno
  | Ohistory hist -> gotohist hist
  | Oremotedest (path, dest) ->
     error "gotounder (Uremotedest (%S, %S))" path dest

class outlinesoucebase fetchoutlines = object (self)
  inherit lvsourcebase
  val mutable m_items = E.a
  val mutable m_minfo = E.a
  val mutable m_orig_items = E.a
  val mutable m_orig_minfo = E.a
  val mutable m_narrow_patterns = []
  val mutable m_gen = -1

  method getitemcount = Array.length m_items

  method getitem n =
    let s, n, _ = m_items.(n) in
    (s, n+0)

  method exit ~(uioh:uioh) ~cancel ~active ~(first:int) ~pan : uioh option =
    ignore (uioh, first);
    let items, minfo =
      if m_narrow_patterns = []
      then m_orig_items, m_orig_minfo
      else m_items, m_minfo
    in
    m_pan <- pan;
    if not cancel
    then (
      m_items <- items;
      m_minfo <- minfo;
      gotooutline m_items.(active);
    )
    else (
      m_items <- items;
      m_minfo <- minfo;
    );
    None

  method hasaction (_:int) = true

  method greetmsg =
    if Array.length m_items != Array.length m_orig_items
    then
      let s =
        match m_narrow_patterns with
        | one :: [] -> one
        | many -> String.concat Utf8syms.ellipsis (List.rev many)
      in
      "Narrowed to " ^ s ^ " (ctrl-u to restore)"
    else E.s

  method statestr =
    match m_narrow_patterns with
    | [] -> E.s
    | one :: [] -> one
    | head :: _ -> Utf8syms.ellipsis ^ head

  method narrow pattern =
    match Str.regexp_case_fold pattern with
    | exception _ -> ()
    | re ->
       let rec loop accu minfo n =
         if n = -1
         then (
           m_items <- Array.of_list accu;
           m_minfo <- Array.of_list minfo;
         )
         else
           let (s, _, _) as o = m_items.(n) in
           let accu, minfo =
             match Str.search_forward re s 0 with
             | exception Not_found -> accu, minfo
             | first -> o :: accu, (first, Str.match_end ()) :: minfo
           in
           loop accu minfo (n-1)
       in
       loop [] [] (Array.length m_items - 1)

  method! getminfo = m_minfo

  method denarrow =
    m_orig_items <- fetchoutlines ();
    m_minfo <- m_orig_minfo;
    m_items <- m_orig_items

  method add_narrow_pattern pattern =
    m_narrow_patterns <- pattern :: m_narrow_patterns

  method del_narrow_pattern =
    match m_narrow_patterns with
    | _ :: rest -> m_narrow_patterns <- rest
    | [] -> ()

  method renarrow =
    self#denarrow;
    match m_narrow_patterns with
    | pattern :: [] -> self#narrow pattern; pattern
    | list ->
       List.fold_left (fun accu pattern ->
           self#narrow pattern;
           pattern ^ Utf8syms.ellipsis ^ accu) E.s list

  method calcactive (_:anchor) = 0

  method reset anchor items =
    if !S.gen != m_gen
    then (
      m_orig_items <- items;
      m_items <- items;
      m_narrow_patterns <- [];
      m_minfo <- E.a;
      m_orig_minfo <- E.a;
      m_gen <- !S.gen;
    )
    else (
      if items != m_orig_items
      then (
        m_orig_items <- items;
        if m_narrow_patterns == []
        then m_items <- items;
      )
    );
    let active = self#calcactive anchor in
    m_active <- active;
    m_first <- firstof m_first active
end

let outlinesource fetchoutlines = object
    inherit outlinesoucebase fetchoutlines
    method! calcactive anchor =
      let rely = getanchory anchor in
      let rec loop n best bestd =
        if n = Array.length m_items
        then best
        else
          let _, _, kind = m_items.(n) in
          match kind with
          | Oanchor anchor ->
             let orely = getanchory anchor in
             let d = abs (orely - rely) in
             if d < bestd
             then loop (n+1) n d
             else loop (n+1) best bestd
          | Onone | Oremote _ | Olaunch _
          | Oremotedest _ | Ouri _ | Ohistory _ ->
             loop (n+1) best bestd
      in
      loop 0 ~-1 max_int
  end

let enteroutlinemode, enterbookmarkmode, enterhistmode =
  let fetchoutlines sourcetype () =
    match sourcetype with
    | `bookmarks -> Array.of_list !S.bookmarks
    | `outlines -> !S.outlines
    | `history -> genhistoutlines () |> Array.of_list
  in
  let so = outlinesource (fetchoutlines `outlines) in
  let sb = outlinesource (fetchoutlines `bookmarks) in
  let sh = outlinesource (fetchoutlines `history) in
  let mkselector sourcetype source =
    (fun emptymsg ->
      let outlines = fetchoutlines sourcetype () in
      if Array.length outlines = 0
      then showtext ' ' emptymsg
      else (
        resetmstate ();
        Wsi.setcursor Wsi.CURSOR_INHERIT;
        let anchor = getanchor () in
        source#reset anchor outlines;
        S.text := source#greetmsg;
        new outlinelistview ~zebra:(sourcetype=`history) ~source |> setuioh;
        Glutils.postRedisplay "enter selector";
      )
    )
  in
  let mkenter src errmsg s = fun () -> mkselector src s errmsg in
  ( mkenter `outlines "document has no outline" so
  , mkenter `bookmarks "document has no bookmarks (yet)" sb
  , mkenter `history "history is empty" sh )

let addbookmark title a =
  let b = List.filter (fun (title', _, _) -> title <> title') !S.bookmarks in
  S.bookmarks := (title, 0, Oanchor a) :: b

let quickbookmark ?title () =
  match !S.layout with
  | [] -> ()
  | l :: _ ->
     let title =
       match title with
       | None ->
          Unix.(
           let tm = localtime (now ()) in
           Printf.sprintf
             "Quick (page %d) (bookmarked on %02d/%02d/%d at %02d:%02d)"
             (l.pageno+1)
             tm.tm_mday (tm.tm_mon+1) (tm.tm_year+1900) tm.tm_hour tm.tm_min
          )
       | Some title -> title
     in
     addbookmark title (getanchor1 l)

let setautoscrollspeed step goingdown =
  let incr = max 1 ((abs step) / 2) in
  let incr = if goingdown then incr else -incr in
  let astep = boundastep !S.winh (step + incr) in
  S.autoscroll := Some astep

let canpan () =
  match conf.columns with
  | Csplit _ -> true
  | Csingle _ | Cmulti _ -> !S.x != 0 || conf.zoom > 1.0

let existsinrow pageno (columns, coverA, coverB) p =
  let last = ((pageno - coverA) mod columns) + columns in
  let rec any = function
    | [] -> false
    | l :: rest ->
       if l.pageno = coverA - 1 || l.pageno = !S.pagecount - coverB
       then p l
       else (
         if not (p l)
         then (if l.pageno = last then false else any rest)
         else true
       )
  in
  any !S.layout

let nextpage () =
  match !S.layout with
  | [] ->
     let pageno = page_of_y !S.y in
     gotoxy !S.x (getpagey (pageno+1))
  | l :: rest ->
     match conf.columns with
     | Csingle _ ->
        if conf.presentation && rest == [] && l.pageh > l.pagey + l.pagevh
        then
          let y = U.add_to_y_and_clamp (U.pgscale !S.winh) in
          gotoxy !S.x y
        else
          let pageno = min (l.pageno+1) (!S.pagecount-1) in
          gotoxy !S.x (getpagey pageno)
     | Cmulti ((c, _, _) as cl, _) ->
        if conf.presentation
           && (existsinrow l.pageno cl
                 (fun l -> l.pageh > l.pagey + l.pagevh))
        then
          let y = U.add_to_y_and_clamp (U.pgscale !S.winh) in
          gotoxy !S.x y
        else
          let pageno = min (l.pageno+c) (!S.pagecount-1) in
          gotoxy !S.x (getpagey pageno)
     | Csplit (n, _) ->
        if l.pageno < !S.pagecount - 1 || l.pagecol < n - 1
        then
          let pagey, pageh = getpageyh l.pageno in
          let pagey = pagey + pageh * l.pagecol in
          let ips = if l.pagecol = 0 then 0 else conf.interpagespace in
          gotoxy !S.x (pagey + pageh + ips)

let prevpage () =
  match !S.layout with
  | [] ->
     let pageno = page_of_y !S.y in
     gotoxy !S.x (getpagey (pageno-1))
  | l :: _ ->
     match conf.columns with
     | Csingle _ ->
        if conf.presentation && l.pagey != 0
        then gotoxy !S.x (U.add_to_y_and_clamp (U.pgscale ~-(!S.winh)))
        else
          let pageno = max 0 (l.pageno-1) in
          gotoxy !S.x (getpagey pageno)
     | Cmulti ((c, _, coverB) as cl, _) ->
        if conf.presentation &&
             (existsinrow l.pageno cl (fun l -> l.pagey != 0))
        then gotoxy !S.x (U.add_to_y_and_clamp (U.pgscale ~-(!S.winh)))
        else
          let decr =
            if l.pageno = !S.pagecount - coverB
            then 1
            else c
          in
          let pageno = max 0 (l.pageno-decr) in
          gotoxy !S.x (getpagey pageno)
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
        gotoxy !S.x y

let save () =
  if emptystr conf.savecmd
  then adderrmsg "savepath-command is empty"
         "don't know where to save modified document"
  else
    let savecmd = Str.global_replace Re.percents !S.path conf.savecmd in
    let path =
      getcmdoutput
        (adderrfmt savecmd "failed to obtain path to the saved copy: %s")
        savecmd
    in
    if nonemptystr path
    then
      let tmp = path ^ ".tmp" in
      Ffi.savedoc tmp;
      Unix.rename tmp path

let viewkeyboard key mask =
  let enttext te =
    let mode = !S.mode in
    S.mode := Textentry (te, fun _ -> S.mode := mode);
    S.text := E.s;
    enttext ();
    Glutils.postRedisplay "view:enttext"
  and histback () =
    match !S.nav.past with
    | [] -> ()
    | prev :: prest ->
       S.nav := { past = prest ; future = getanchor () :: !S.nav.future; };
       gotoxy !S.x (getanchory prev)
  in
  let ctrl = Wsi.withctrl mask in
  let open Keys in
  match Wsi.ks2kt key with
  | Ascii 'Q' -> exit 0
  | Ascii 'z' ->
     let yloc f =
       match List.rev !S.rects with
       | [] -> ()
       | (pageno, _, (_, y0, _, y1, _, y2, _, y3)) :: _ ->
          f pageno (y0, y1, y2, y3)
     and fsel f (y0, y1, y2, y3) = f y0 y1 |> f y2 |> f y3 |> truncate in
     let ondone msg = S.text := msg
     and zmod _ _ k =
       match [@warning "-fragile-match"] k with
       | Keys.Ascii 'z' ->
          let f pageno ys =
            let miny = fsel min ys in
            let hh = (fsel max ys - miny)/2 in
            gotopage1 pageno (miny + hh - !S.winh/2)
          in
          yloc f;
          TEdone "center"
       | Keys.Ascii 't' ->
          let f pageno ys = gotopage1 pageno @@ fsel min ys in
          yloc f;
          TEdone "top"
       | Keys.Ascii 'b' ->
          let f pageno ys = gotopage1 pageno (fsel max ys - !S.winh) in
          yloc f;
          TEdone "bottom"
       | _ -> TEstop
     in
     enttext (": ", E.s, None, zmod !S.mode, ondone, true)
  | Ascii 'W' ->
     if Ffi.hasunsavedchanges ()
     then save ()
  | Insert ->
     if conf.angle mod 360 = 0 && not (isbirdseye !S.mode)
     then (
       S.mode := (
         match !S.lnava with
         | None -> LinkNav (Ltgendir 0)
         | Some pn -> LinkNav (Ltexact pn)
       );
       gotoxy !S.x !S.y;
     )
     else impmsg "keyboard link navigation does not work under rotation"
  | Escape | Ascii 'q' ->
     begin match !S.mstate with
     | Mzoomrect _ ->
        resetmstate ();
        Glutils.postRedisplay "kill rect";
     | Msel _
     | Mpan _
     | Mscrolly | Mscrollx
     | Mzoom _
     | Mnone ->
        begin match !S.mode with
        | LinkNav ln ->
           begin match ln with
           | Ltexact pl -> S.lnava := Some pl
           | Ltgendir _ | Ltnotready _ -> S.lnava := None
           end;
           S.mode := View;
           Glutils.postRedisplay "esc leave linknav"
        | Birdseye _ | Textentry _ | View ->
           match !S.ranchors with
           | [] -> raise Quit
           | (path, mimetype, password, anchor, origin) :: rest ->
              S.ranchors := rest;
              S.anchor := anchor;
              S.origin := origin;
              S.nameddest := E.s;
              opendoc path mimetype password
        end;
     end;
  | Ascii 'o' -> enteroutlinemode ()
  | Ascii 'u' ->
     S.rects := [];
     S.text := E.s;
     Hashtbl.iter (fun _ opaque -> Ffi.clearmark opaque) S.pagemap;
     Glutils.postRedisplay "dehighlight";
  | Ascii (('/' | '?') as c) ->
     let ondone isforw s =
       cbput !S.hists.pat s;
       S.searchpattern := s;
       search s isforw
     in
     enttext (String.make 1 c, E.s, Some (onhist !S.hists.pat),
              textentry, ondone (c = '/'), true)
  | Ascii '+' | Ascii '=' when ctrl ->
     let incr = if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01 in
     pivotzoom (conf.zoom +. incr)
  | Ascii '+' ->
     let ondone s =
       let n =
         try int_of_string s with exn ->
           S.text := Printf.sprintf "bad integer `%s': %s" s @@ exntos exn;
           max_int
       in
       if n != max_int
       then (
         conf.pagebias <- n;
         S.text := "page bias is now " ^ string_of_int n;
       )
     in
     enttext ("page bias: ", E.s, None, intentry, ondone, true)
  | Ascii '-' when ctrl ->
     let decr = if conf.zoom -. 0.1 < 0.1 then 0.01 else 0.1 in
     pivotzoom (max 0.01 (conf.zoom -. decr))
  | Ascii '-' ->
     let ondone msg = S.text := msg in
     enttext ("option: ", E.s, None,
              optentry !S.mode, ondone, true)
  | Ascii '0' when ctrl ->
     if conf.zoom = 1.0
     then gotoxy 0 !S.y
     else setzoom 1.0
  | Ascii ('1'|'2' as c) when ctrl && conf.fitmodel != FitPage ->
     let cols =
       match conf.columns with
       | Csingle _ | Cmulti _ -> 1
       | Csplit (n, _) -> n
     in
     let h = !S.winh -
               conf.interpagespace lsl (if conf.presentation then 1 else 0)
     in
     let zoom = Ffi.zoomforh !S.winw h 0 cols in
     if zoom > 0.0 && (c = '2' || zoom < 1.0)
     then setzoom zoom
  | Ascii '3' when ctrl ->
     let fm =
       match conf.fitmodel with
       | FitWidth -> FitProportional
       | FitProportional -> FitPage
       | FitPage -> FitWidth
     in
     S.text := "fit model: " ^ FMTE.to_string fm;
     reqlayout conf.angle fm
  | Ascii '4' when ctrl ->
     let zoom = Ffi.getmaxw () /. float !S.winw in
     if zoom > 0.0 then setzoom zoom
  | Fn 9 -> togglebirdseye ()
  | Ascii '9' when ctrl -> togglebirdseye ()
  | Ascii ('0'..'9' as c) when not ctrl ->
     let ondone s =
       let n =
         try int_of_string s with exn ->
           adderrfmt "int_of_string" "`%s': %s" s @@ exntos exn;
           -1
       in
       if n >= 0
       then (
         addnav ();
         cbput !S.hists.pag (string_of_int n);
         gotopage1 (n + conf.pagebias - 1) 0;
       )
     in
     let pageentry text = function [@warning "-fragile-match"]
                                 | Keys.Ascii 'g' -> TEdone text
                                 | key -> intentry text key
     in
     enttext (":", String.make 1 c, Some (onhist !S.hists.pag),
              pageentry, ondone, true)
  | Ascii 'b' ->
     conf.scrollb <- if conf.scrollb = 0 then (scrollbvv lor scrollbhv) else 0;
     Glutils.postRedisplay "toggle scrollbar";
  | Ascii 'B' ->
     S.bzoom := not !S.bzoom;
     S.rects := [];
     showtext ' ' ("block zoom " ^ onoffs !S.bzoom)
  | Ascii 'l' ->
     conf.hlinks <- not conf.hlinks;
     S.text := "highlightlinks " ^ onoffs conf.hlinks;
     Glutils.postRedisplay "toggle highlightlinks"
  | Ascii 'F' ->
     if conf.angle mod 360 = 0
     then (
       S.glinks := true;
       let mode = !S.mode in
       let te = ("goto: ", E.s, None, linknentry, linknact gotounder, false) in
       S.mode := Textentry (te, (fun _ -> S.glinks := false; S.mode := mode));
       S.text := E.s;
       Glutils.postRedisplay "view:linkent(F)"
     )
     else impmsg "hint mode does not work under rotation"
  | Ascii 'y' ->
     S.glinks := true;
     let mode = !S.mode in
     let te = ("copy: ", E.s, None, linknentry,
               linknact (fun under -> selstring conf.selcmd (undertext under)),
               false) in
     S.mode := Textentry (te, (fun _ -> S.glinks := false; S.mode := mode));
     S.text := E.s;
     Glutils.postRedisplay "view:linkent"
  | Ascii 'a' ->
     begin match !S.autoscroll with
     | Some step ->
        conf.autoscrollstep <- step;
        S.autoscroll := None
     | None -> S.autoscroll := Some conf.autoscrollstep
     end
  | Ascii 'p' when ctrl -> launchpath ()
  | Ascii 'P' ->
     setpresentationmode (not conf.presentation);
     showtext ' ' ("presentation mode " ^ onoffs conf.presentation)
  | Ascii 'f' ->
     if List.mem Wsi.Fullscreen !S.winstate
     then Wsi.reshape conf.cwinw conf.cwinh
     else Wsi.fullscreen ()
  | Ascii ('p'|'N') -> search !S.searchpattern false
  | Ascii 'n' | Fn 3 -> search !S.searchpattern true
  | Ascii 't' ->
     begin match !S.layout with
     | [] -> ()
     | l :: _ -> gotoxy !S.x (getpagey l.pageno)
     end
  | Ascii ' ' -> nextpage ()
  | Delete -> prevpage ()
  | Ascii '=' -> showtext ' ' (describe_layout !S.layout);
  | Ascii 'w' ->
     begin match !S.layout with
     | [] -> ()
     | l :: _ ->
        Wsi.reshape l.pagew l.pageh;
        Glutils.postRedisplay "w"
     end
  | Ascii '\'' -> enterbookmarkmode ()
  | Ascii 'i' -> enterinfomode ()
  | Ascii 'e' when Buffer.length S.errmsgs > 0 -> entermsgsmode ()
  | Ascii 'm' ->
     let ondone s =
       match !S.layout with
       | l :: _ when nonemptystr s -> addbookmark s @@ getanchor1 l
       | _ -> ()
     in
     enttext ("bookmark: ", E.s, None, textentry, ondone, true)
  | Ascii '~' ->
     quickbookmark ();
     showtext ' ' "Quick bookmark added";
  | Ascii 'x' -> !S.roamf ()
  | Ascii ('<'|'>' as c) ->
     reqlayout (conf.angle + (if c = '>' then 30 else -30)) conf.fitmodel
  | Ascii ('['|']' as c) ->
     conf.colorscale <-
       bound (conf.colorscale +. (if c = ']' then 0.1 else -0.1)) 0.0 1.0;
     Glutils.postRedisplay "brightness";
  | Ascii 'c' when !S.mode = View ->
     if Wsi.withalt mask
     then (
       if conf.zoom > 1.0
       then
         let m = (!S.winw - !S.w) / 2 in
         gotoxy m !S.y
     )
     else
       let (c, a, b), z =
         match !S.prevcolumns with
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
  | Down | Up when ctrl && Wsi.withshift mask ->
     let zoom, x = !S.prevzoom in
     setzoom zoom;
     S.x := x;
  | Up ->
     begin match !S.autoscroll with
     | None ->
        begin match !S.mode with
        | Birdseye beye -> upbirdseye 1 beye
        | Textentry _ | View | LinkNav _ ->
           if ctrl
           then gotoxy !S.x (U.add_to_y_and_clamp ~-(!S.winh/2))
           else (
             if not (Wsi.withshift mask) && conf.presentation
             then prevpage ()
             else gotoxy !S.x (U.add_to_y_and_clamp (-conf.scrollstep))
           )
        end
     | Some n -> setautoscrollspeed n false
     end
  | Down ->
     begin match !S.autoscroll with
     | None ->
        begin match !S.mode with
        | Birdseye beye -> downbirdseye 1 beye
        | Textentry _ | View | LinkNav _ ->
           if ctrl
           then gotoxy !S.x (U.add_to_y_and_clamp (!S.winh/2))
           else (
             if not (Wsi.withshift mask) && conf.presentation
             then nextpage ()
             else gotoxy !S.x (U.add_to_y_and_clamp (conf.scrollstep))
           )
        end
     | Some n -> setautoscrollspeed n true
     end
  | Ascii 'H' -> enterhistmode ()
  | Fn 1 when Wsi.withalt mask -> enterhistmode ()
  | Fn 1 -> enterhelpmode ()
  | Left | Right when not (Wsi.withalt mask) ->
     if canpan ()
     then
       let dx =
         if ctrl
         then !S.winw / 2
         else conf.hscrollstep
       in
       let dx =
         let pv = Wsi.ks2kt key in
         if pv = Keys.Left then dx else -dx
       in
       gotoxy (U.panbound (!S.x + dx)) !S.y
     else (
       S.text := E.s;
       Glutils.postRedisplay "left/right"
     )
  | Prior ->
     let y =
       if ctrl
       then
         match !S.layout with
         | [] -> !S.y
         | l :: _ -> !S.y - l.pagey
       else U.add_to_y_and_clamp (U.pgscale ~- !S.winh)
     in
     gotoxy !S.x y
  | Next ->
     let y =
       if ctrl
       then
         match List.rev !S.layout with
         | [] -> !S.y
         | l :: _ -> getpagey l.pageno
       else U.add_to_y_and_clamp (U.pgscale !S.winh)
     in
     gotoxy !S.x y
  | Ascii 'g' | Home ->
     addnav ();
     gotoxy 0 0
  | Ascii 'G' | End ->
     addnav ();
     gotoxy 0 (U.add_to_y_and_clamp !S.maxy)
  | Right when Wsi.withalt mask ->
     (match !S.nav.future with
      | [] -> ()
      | next :: frest ->
         S.nav := { past = getanchor () :: !S.nav.past; future = frest; };
         gotoxy !S.x (getanchory next)
     )
  | Left when Wsi.withalt mask -> histback ()
  | Backspace -> histback ()
  | Ascii 'r' -> reload ()
  | Ascii 'v' when conf.debug ->
     S.rects := [];
     List.iter (fun l ->
         match getopaque l.pageno with
         | exception Not_found -> ()
         | opaque ->
            let x0, y0, x1, y1 = Ffi.pagebbox opaque in
            let rect = (float x0, float y0,
                        float x1, float y0,
                        float x1, float y1,
                        float x0, float y1) in
            debugrect rect;
            let color = (0.0, 0.0, 1.0 /. (l.pageno mod 3 |> float), 0.5) in
            S.rects := (l.pageno, color, rect) :: !S.rects;
       ) !S.layout;
     Glutils.postRedisplay "v";
  | Ascii '|' ->
     let mode = !S.mode in
     let cmd = ref E.s in
     let onleave = function
       | Cancel -> S.mode := mode
       | Confirm ->
          List.iter (fun l ->
              match getopaque l.pageno with
              | exception Not_found -> ()
              | opaque -> pipesel opaque !cmd) !S.layout;
          S.mode := mode
     in
     let ondone s =
       cbput !S.hists.sel s;
       cmd := s
     in
     let te =
       "| ", !cmd, Some (onhist !S.hists.sel), textentry, ondone, true
     in
     Glutils.postRedisplay "|";
     S.mode := Textentry (te, onleave);
  | (Ascii _|Fn _|Enter|Left|Right|Code _|Ctrl _) ->
     vlog "huh? %s" (Wsi.keyname key)

let linknavkeyboard key mask linknav =
  let pv = Wsi.ks2kt key in
  let getpage pageno =
    let rec loop = function
      | [] -> None
      | l :: _ when l.pageno = pageno -> Some l
      | _ :: rest -> loop rest
    in loop !S.layout
  in
  let doexact (pageno, n) =
    match getopaque pageno, getpage pageno with
    | opaque, Some l ->
       if pv = Keys.Enter
       then
         let under = Ffi.getlink opaque n in
         Glutils.postRedisplay "link gotounder";
         gotounder under;
         S.mode := View;
       else
         let opt, dir =
           let open Keys in
           match pv with
           | Home -> Some (Ffi.findlink opaque LDfirst), -1
           | End -> Some (Ffi.findlink opaque LDlast), 1
           | Left -> Some (Ffi.findlink opaque (LDleft n)), -1
           | Right -> Some (Ffi.findlink opaque (LDright n)), 1
           | Up -> Some (Ffi.findlink opaque (LDup n)), -1
           | Down -> Some (Ffi.findlink opaque (LDdown n)), 1
           | Delete|Escape|Insert|Enter|Next|Prior|Ascii _
           | Code _|Fn _|Ctrl _|Backspace -> None, 0
         in
         let pwl l dir =
           begin match Ffi.findpwl l.pageno dir with
           | Pwlnotfound -> ()
           | Pwl pageno ->
              let notfound dir =
                S.mode := LinkNav (Ltgendir dir);
                let y, h = getpageyh pageno in
                let y =
                  if dir < 0
                  then y + h - !S.winh
                  else y
                in
                gotoxy !S.x y
              in
              begin match getopaque pageno, getpage pageno with
              | opaque, Some _ ->
                 let link =
                   let ld = if dir > 0 then LDfirst else LDlast in
                   Ffi.findlink opaque ld
                 in
                 begin match link with
                 | Lfound m ->
                    showlinktype (Ffi.getlink opaque m);
                    S.mode := LinkNav (Ltexact (pageno, m));
                    Glutils.postRedisplay "linknav jpage";
                 | Lnotfound -> notfound dir
                 end;
              | _ | exception Not_found -> notfound dir
              end;
           end;
         in
         begin match opt with
         | Some Lnotfound -> pwl l dir;
         | Some (Lfound m) ->
            if m = n
            then pwl l dir
            else (
              let _, y0, _, y1 = Ffi.getlinkrect opaque m in
              if y0 < l.pagey
              then gotopage1 l.pageno y0
              else (
                let d = fstate.fontsize + 1 in
                if y1 - l.pagey > l.pagevh - d
                then gotopage1 l.pageno (y1 - !S.winh + d)
                else Glutils.postRedisplay "linknav";
              );
              showlinktype (Ffi.getlink opaque m);
              S.mode := LinkNav (Ltexact (l.pageno, m));
            )

         | None -> viewkeyboard key mask
         end;
    | _ | exception Not_found -> viewkeyboard key mask
  in
  if pv = Keys.Insert
  then (
    begin match linknav with
    | Ltexact pa -> S.lnava := Some pa
    | Ltgendir _ | Ltnotready _ -> ()
    end;
    S.mode := View;
    Glutils.postRedisplay "leave linknav"
  )
  else
    match linknav with
    | Ltgendir _ | Ltnotready _ -> viewkeyboard key mask
    | Ltexact exact -> doexact exact

let keyboard key mask =
  if (key = Char.code 'g' && Wsi.withctrl mask) && not (istextentry !S.mode)
  then wcmd U.interrupt ""
  else !S.uioh#key key mask |> setuioh

let birdseyekeyboard key mask
      ((oconf, leftx, pageno, hooverpageno, anchor) as beye) =
  let incr =
    match conf.columns with
    | Csingle _ -> 1
    | Cmulti ((c, _, _), _) -> c
    | Csplit _ -> error "bird's eye split mode"
  in
  let pgh layout = List.fold_left
                     (fun m l -> max l.pageh m) !S.winh layout in
  let open Keys in
  match Wsi.ks2kt key with
  | Ascii 'l' when Wsi.withctrl mask ->
     let y, h = getpageyh pageno in
     let top = (!S.winh - h) / 2 in
     gotoxy !S.x (max 0 (y - top))
  | Enter -> leavebirdseye beye false
  | Escape -> leavebirdseye beye true
  | Up -> upbirdseye incr beye
  | Down -> downbirdseye incr beye
  | Left -> upbirdseye 1 beye
  | Right -> downbirdseye 1 beye

  | Prior ->
     begin match !S.layout with
     | l :: _ ->
        if l.pagey != 0
        then (
          S.mode := Birdseye (oconf, leftx, l.pageno, hooverpageno, anchor);
          gotopage1 l.pageno 0;
        )
        else (
          let layout = layout !S.x (!S.y - !S.winh)
                         !S.winw
                         (pgh !S.layout) in
          match layout with
          | [] -> gotoxy !S.x (U.add_to_y_and_clamp ~- !S.winh)
          | l :: _ ->
             S.mode := Birdseye (oconf, leftx, l.pageno, hooverpageno, anchor);
             gotopage1 l.pageno 0
        );

     | [] -> gotoxy !S.x (U.add_to_y_and_clamp ~- !S.winh)
     end;

  | Next ->
     begin match List.rev !S.layout with
     | l :: _ ->
        let layout = layout !S.x
                       (!S.y + (pgh !S.layout))
                       !S.winw !S.winh in
        begin match layout with
        | [] ->
           let incr = l.pageh - l.pagevh in
           if incr = 0
           then (
             S.mode :=
               Birdseye (
                   oconf, leftx, !S.pagecount - 1, hooverpageno, anchor
                 );
             Glutils.postRedisplay "birdseye pagedown";
           )
           else
             gotoxy !S.x (U.add_to_y_and_clamp (incr + conf.interpagespace*2));

        | l :: _ ->
           S.mode := Birdseye (oconf, leftx, l.pageno, hooverpageno, anchor);
           gotopage1 l.pageno 0;
        end

     | [] -> gotoxy !S.x (U.add_to_y_and_clamp !S.winh)
     end;

  | Home ->
     S.mode := Birdseye (oconf, leftx, 0, hooverpageno, anchor);
     gotopage1 0 0

  | End ->
     let pageno = !S.pagecount - 1 in
     S.mode := Birdseye (oconf, leftx, pageno, hooverpageno, anchor);
     if not (U.pagevisible !S.layout pageno)
     then
       let h =
         match List.rev !S.pdims with
         | [] -> !S.winh
         | (_, _, h, _) :: _ -> h
       in
       gotoxy
         !S.x
         (max 0 (getpagey pageno - (!S.winh - h - conf.interpagespace)))
     else Glutils.postRedisplay "birdseye end";

  | Delete|Insert|Ascii _|Code _|Ctrl _|Fn _|Backspace -> viewkeyboard key mask

let drawpage l =
  let color =
    match !S.mode with
    | Textentry _ -> U.scalecolor 0.4
    | LinkNav _ | View -> U.scalecolor 1.0
    | Birdseye (_, _, pageno, hooverpageno, _) ->
       if l.pageno = hooverpageno
       then U.scalecolor 0.9
       else (
         if l.pageno = pageno
         then (
           let c = U.scalecolor 1.0 in
           GlDraw.color c;
           GlDraw.line_width 3.0;
           let dispx = l.pagedispx in
           Glutils.linerect
             (float (dispx-1)) (float (l.pagedispy-1))
             (float (dispx+l.pagevw+1))
             (float (l.pagedispy+l.pagevh+1));
           GlDraw.line_width 1.0;
           c;
         )
         else U.scalecolor 0.8
       )
  in
  drawtiles l color

let postdrawpage l linkindexbase =
  match getopaque l.pageno with
  | exception Not_found -> 0
  | opaque ->
     if tileready l l.pagex l.pagey
     then
       let x = l.pagedispx - l.pagex
       and y = l.pagedispy - l.pagey in
       let hlmask =
         match conf.columns with
         | Csingle _ | Cmulti _ ->
            (if conf.hlinks then 1 else 0)
            + (if !S.glinks
                  && not (isbirdseye !S.mode) then 2 else 0)
         | Csplit _ -> 0
       in
       let s =
         match !S.mode with
         | Textentry ((_, s, _, _, _, _), _) when !S.glinks -> s
         | Textentry _
         | Birdseye _
         | View
         | LinkNav _ -> E.s
       in
       let n =
         Ffi.postprocess opaque hlmask x y
           (linkindexbase, s, conf.hfsize, conf.hcs) in
       if n < 0
       then (Glutils.redisplay := not @@ hasdata !S.ss; 0)
       else n
     else 0

let scrollindicator () =
  let sbw, ph, sh = !S.uioh#scrollph in
  let sbh, pw, sw = !S.uioh#scrollpw in

  let x0,x1,hx0 =
    if conf.leftscroll
    then (0, sbw, sbw)
    else ((!S.winw - sbw), !S.winw, 0)
  in

  Gl.enable `blend;
  GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
  let (r, g, b, alpha) = conf.sbarcolor in
  GlDraw.color (r, g, b) ~alpha;
  Glutils.filledrect (float x0) 0. (float x1) (float !S.winh);
  Glutils.filledrect
    (float hx0) (float (!S.winh - sbh))
    (float (hx0 + !S.winw)) (float !S.winh);
  let (r, g, b, alpha) = conf.sbarhndlcolor in
  GlDraw.color (r, g, b) ~alpha;

  Glutils.filledrect (float x0) ph (float x1) (ph +. sh);
  let pw = pw +. float hx0 in
  Glutils.filledrect pw (float (!S.winh - sbh)) (pw +. sw) (float !S.winh);
  Gl.disable `blend

let showsel () =
  match !S.mstate with
  | Mnone | Mscrolly | Mscrollx | Mpan _ | Mzoom _ | Mzoomrect _ -> ()
  | Msel ((x0, y0), (x1, y1)) ->
     let identify opaque l px py = Some (opaque, l.pageno, px, py) in
     let o0,n0,px0,py0 =
       onppundermouse identify x0 y0 (Opaque.of_string E.s, -1, 0, 0) in
     let _o1,n1,px1,py1 =
       onppundermouse identify x1 y1 (Opaque.of_string E.s, -1, 0, 0) in
     if n0 != -1 && n0 = n1 then Ffi.seltext o0 (px0, py0, px1, py1)

let showrects = function
  | [] -> ()
  | rects ->
     Gl.enable `blend;
     GlDraw.color (0.0, 0.0, 1.0) ~alpha:0.5;
     GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
     List.iter
       (fun (pageno, c, (x0, y0, x1, y1, x2, y2, x3, y3)) ->
         List.iter (fun l ->
             if l.pageno = pageno
             then
               let dx = float (l.pagedispx - l.pagex) in
               let dy = float (l.pagedispy - l.pagey) in
               let r, g, b, alpha = c in
               GlDraw.color (r, g, b) ~alpha;
               Glutils.filledrect2
                 (x0+.dx) (y0+.dy)
                 (x1+.dx) (y1+.dy)
                 (x3+.dx) (y3+.dy)
                 (x2+.dx) (y2+.dy);
           ) !S.layout
       ) rects;
     Gl.disable `blend

let display () =
  let sc (r, g, b) = let s = conf.colorscale in (r *. s, g *. s, b *. s) in
  GlDraw.color (sc conf.bgcolor);
  GlClear.color (sc conf.bgcolor);
  GlClear.clear [`color];
  List.iter drawpage !S.layout;
  let rects =
    match !S.mode with
    | LinkNav (Ltgendir _) | LinkNav (Ltnotready _)
    | Birdseye _
    | Textentry _
    | View -> !S.rects
    | LinkNav (Ltexact (pageno, linkno)) ->
       match getopaque pageno with
       | exception Not_found -> !S.rects
       | opaque ->
          let x0, y0, x1, y1 = Ffi.getlinkrect opaque linkno in
          let color =
            if conf.invert
            then (1.0, 1.0, 1.0, 0.5)
            else (0.0, 0.0, 0.5, 0.5)
          in
          (pageno, color,
           (float x0, float y0,
            float x1, float y0,
            float x1, float y1,
            float x0, float y1)
          ) :: !S.rects
  in
  showrects rects;
  let rec postloop linkindexbase = function
    | l :: rest ->
       let linkindexbase = linkindexbase + postdrawpage l linkindexbase in
       postloop linkindexbase rest
    | [] -> ()
  in
  showsel ();
  postloop 0 !S.layout;
  !S.uioh#display;
  begin match !S.mstate with
  | Mzoomrect ((x0, y0), (x1, y1)) ->
     Gl.enable `blend;
     GlDraw.color (0.3, 0.3, 0.3) ~alpha:0.5;
     GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
     Glutils.filledrect (float x0) (float y0) (float x1) (float y1);
     Gl.disable `blend;
  | Msel _
  | Mpan _
  | Mscrolly | Mscrollx
  | Mzoom _
  | Mnone -> ()
  end;
  enttext ();
  scrollindicator ();

  if conf.pgscale > 0.0
  then (
    let drawsep y =
       let x0 = 0.0 and y0 = y -. 3.0 in
       let x1 = float !S.winw and y1 = y +. 3.0 in
       Glutils.filledrect x0 y0 x1 y1;
    in
    Gl.enable `blend;
    GlDraw.color (0.1, 0.1, 0.1) ~alpha:0.5;
    GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
    (match !S.layout with
     | _ :: [] -> drawsep (conf.pgscale *. float !S.winh)
     | l -> List.iter (fun p -> drawsep (float (p.pagedispy+p.pagevh))) l
    );
    Gl.disable `blend;
  );
  Wsi.swapb ()

let display () =
  match !S.reload with
  | Some (x, y, t) ->
     if x != !S.x || y != !S.y || abs_float @@ now () -. t > 0.5
        || (!S.layout != [] && alltilesrendered !S.layout)
     then (
       S.reload := None;
       display ()
     )
  | None -> display ()

let zoomrect x y x1 y1 =
  let x0 = min x x1
  and x1 = max x x1
  and y0 = min y y1 in
  let zoom = (float !S.w) /. float (x1 - x0) in
  let margin =
    let simple () =
      if !S.w < !S.winw
      then (!S.winw - !S.w) / 2
      else 0
    in
    match conf.fitmodel with
    | FitWidth | FitProportional -> simple ()
    | FitPage ->
       match conf.columns with
       | Csplit _ ->
          onppundermouse (fun _ l _ _ -> Some l.pagedispx) x0 y0 x0
       | Cmulti _ | Csingle _ -> simple ()
  in
  gotoxy ((!S.x + margin) - x0) (!S.y + y0);
  S.anchor := getanchor ();
  setzoom zoom;
  resetmstate ()

let annot inline x y =
  match unproject x y with
  | Some (opaque, n, ux, uy) ->
     let add text =
       Ffi.addannot opaque ux uy text;
       wcmd1 U.freepage opaque;
       Hashtbl.remove S.pagemap (n, !S.gen);
       flushtiles ();
       gotoxy !S.x !S.y
     in
     if inline
     then
       let mode = !S.mode in
       let te = ("annotation: ", E.s, None, textentry, add, true) in
       S.mode := Textentry (te, fun _ -> S.mode := mode);
       S.text := E.s;
       enttext ();
       Glutils.postRedisplay "annot"
     else add @@ getusertext E.s
  | _ -> ()

let zoomblock x y =
  let g opaque l px py =
    match Ffi.rectofblock opaque px py with
    | Some a ->
       let x0 = a.(0) -. 20. in
       let x1 = a.(1) +. 20. in
       let y0 = a.(2) -. 20. in
       let zoom = (float !S.w) /. (x1 -. x0) in
       let pagey = getpagey l.pageno in
       let margin = (!S.w - l.pagew)/2 in
       let nx = -truncate x0 - margin in
       gotoxy nx (pagey + truncate y0);
       S.anchor := getanchor ();
       setzoom zoom;
       None
    | None -> None
  in
  match conf.columns with
  | Csplit _ ->
     impmsg "block zooming while in split columns mode is not implemented"
  | Cmulti _ | Csingle _ -> onppundermouse g x y ()

let scrollx x =
  let winw = !S.winw - 1 in
  let s = float x /. float winw in
  let destx = truncate (float (!S.w + winw) *. s) in
  gotoxy (winw - destx) !S.y;
  S.mstate := Mscrollx

let scrolly y =
  let s = float y /. float !S.winh in
  let desty = truncate (s *. float (U.maxy ())) in
  gotoxy !S.x desty;
  S.mstate := Mscrolly

let viewmulticlick clicks x y mask =
  let g opaque l px py =
    let mark =
      match clicks with
      | 2 -> MarkWord
      | 3 -> MarkLine
      | 4 -> MarkBlock
      | _ -> MarkPage
    in
    if Ffi.markunder opaque px py mark
    then (
      Some (fun () ->
          let dopipe cmd =
            match getopaque l.pageno with
            | exception Not_found -> ()
            | opaque -> pipesel opaque cmd
          in
          S.roamf := (fun () -> dopipe conf.paxcmd);
          if not (Wsi.withctrl mask) then dopipe conf.selcmd;
        )
    )
    else None
  in
  Glutils.postRedisplay "viewmulticlick";
  onppundermouse g x y (fun () -> impmsg "nothing to select") ()

let canselect () = conf.angle mod 360 = 0

let viewmouse button down x y mask =
  match button with
  | n when (n == 4 || n == 5) && not (Wsi.withshift mask) && not down ->
     if Wsi.withctrl mask
     then (
       let incr =
         if n = 5
         then if conf.zoom +. 0.01 > 0.1 then 0.1 else 0.01
         else if conf.zoom -. 0.1 < 0.1 then -0.01 else -0.1
       in
       let fx, fy =
         match !S.mstate with
         | Mzoom (oldn, _, pos) when n = oldn -> pos
         | Mzoomrect _ | Mnone | Mpan _
         | Msel _ | Mscrollx | Mscrolly | Mzoom _ -> (x, y)
       in
       let zoom = conf.zoom -. incr in
       S.mstate := Mzoom (n, 0, (x, y));
       if false && abs (fx - x) > 5 || abs (fy - y) > 5
       then pivotzoom ~x ~y zoom
       else pivotzoom zoom
     )
     else (
       match !S.autoscroll with
       | Some step -> setautoscrollspeed step (n=4)
       | None ->
          if conf.wheelbypage || conf.presentation
          then (
            if n = 4
            then prevpage ()
            else nextpage ()
          )
          else
            let incr = if n = 4 then -conf.scrollstep else conf.scrollstep in
            let incr = incr * 2 in
            let y = U.add_to_y_and_clamp incr in
            gotoxy !S.x y
     )

  | n when (n = 4 || n = 5 || n = 6 || n = 7) && not down && canpan () ->
     let x = U.panbound
               (!S.x + (if n = 5 || n = 7 then -2 else 2) * conf.hscrollstep)
     in
     gotoxy x !S.y

  | 1 when Wsi.withshift mask ->
     S.mstate := Mnone;
     if not down
     then (
       match unproject x y with
       | None -> ()
       | Some (_, pageno, ux, uy) ->
          let cmd = Printf.sprintf "%s %s %d %d %d" conf.stcmd !S.path
                      pageno ux uy
          in
          match spawn cmd [] with
          | exception exn ->
             adderrfmt "spawn" "execution of synctex command(%S) failed: %S"
               conf.stcmd @@ exntos exn
          | _pid -> ()
     )

  | 1 when Wsi.withctrl mask ->
     if down
     then (
       Wsi.setcursor Wsi.CURSOR_FLEUR;
       S.mstate := Mpan (x, y)
     )
     else S.mstate := Mnone

  | 3 ->
     if down
     then (
       if Wsi.withshift mask
       then (
         annot conf.annotinline x y;
         Glutils.postRedisplay "addannot"
       )
       else
         let p = (x, y) in
         Wsi.setcursor Wsi.CURSOR_CYCLE;
         S.mstate := Mzoomrect (p, p)
     )
     else (
       match !S.mstate with
       | Mzoomrect ((x0, y0), _) ->
          if abs (x-x0) > 10 && abs (y - y0) > 10
          then zoomrect x0 y0 x y
          else (
            resetmstate ();
            Glutils.postRedisplay "kill accidental zoom rect";
          )
       | Msel _
       | Mpan _
       | Mscrolly | Mscrollx
       | Mzoom _
       | Mnone -> resetmstate ()
     )

  | 1 when vscrollhit x ->
     if down
     then
       let _, position, sh = !S.uioh#scrollph in
       if y > truncate position && y < truncate (position +. sh)
       then S.mstate := Mscrolly
       else scrolly y
     else S.mstate := Mnone

  | 1 when y > !S.winh - hscrollh () ->
     if down
     then
       let _, position, sw = !S.uioh#scrollpw in
       if x > truncate position && x < truncate (position +. sw)
       then S.mstate := Mscrollx
       else scrollx x
     else S.mstate := Mnone

  | 1 when !S.bzoom -> if not down then zoomblock x y

  | 1 ->
     let dest = if down then getunder x y else Unone in
     begin match dest with
     | Ulinkuri _ -> gotounder dest
     | Unone when down ->
        Wsi.setcursor Wsi.CURSOR_FLEUR;
        S.mstate := Mpan (x, y);
     | Utextannot (opaque, slinkindex) -> enterannotmode opaque slinkindex
     | Unone | Utext _ | Ufileannot _ ->
        if down
        then (
          if canselect ()
          then (
            S.mstate := Msel ((x, y), (x, y));
            Glutils.postRedisplay "mouse select";
          )
        )
        else (
          match !S.mstate with
          | Mnone -> ()
          | Mzoom _ | Mscrollx | Mscrolly -> S.mstate := Mnone
          | Mzoomrect ((x0, y0), _) -> zoomrect x0 y0 x y
          | Mpan _ ->
             Wsi.setcursor Wsi.CURSOR_INHERIT;
             S.mstate := Mnone
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
                    | exception Not_found -> ()
                    | opaque ->
                       let dosel cmd () =
                         pipef ~closew:false "Msel"
                           (fun w ->
                             Ffi.copysel w opaque;
                             Glutils.postRedisplay "Msel") cmd
                       in
                       dosel conf.selcmd ();
                       S.roamf := dosel conf.paxcmd;
                  else loop rest
             in
             loop !S.layout;
             resetmstate ();
        )
     end
  | _ -> ()

let birdseyemouse button down x y mask
      (conf, leftx, _, hooverpageno, anchor) =
  match button with
  | 1 when down ->
     let rec loop = function
       | [] -> ()
       | l :: rest ->
          if y > l.pagedispy && y < l.pagedispy + l.pagevh
             && x > l.pagedispx && x < l.pagedispx + l.pagevw
          then
            leavebirdseye (conf, leftx, l.pageno, hooverpageno, anchor) false
          else loop rest
     in
     loop !S.layout
  | 3 -> ()
  | _ -> viewmouse button down x y mask

let uioh = object
    method display = ()
    method infochanged _ = ()

    method key key mask =
      begin match !S.mode with
      | Textentry textentry -> textentrykeyboard key mask textentry
      | Birdseye birdseye -> birdseyekeyboard key mask birdseye
      | View -> viewkeyboard key mask
      | LinkNav linknav -> linknavkeyboard key mask linknav
      end;
      !S.uioh

    method button button bstate x y mask =
      begin match !S.mode with
      | LinkNav _ | View -> viewmouse button bstate x y mask
      | Birdseye beye -> birdseyemouse button bstate x y mask beye
      | Textentry _ -> ()
      end;
      !S.uioh

    method multiclick clicks x y mask =
      begin match !S.mode with
      | LinkNav _ | View -> viewmulticlick clicks x y mask
      | Birdseye _ | Textentry _ -> ()
      end;
      !S.uioh

    method motion x y =
      begin match !S.mode with
      | Textentry _ -> ()
      | View | Birdseye _ | LinkNav _ ->
         match !S.mstate with
         | Mzoom _ | Mnone -> ()
         | Mpan (x0, y0) ->
            let dx = x - x0
            and dy = y0 - y in
            S.mstate := Mpan (x, y);
            let x = if canpan () then U.panbound (!S.x + dx) else !S.x in
            let y = U.add_to_y_and_clamp dy in
            gotoxy x y

         | Msel (a, _) ->
            S.mstate := Msel (a, (x, y));
            Glutils.postRedisplay "motion select";

         | Mscrolly ->
            let y = min !S.winh (max 0 y) in
            scrolly y

         | Mscrollx ->
            let x = min !S.winw (max 0 x) in
            scrollx x

         | Mzoomrect (p0, _) ->
            S.mstate := Mzoomrect (p0, (x, y));
            Glutils.postRedisplay "motion zoomrect";
      end;
      !S.uioh

    method pmotion x y =
      begin match !S.mode with
      | Birdseye (conf, leftx, pageno, hooverpageno, anchor) ->
         let rec loop = function
           | [] ->
              if hooverpageno != -1
              then (
                S.mode := Birdseye (conf, leftx, pageno, -1, anchor);
                Glutils.postRedisplay "pmotion birdseye no hoover";
              )
           | l :: rest ->
              if y > l.pagedispy && y < l.pagedispy + l.pagevh
                 && x > l.pagedispx && x < l.pagedispx + l.pagevw
              then (
                S.mode := Birdseye (conf, leftx, pageno, l.pageno, anchor);
                Glutils.postRedisplay "pmotion birdseye hoover";
              )
              else loop rest
         in
         loop !S.layout

      | Textentry _ -> ()

      | LinkNav _ | View ->
         match !S.mstate with
         | Mpan _ | Msel _ | Mzoom _ | Mscrolly | Mscrollx | Mzoomrect _ -> ()
         | Mnone ->
            updateunder x y;
            if canselect ()
            then
              match conf.pax with
              | None -> ()
              | Some past ->
                 let now = now () in
                 let delta = now -. past in
                 if delta > 0.01
                 then paxunder x y
                 else conf.pax <- Some now
      end;
      !S.uioh

    method scrollph =
      let maxy = U.maxy () in
      let p, h =
        if maxy = 0
        then 0.0, float !S.winh
        else scrollph !S.y maxy
      in
      vscrollw (), p, h

    method scrollpw =
      let fwinw = float (!S.winw - vscrollw ()) in
      let sw =
        let sw = fwinw /. float !S.w in
        let sw = fwinw *. sw in
        max sw (float conf.scrollh)
      in
      let position =
        let maxx = !S.w + !S.winw in
        let x = !S.winw - !S.x in
        let percent = float x /. float maxx in
        (fwinw -. sw) *. percent
      in
      hscrollh (), position, sw

    method modehash =
      let modename =
        match !S.mode with
        | LinkNav _ -> "links"
        | Textentry _ -> "textentry"
        | Birdseye _ -> "birdseye"
        | View -> "view"
      in
      findkeyhash conf modename

    method eformsgs = true
    method alwaysscrolly = false
    method scroll dx dy =
      let x = if canpan () then U.panbound (!S.x + dx) else !S.x in
      gotoxy x (U.add_to_y_and_clamp (2 * dy));
      !S.uioh
    method zoom z x y =
      pivotzoom ~x ~y (conf.zoom *. exp z);
  end

let ract cmds =
  let cl = splitatchar cmds ' ' in
  let scan s fmt f =
    try Scanf.sscanf s fmt f
    with exn -> adderrfmt "remote exec" "error processing '%S': %s\n"
                  cmds @@ exntos exn
  in
  let rectx s pageno (r, g, b, a) x0 y0 x1 y1 =
    vlog "%s page %d color (%f %f %f %f) x0,y0,x1,y1 = %f %f %f %f"
      s pageno r g b a x0 y0 x1 y1;
    onpagerect
      pageno
      (fun w h ->
        let _,w1,h1,_ = getpagedim pageno in
        let sw = float w1 /. float w
        and sh = float h1 /. float h in
        let x0s = x0 *. sw
        and x1s = x1 *. sw
        and y0s = y0 *. sh
        and y1s = y1 *. sh in
        let rect = (x0s,y0s,x1s,y0s,x1s,y1s,x0s,y1s) in
        let color = (r, g, b, a) in
        if conf.verbose then debugrect rect;
        S.rects := (pageno, color, rect) :: !S.rects;
        Glutils.postRedisplay s;
      )
  in
  match cl with
  | "reload", "" -> reload ()
  | "goto", args ->
     scan args "%u %f %f"
       (fun pageno x y ->
         let cmd, _ = !S.geomcmds in
         if emptystr cmd
         then gotopagexy pageno x y
         else
           let f prevf () =
             gotopagexy pageno x y;
             prevf ()
           in
           S.reprf := f !S.reprf
       )
  | "goto1", args -> scan args "%u %f" gotopage
  | "gotor", args -> scan args "%S" gotoremote
  | "rect", args ->
     scan args "%u %u %f %f %f %f"
       (fun pageno c x0 y0 x1 y1 ->
         let color = (0.0, 0.0, 1.0 /. float c, 0.5) in
         rectx "rect" pageno color x0 y0 x1 y1;
       )
  | "pgoto", args ->
     scan args "%u %f %f"
       (fun pageno x y ->
         let optopaque =
           match getopaque pageno with
           | exception Not_found -> Opaque.of_string E.s
           | opaque -> opaque
         in
         pgoto optopaque pageno x y;
         let rec fixx = function
           | [] -> ()
           | l :: rest ->
              if l.pageno = pageno
              then gotoxy (!S.x - l.pagedispx) !S.y
              else fixx rest
         in
         let layout =
           let mult =
             match conf.columns with
             | Csingle _ | Csplit _ -> 1
             | Cmulti ((n, _, _), _) -> n
           in
           layout 0 !S.y (!S.winw * mult) !S.winh
         in
         fixx layout
       )
  | "activatewin", "" -> Wsi.activatewin ()
  | "quit", "" -> raise Quit
  | "keys", keys ->
     begin try
         let l = Config.keys_of_string keys in
         List.iter (fun (k, m) -> keyboard k m) l
       with exn -> adderrfmt "error processing keys" "`%S': %s\n"
                     cmds @@ exntos exn
     end
  | _ ->
     adderrfmt "remote command"
       "error processing remote command: %S\n" cmds

let remote =
  let scratch = Bytes.create 80 in
  let buf = Buffer.create 80 in
  fun fd ->
  match tempfailureretry (Unix.read fd scratch 0) 80 with
  | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> None
  | 0 ->
     Unix.close fd;
     if Buffer.length buf > 0
     then (
       let s = Buffer.contents buf in
       Buffer.clear buf;
       ract s;
     );
     None
  | n ->
     let rec eat ppos =
       let nlpos =
         match Bytes.index_from scratch ppos '\n' with
         | exception Not_found -> -1
         | pos -> if pos >= n then -1 else pos
       in
       if nlpos >= 0
       then (
         Buffer.add_subbytes buf scratch ppos (nlpos-ppos);
         let s = Buffer.contents buf in
         Buffer.clear buf;
         ract s;
         eat (nlpos+1);
       )
       else (
         Buffer.add_subbytes buf scratch ppos (n-ppos);
         Some fd
       )
     in eat 0

let remoteopen path =
  try Some (Unix.openfile path [Unix.O_NONBLOCK; Unix.O_RDONLY] 0o0)
  with exn ->
    adderrfmt "remoteopen" "error opening %S: %s" path @@ exntos exn;
    None

let () =
  vlogf := (fun s -> if conf.verbose then print_endline s else ignore s);
  S.redirstderr := not @@ Unix.isatty Unix.stderr;
  let gc = ref false in
  let rcmdpath = ref E.s in
  let dcfpath = ref E.s in
  let pageno = ref None in
  let openlast = ref false in
  let doreap = ref false in
  let csspath = ref None in
  let justversion = ref false in
  S.selfexec := Sys.executable_name;
  let spec =
    [("-p", Arg.Set_string S.password, "<password> Set password");
     ("-f", Arg.String
              (fun s ->
                S.fontpath := s;
                S.selfexec := !S.selfexec ^ " -f " ^ Filename.quote s;
              ), "<path> Set path to the user interface font");
     ("-c", Arg.String
              (fun s ->
                S.selfexec := !S.selfexec ^ " -c " ^ Filename.quote s;
                S.confpath := s), "<path> Set path to the configuration file");
     ("-last", Arg.Set openlast, " Open last document");
     ("-page", Arg.Int (fun pageno1 -> pageno := Some (pageno1-1)),
      "<page-number> Jump to page");
     ("-dest", Arg.Set_string S.nameddest,
      "<dest-name> Set named destination");
     ("-remote", Arg.Set_string rcmdpath,
      "<path> Set path to the remote fifo");
     ("-gc", Arg.Set gc, " Collect garbage");
     ("-v", Arg.Set justversion, " Print version and exit");
     ("-css", Arg.String (fun s -> csspath := Some s),
      "<path> Set path to the style sheet to use with EPUB/HTML");
     ("-origin", Arg.Set_string S.origin, "<origin> <undocumented>");
     ("-no-title", Arg.Set S.ignoredoctitlte, " Ignore document title");
     ("-dcf", Arg.Set_string dcfpath, "<path> <undocumented>");
     ("-flip-stderr-redirection",
      Arg.Unit (fun () -> S.redirstderr := not !S.redirstderr),
      " <undocumented>");
     ("-mime", Arg.Set_string S.mimetype, "<mime-type> <undocumented>")
    ]
  in
  Arg.parse (Arg.align spec) (fun s -> S.path := s)
    ("Usage: " ^ Sys.argv.(0) ^ " [options] some.pdf\nOptions:");

  if !S.confpath == E.s
  then (
    let dir =
      let dir = Filename.concat home ".config" in
      if try Sys.is_directory dir with _ -> false then dir else home
    in
    S.confpath := Filename.concat dir "llpp.conf"
  );

  if !justversion
  then Printf.(
    printf "%s\nconfiguration file: %s\n" (Help.version ()) !S.confpath;
    exit 0
  );

  let histmode = emptystr !S.path && not !openlast in

  if !gc
  then (
    Config.gc ();
    if histmode then exit 0;
  );

  if not (Config.load !openlast)
  then dolog "failed to load configuration";

  if nonemptystr !dcfpath
  then conf.dcf <- !dcfpath;

  begin match !pageno with
  | Some pageno -> S.anchor := (pageno, 0.0, 0.0)
  | None -> ()
  end;

  fillhelp ();
  let mu =
    object (self)
      val mutable m_clicks = 0
      val mutable m_click_x = 0
      val mutable m_click_y = 0
      val mutable m_lastclicktime = infinity

      method private cleanup =
        S.roamf := noroamf;
        Hashtbl.iter (fun _ opaque -> Ffi.clearmark opaque) S.pagemap
      method expose = Glutils.postRedisplay "expose"
      method visible v =
        let name =
          match v with
          | Wsi.Unobscured -> "unobscured"
          | Wsi.PartiallyObscured -> "partiallyobscured"
          | Wsi.FullyObscured -> "fullyobscured"
        in
        vlog "visibility change %s" name
      method display = display ()
      method map mapped = vlog "mapped %b" mapped
      method reshape w h =
        self#cleanup;
        reshape w h
      method mouse b d x y m =
        (*http://blogs.msdn.com/b/oldnewthing/archive/2004/10/18/243925.aspx*)
        m_click_x <- x;
        setuioh @@
          if d && canselect ()
          then (
            m_click_y <- y;
                     if b = 1
                     then (
                       let t = now () in
                       if abs x - m_click_x > 10
                          || abs y - m_click_y > 10
                          || abs_float (t -. m_lastclicktime) > 0.3
                       then m_clicks <- 0;
                       m_clicks <- m_clicks + 1;
                       m_lastclicktime <- t;
                       if m_clicks = 1
                       then (
                         self#cleanup;
                         Glutils.postRedisplay "cleanup";
                         !S.uioh#button b d x y m
                       )
                       else !S.uioh#multiclick m_clicks x y m
                     )
                     else (
                       self#cleanup;
                       m_clicks <- 0;
                       m_lastclicktime <- infinity;
                       !S.uioh#button b d x y m
                     );
                   )
                   else !S.uioh#button b d x y m
      method motion x y =
        S.mpos := (x, y);
        !S.uioh#motion x y |> setuioh
      method pmotion x y =
        S.mpos := (x, y);
        !S.uioh#pmotion x y |> setuioh
      method key k m =
        vlog "k=%#x m=%#x" k m;
        let mascm = m land (
            Wsi.altmask + Wsi.shiftmask + Wsi.ctrlmask + Wsi.metamask
          ) in
        let keyboard k m =
          let x = !S.x and y = !S.y in
          keyboard k m;
          if x != !S.x || y != !S.y then self#cleanup
        in
        match !S.keystate with
        | KSnone ->
           let km = k, mascm in
           begin
             match
               let modehash = !S.uioh#modehash in
               try Hashtbl.find modehash km
               with Not_found ->
                 try Hashtbl.find (findkeyhash conf "global") km
                 with Not_found -> KMinsrt (k, m)
             with
             | KMinsrt (k, m) -> keyboard k m
             | KMinsrl l -> List.iter (fun (k, m) -> keyboard k m) l
             | KMmulti (l, r) -> S.keystate := KSinto (l, r)
           end
        | KSinto ((k', m') :: [], insrt) when k'=k && m' land mascm = m' ->
           List.iter (fun (k, m) -> keyboard k m) insrt;
           S.keystate := KSnone
        | KSinto ((k', m') :: keys, insrt) when k'=k && m' land mascm = m' ->
           S.keystate := KSinto (keys, insrt)
        | KSinto _ -> S.keystate := KSnone
      method enter x y =
        S.mpos := (x, y);
        !S.uioh#pmotion x y |> setuioh
      method leave = S.mpos := (-1, -1)
      method winstate wsl = S.winstate := wsl
      method quit : 'a. 'a = raise Quit
      method scroll dx dy =
        !S.uioh#scroll dx dy |> setuioh
      method zoom z x y = !S.uioh#zoom z x y
      method opendoc path =
        S.mode := View;
        setuioh uioh;
        Glutils.postRedisplay "opendoc";
        opendoc path !S.mimetype !S.password
    end
  in
  let wsfd, winw, winh = Wsi.init mu conf.cwinw conf.cwinh in
  S.wsfd := wsfd;

  let cs, ss =
    match Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 with
    | exception exn ->
       dolog "socketpair failed: %s" @@ exntos exn;
       exit 1
    | (r, w) ->
       Unix.set_close_on_exec r;
       Unix.set_close_on_exec w;
       r, w
  in

  begin match !csspath with
  | None -> ()
  | Some "" -> conf.css <- E.s
  | Some path ->
     let css = filecontents path in
     let l = String.length css in
     conf.css <-
       if l > 1 && substratis css (l-2) "\r\n"
       then String.sub css 0 (l-2)
       else (if l > 0 && css.[l-1] = '\n' then String.sub css 0 (l-1) else css)
  end;
  S.stderr := Ffi.init cs (
                  conf.angle, conf.fitmodel, (conf.trimmargins, conf.trimfuzz),
                  conf.texcount, conf.sliceheight, conf.mustoresize,
                  conf.colorspace, !S.fontpath, !S.redirstderr
                );
  List.iter GlArray.enable [`texture_coord; `vertex];
  GlTex.env (`color conf.texturecolor);
  S.ss := ss;
  reshape ~firsttime:true winw winh;
  setuioh uioh;
  if histmode
  then (Wsi.settitle "previously visited - llpp"; enterhistmode ())
  else opendoc !S.path !S.mimetype !S.password;
  display ();
  Wsi.mapwin ();
  Wsi.setcursor Wsi.CURSOR_INHERIT;
  Sys.set_signal Sys.sighup (Sys.Signal_handle (fun _ -> reload ()));

  let rec reap () =
    match Unix.waitpid [Unix.WNOHANG] ~-1 with
    | exception (Unix.Unix_error (Unix.ECHILD, _, _)) -> ()
    | exception exn -> dolog "Unix.waitpid: %s" @@ exntos exn
    | 0, _ -> ()
    | _pid, _status -> reap ()
  in
  Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> doreap := true));

  let optrfd =
    ref (if nonemptystr !rcmdpath then remoteopen !rcmdpath else None)
  in
  if !S.redirstderr
  then dologf := (adderrfmt "stderr" "%s\n");

  let fdl =
    let l = [!S.ss; !S.wsfd] in if !S.redirstderr then !S.stderr :: l else l
  in
  let rec loop deadline =
    if !doreap
    then (
      doreap := false;
      reap ()
    );
    let r =
      match !optrfd with
      | None -> fdl
      | Some fd -> fd :: fdl
    in
    if !Glutils.redisplay
    then (
      Glutils.redisplay := false;
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
       let newdeadline =
         match !S.autoscroll with
         | Some step when step != 0 ->
            let y = !S.y + step in
            let fy = if conf.maxhfit then !S.winh else 0 in
            let y =
              if y < 0
              then !S.maxy - fy
              else
                if y >= !S.maxy - fy
                then 0
                else y
            in
            gotoxy !S.x y;
            deadline +. 0.01
         | _ -> infinity
       in
       loop newdeadline

    | l ->
       let rec checkfds = function
         | [] -> ()
         | fd :: rest when fd = !S.ss ->
            let cmd = Ffi.rcmd !S.ss in
            act cmd;
            checkfds rest

         | fd :: rest when fd = !S.wsfd ->
            Wsi.readresp fd;
            checkfds rest

         | fd :: rest when fd = !S.stderr ->
            let b = Bytes.create 80 in
            begin match Unix.read fd b 0 80 with
            | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
            | exception exn -> adderrmsg "Unix.read exn" @@ exntos exn
            | 0 -> ()
            | n -> adderrmsg "stderr" @@ Bytes.sub_string b 0 n
            end;
            checkfds rest

         | fd :: rest when Some fd = !optrfd ->
            begin match remote fd with
            | None -> optrfd := remoteopen !rcmdpath;
            | opt -> optrfd := opt
            end;
            checkfds rest

         | _ :: rest ->
            adderrmsg "mainloop" "select returned unknown descriptor";
            checkfds rest
       in
       checkfds l;
       let newdeadline =
         match !S.autoscroll with
         | Some step when step != 0 ->
            if deadline = infinity
            then now () +. 0.01
            else deadline
         | _ -> infinity
       in
       loop newdeadline
    end;
  in
  match loop infinity with
  | exception Quit ->
     (match Buffer.length S.errmsgs with
      | 0 -> ()
      | n ->
         match Unix.write Unix.stdout (Buffer.to_bytes S.errmsgs) 0 n with
         | exception _ | _ -> ());
     Config.save leavebirdseye;
     if Ffi.hasunsavedchanges ()
     then save ()
  | _ -> error "umpossible - infinity reached"
