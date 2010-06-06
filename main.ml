open Format;;
external init : Unix.file_descr -> unit = "ml_init";;
external draw : int -> int -> int -> int -> string  -> unit = "ml_draw";;
external preload : string -> unit = "ml_preload";;
(* external layout : int -> unit = "ml_layout";; *)

type ('a, 'b, 'c) g =
    { mutable csock : Unix.file_descr
    ; mutable ssock : Unix.file_descr
    ; mutable w : int
    ; mutable h : int
    ; mutable y : int
    ; mutable maxy : int
    ; mutable layout : (int * int * int * int * int * int * int) list
    ; pixcache : ((int * int * int), string) Hashtbl.t
    ; mutable pages : 'a list
    ; mutable pagecount : int
    ; lru : string array
    ; mutable lruidx : int
    }
;;

let state =
  { csock = Unix.stdin
  ; ssock = Unix.stdin
  ; w = 0
  ; h = 0
  ; y = 0
  ; layout = []
  ; maxy = max_int
  ; pixcache = Hashtbl.create 10
  ; pages = []
  ; pagecount = 0
  ; lru = Array.create 12 ""
  ; lruidx = 0
  }
;;

let aincr = 20;;
let log fmt = Printf.kprintf prerr_endline fmt;;
let dolog fmt = Printf.kprintf prerr_endline fmt;;

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

let wcmd s l =
  let b = Buffer.create 10 in
  Buffer.add_string b s;
  let rec combine = function
    | [] -> Buffer.contents b
    | x :: xs ->
        Buffer.add_char b ' ';
        let s =
          match x with
          | `s s -> s
          | `i i -> string_of_int i
          | `f f -> string_of_float f
          | `I f -> string_of_int (truncate f)
        in
        Buffer.add_string b s;
        combine xs;
  in
  let s = combine l in
  writecmd state.csock s;
;;

let layout y sh =
  let rec f pagenum pindex prev vy py dy l accu =
    if pagenum = state.pagecount
    then accu
    else
      let ((_, w, h) as curr), rest, pindex =
        match l with
        | ((pagenum', _, _) as curr) :: rest when pagenum' = pagenum ->
            (* log "pagenum=%d(%d) index=%d" pagenum pagenum' pindex; *)
            curr, rest, pindex + 1
        | _ ->
            prev, l, pindex
      in
      let pagenum' = pagenum + 1 in
      if py + h > vy
      then
        let py' = vy - py in
        let vh = h - py' in
        if dy + vh > sh
        then
          let vh = sh - dy in
          let e = pagenum, pindex, w, h, dy, py', vh in
          (* log "lay1[%d,%d] %dx%d" pagenum pindex w h; *)
          e :: accu
        else
          let e = pagenum, pindex, w, h, dy, py', vh in
          let accu = e :: accu in
          (* log "lay2[%d,%d] %dx%d" pagenum pindex w h; *)
          f pagenum' pindex curr (vy + vh) (py + h) (dy + vh) rest accu
      else
        f pagenum' pindex curr vy (py + h) dy rest accu
  in
  let accu = f 0 ~-1 (0,0,0) y 0 0 state.pages [] in
  (* log ""; *)
  List.rev accu
;;

let reshape ~w ~h =
  state.w <- w;
  state.h <- h;
  GlDraw.viewport 0 0 w h;
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.rotate ~x:1.0 ~angle:180.0 ();
  GlMat.translate ~x:~-.1.0 ~y:~-.1.0 ();
  GlMat.scale3 (2.0 /. float state.w, 2.0 /. float state.h, 1.0);
  state.pages <- [];
  wcmd "geometry" [`i w; `i h];
  let pages = layout state.y state.h in
  state.layout <- pages;
  Glut.postRedisplay ();
  log "reshape %dx%d" w h;
;;

let act cmd =
  match cmd.[0] with
  | 'C' ->
      let n = Scanf.sscanf cmd "C %d" (fun n -> n) in
      state.pagecount <- n;
      let pages = layout state.y state.h in
      state.layout <- pages;
      Glut.postRedisplay ();

  | 'c' ->
      state.layout <- []

  | 'r' ->
      let n, w, h, p =
        Scanf.sscanf cmd "r %d %d %d %s"
          (fun n w h p -> (n, w, h, p))
      in
      Hashtbl.replace state.pixcache (n, w, h) p;
      let idx = state.lruidx mod (Array.length state.lru) in
      let s = state.lru.(idx) in
      if String.length s != 0
      then begin
        log "free %s" s;
        wcmd "free" [`s s];
        let l = Hashtbl.fold (fun k s' a ->
          if s = s' then k :: a else a) state.pixcache []
        in
        List.iter (fun k ->
          let n,w,h = k in
          Hashtbl.remove state.pixcache k) l;
      end;
      state.lru.(idx) <- p;
      state.lruidx <- state.lruidx + 1;
      Glut.postRedisplay ();

  | 'm' ->
      let n = Scanf.sscanf cmd "m %d" (fun n -> n) in
      state.maxy <- n

  | 'u' ->
      let n = Scanf.sscanf cmd "u %d" (fun n -> n) in
      let s = Hashtbl.find state.pixcache (n, state.w, state.h) in
      Hashtbl.replace state.pixcache (n, state.w, state.h) s

  | 'l' ->
      let (n, w, h) as pagelayout =
        Scanf.sscanf cmd "l %d %d %d" (fun n w h -> n, w, h)
      in
(*       if n = 0 *)
(*       then state.pages <- []; *)
      state.pages <- pagelayout :: state.pages

  | _ ->
      log "unknown cmd `%S'" cmd
;;

let preload
    ((pageno, pindex, pagewidth, pageheight, screeny, pageyoffset, screenheight) as page) =
  let key = (pageno + 1, state.w, state.h) in
  begin try
      let pixmap = Hashtbl.find state.pixcache key in
      if
        String.length pixmap = 0
      then
        ()
      else (
        (* preload pixmap *)
      );
    with Not_found ->
      log "preload render %d" pageno;
      Hashtbl.add state.pixcache key "";
      wcmd "render" [`i (pageno + 1)
                    ;`i pindex
                    ;`i pagewidth
                    ;`i pageheight];
  end;
;;

let idle () =
  let r, _, _ = Unix.select [state.csock] [] [] 0.01 in

  begin match r with
  | [] ->
      if false then begin
        let h = state.h in
        let pages = layout (state.y + state.h) h in
        List.iter preload pages;
      end

  | _ ->
      let cmd = readcmd state.csock in
      act cmd;
  end;
;;

let clamp incr =
  let y = state.y + incr in
  let y = max 0 y in
  let y = min y state.maxy in
  state.y <- y;
;;

let keyboard ~key ~x ~y =
  begin match Char.chr key with
  | '\027' | 'q' -> exit 0
  | 'n' ->
      begin match List.rev state.layout with
      | [] -> ()
      | (_, _, _, h, _, pyo, sh) :: _ ->
          log "%d %d" h pyo;
          clamp (h-pyo);
          let pages = layout state.y state.h in
          state.layout <- pages;
          Glut.postRedisplay ();
      end

  | 'w' ->
      begin match state.layout with
      | [] -> ()
      | (_, _, w, h, _, _, _) :: _ ->
          Glut.reshapeWindow w h
      end

  | 'p' ->
      begin match state.layout with
      | [] -> ()
      | (_, _, _, h, _, _, sh) :: _ ->
          clamp ~-h;
          let pages = layout state.y state.h in
          state.layout <- pages;
          Glut.postRedisplay ();
      end
  | _ -> ()
  end;
  Glut.postRedisplay ();
;;

let special ~key ~x ~y =
  begin match key with
  | Glut.KEY_LEFT      -> ()
  | Glut.KEY_RIGHT     -> ()
  | Glut.KEY_UP        -> clamp ~-aincr
  | Glut.KEY_DOWN      -> clamp aincr
  | Glut.KEY_PAGE_UP   -> clamp (-state.h)
  | Glut.KEY_PAGE_DOWN -> clamp state.h
  | Glut.KEY_HOME -> state.y <- 0
  | Glut.KEY_END -> state.y <- state.maxy - state.h
  | _ -> ()
  end;
  let pages = layout state.y state.h in
  state.layout <- pages;
  Glut.postRedisplay ();
;;

let colors =
  [| (1.0, 0.0, 0.0)
  ;  (0.0, 1.0, 0.0)
  ;  (0.0, 0.0, 1.0)
  ;  (0.0, 0.0, 0.0)
  ;  (1.0, 1.0, 1.0)
  ;  (1.0, 1.0, 0.0)
  ;  (1.0, 0.0, 1.0)
  ;  (0.0, 1.0, 1.0)
  |]
;;

let drawplaceholder (pageno, pindex, pagewidth, pageheight,
                    screeny, pageyoffset, screenheight) =
  GlDraw.color (0.0, 0.0, 0.0);
  GlDraw.begins `quads;
  GlDraw.vertex2 (0.0, float screeny);
  GlDraw.vertex2 (float pagewidth, float screeny);
  GlDraw.vertex2 (float pagewidth, float (screeny + screenheight));
  GlDraw.vertex2 (0.0, float (screeny + screenheight));
  GlDraw.ends ();
;;

let now () = Unix.gettimeofday ();;

let drawpage i
    ((pageno, pindex, pagewidth, pageheight, screeny, pageyoffset, screenheight) as page) =
  let key = (pageno + 1, state.w, state.h) in
  begin try
      let pixmap = Hashtbl.find state.pixcache key in
      if
        String.length pixmap = 0
      then
        drawplaceholder page
      else (
        GlDraw.color (1.0, 1.0, 1.0);
        let a = now () in
        draw screeny pagewidth screenheight pageyoffset pixmap;
        let b = now () in
        let d = b-.a in
        if d > 0.000405
        then
          log "draw %f sec" d
        ;
      );
    with Not_found ->
      Hashtbl.add state.pixcache key "";
      drawplaceholder page;
      wcmd "render" [`i (pageno + 1)
                    ;`i pindex
                    ;`i pagewidth
                    ;`i pageheight];
  end;
  succ i;
;;

let display () =
  GlClear.color (0.5, 0.5, 0.5) ~alpha:0.0;
  GlClear.clear [`color];
  GlDraw.color (0.0, 0.0, 0.0);
  ignore (List.fold_left drawpage 0 (state.layout));
  Gl.finish ();
  Glut.swapBuffers ();
;;

let () =
  let w = 704
  and h = 576 in
  let w = 1448 in
(*   let w = 612 *)
(*   and h = 792 in *)
  let w = 800
  and h = 900 in
  let _ = Glut.init Sys.argv in
  let () = Glut.initDisplayMode ~depth:false ~double_buffer:true () in
  let () = Glut.initWindowSize w h in
  let _ = Glut.createWindow "lpdf (press 'h' to get help)" in

  let csock, ssock = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  init ssock;
  state.w <- w;
  state.h <- h;
  state.csock <- csock;
  state.ssock <- ssock;
  let name =
    if Array.length Sys.argv = 1
    then "/home/malc/x/inc/Info_PT_Sans.pdf"
    else Sys.argv.(1)
  in
  writecmd csock ("open " ^ name ^ "\000");
  (* writecmd csock "open /home/malc/x/doc/cell/CBE_Handbook_v1.1_24APR2007_pub.pdf\000"; *)
  (* writecmd csock "box 1"; *)

  let () = Glut.displayFunc display in
  let () = Glut.reshapeFunc reshape in
  let () = Glut.keyboardFunc keyboard in
  let () = Glut.specialFunc special in
  let () = Glut.idleFunc (Some idle) in
  (* let () = Glut.mouseFunc mouse in *)
  (* let () = Glut.motionFunc motion in *)
  Glut.mainLoop ();
;;
