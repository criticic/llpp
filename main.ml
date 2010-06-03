open Format;;
external init : Unix.file_descr -> GlTex.texture_id -> unit = "ml_init";;
external draw : int -> int -> int -> int -> string  -> unit = "ml_draw";;
(* external layout : int -> unit = "ml_layout";; *)

type g =
    { mutable csock : Unix.file_descr
    ; mutable ssock : Unix.file_descr
    ; mutable w : int
    ; mutable h : int
    ; mutable y : float
    ; mutable s : float
    ; mutable maxy : float
    ; mutable texs : GlTex.texture_id array
    ; mutable layout : (int * int * int * int * int * int) list
    ; pixcache : ((int * int * int), (string * bool)) Hashtbl.t
    }
;;

let state =
  { csock = Unix.stdin
  ; ssock = Unix.stdin
  ; w = 0
  ; h = 0
  ; y = 0.0
  ; s = 1.0
  ; texs = [||]
  ; layout = []
  ; maxy = float max_int
  ; pixcache = Hashtbl.create 10
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

let layout y =
  wcmd "layout" [`i y];
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
  wcmd "geometry" [`i w; `i h];
  layout (truncate state.y);
  (* wcmd "layout" [`I state.y]; *)
  log "reshape %dx%d" w h;
;;

let act cmd =
  match cmd.[0] with
  | 'c' ->
      state.layout <- []

  | 'p' ->
      let (n, pw, ph, dy, py, dh) as page =
        Scanf.sscanf cmd "p %d %d %d %d %d %d"
          (fun n pw ph dy py dh -> (n, pw, ph, dy, py, dh))
      in
      state.layout <- page :: state.layout;
      Glut.postRedisplay ();
      log "page[%d] pw=%d ph=%d dy=%d py=%d dh=%d" n pw ph dy py dh

  | 'r' ->
      let n, w, h, p =
        Scanf.sscanf cmd "r %d %d %d %s"
          (fun n w h p -> (n, w, h, p))
      in
      Hashtbl.add state.pixcache (n, w, h) (p, false);
      Glut.postRedisplay ();
      log "render[%d] w=%d h=%d p=%s" n w h p

  | 'm' ->
      let n = Scanf.sscanf cmd "m %d" (fun n -> n) in
      log "max %d" n;
      state.maxy <- float n

  | 'u' ->
      let n = Scanf.sscanf cmd "u %d" (fun n -> n) in
      let (s, _) = Hashtbl.find state.pixcache (n, state.w, state.h) in
      Hashtbl.replace state.pixcache (n, state.w, state.h) (s, true)

  | _ ->
      log "unknown cmd `%S'" cmd
;;

let idle () =
  let r, _, _ = Unix.select [state.csock] [] [] 0.01 in

  begin match r with
  | [] -> ()
  | _ ->
      let cmd = readcmd state.csock in
      act cmd;
  end;
;;

let clamp incr =
  let y = state.y +. float incr in
  let y = max 0. y in
  let y = min y state.maxy in
  state.y <- y;
;;

let keyboard ~key ~x ~y =
  begin match Char.chr key with
  | '\027' | 'q' -> exit 0
  | 'n' ->
      begin match List.rev state.layout with
      | [] -> ()
      | (_, _, h, _, pyo, sh) :: _ ->
          log"%d %d" h pyo;
          clamp (h-pyo);
          layout (truncate state.y)
      end
  | 'p' ->
      begin match state.layout with
      | [] -> ()
      | (_, _, h, _, _, sh) :: _ ->
          clamp ~-h;
          layout (truncate state.y)
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
  | Glut.KEY_HOME -> state.y <- 0.0
  | Glut.KEY_END -> state.y <- state.maxy -. float state.h
  | _ -> ()
  end;
  layout (truncate state.y);
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

let drawplaceholder (pageno, pagewidth, pageheight, screeny, pageyoffset, screenheight) =
  GlDraw.color (0.7, 0.7, 0.7);
  GlDraw.begins `quads;
  GlDraw.vertex2 (0.0, float screeny);
  GlDraw.vertex2 (float pagewidth, float screeny);
  GlDraw.vertex2 (float pagewidth, float (screeny + screenheight));
  GlDraw.vertex2 (0.0, float (screeny + screenheight));
  GlDraw.ends ();

  if false then (
    (* GlDraw.color colors.((pageno-1) mod (Array.length colors)); *)
    GlDraw.line_width 4.0;
    GlDraw.color (1.7, 1.7, 1.7);
    GlDraw.begins `lines;
    GlDraw.vertex2 (0.0, float screeny);
    GlDraw.vertex2 (float pagewidth, float screeny);
    GlDraw.vertex2 (float pagewidth, float (screeny + screenheight));
    GlDraw.vertex2 (0.0, float (screeny + screenheight));

    GlDraw.vertex2 (0.0, float screeny);
    GlDraw.vertex2 (float pagewidth, float (screeny + screenheight));

    GlDraw.vertex2 (float pagewidth, float screeny);
    GlDraw.vertex2 (0.0, float (screeny + screenheight));
    GlDraw.ends ();
  )
;;

let drawpage i
    ((pageno, pagewidth, pageheight, screeny, pageyoffset, screenheight) as page) =
  (* log "drawpage %d %d at %f" i pageno state.y; *)
  let key = (pageno, state.w, state.h) in
  begin try
      let pixmap, texready = Hashtbl.find state.pixcache key in
      if
        (* true || *)
        String.length pixmap = 0
      then
        drawplaceholder page
      else (
        GlDraw.color (1.0, 1.0, 1.0);
        if texready || true
        then (
          draw screeny pagewidth screenheight pageyoffset pixmap;
        )
        else (
          drawplaceholder page;
          wcmd "upload" [`i pageno; `s pixmap];
        );
      );
    with Not_found ->
      Hashtbl.add state.pixcache key ("", false);
      drawplaceholder page;
      wcmd "render" [`i pageno
                    ;`i pagewidth
                    ;`i pageheight];
  end;
  succ i;
;;

let display () =
  GlClear.color (0.5, 0.5, 0.5) ~alpha:0.0;
  (* GlClear.clear [`color]; *)
  GlDraw.color (0.0, 0.0, 0.0);
  ignore (List.fold_left drawpage 0 (List.rev state.layout));
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
  state.texs <- GlTex.gen_textures 1;
  init ssock state.texs.(0);
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
