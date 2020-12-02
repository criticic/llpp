let vraw = Raw.create_static `float ~len:8;;

let filledrect2 x0 y0 x1 y1 x2 y2 x3 y3 =
  Raw.sets_float vraw ~pos:0 [| x0; y0; x1; y1; x2; y2; x3; y3 |];
  GlArray.vertex `two vraw;
  GlArray.draw_arrays `triangle_strip ~first:0 ~count:4;
;;

let filledrect1 x0 y0 x1 y1 = filledrect2 x0 y0 x0 y1 x1 y0 x1 y1;;

let filledrect x0 y0 x1 y1 =
  GlArray.disable `texture_coord;
  filledrect1 x0 y0 x1 y1;
  GlArray.enable `texture_coord;
;;

let linerect x0 y0 x1 y1 =
  GlArray.disable `texture_coord;
  Raw.sets_float vraw ~pos:0 [| x0; y0; x0; y1; x1; y1; x1; y0 |];
  GlArray.vertex `two vraw;
  GlArray.draw_arrays `line_loop ~first:0 ~count:4;
  GlArray.enable `texture_coord;
;;

let drawstring size x y s =
  Gl.enable `blend;
  Gl.enable `texture_2d;
  GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
  ignore (Ffi.drawstr size x y s);
  Gl.disable `blend;
  Gl.disable `texture_2d;
;;

let drawstringf size x y = Printf.kprintf (drawstring size (x+1) (y+size+1));;
let redisplay = ref false;;
let postRedisplay who =
  Utils.vlog "redisplay for [%S]" who;
  redisplay := true;
;;
