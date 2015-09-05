(* $Id: glFunc.ml,v 1.7 2000-04-12 07:40:23 garrigue Exp $ *)

open Gl

external accum : op:[`accum|`load|`add|`mult|`return] -> float -> unit
    = "ml_glAccum"
external alpha_func : cmp_func -> ref:clampf -> unit = "ml_glAlphaFunc"

type sfactor = [
    `zero
  | `one
  | `dst_color
  | `one_minus_dst_color
  | `src_alpha
  | `one_minus_src_alpha
  | `dst_alpha
  | `one_minus_dst_alpha
  | `src_alpha_saturate
]
type dfactor = [
    `zero
  | `one
  | `src_color
  | `one_minus_src_color
  | `src_alpha
  | `one_minus_src_alpha
  | `dst_alpha
  | `one_minus_dst_alpha
]
external blend_func : src:sfactor -> dst:dfactor -> unit = "ml_glBlendFunc"

external color_mask : bool -> bool -> bool -> bool -> unit
    = "ml_glColorMask"
let color_mask ?(red=false) ?(green=false) ?(blue=false) ?(alpha=false) ()=
  color_mask red green blue alpha

external depth_func : cmp_func -> unit = "ml_glDepthFunc"
external depth_mask : bool -> unit = "ml_glDepthMask"
external depth_range : near:float -> far:float -> unit = "ml_glDepthRange"

type draw_buffer =
    [`none|`front_left|`front_right|`back_left|`back_right
    |`front|`back|`left|`right|`front_and_back|`aux of int]
external draw_buffer : draw_buffer -> unit = "ml_glDrawBuffer"

external index_mask : int -> unit = "ml_glIndexMask"

type logic_op =
    [`clear|`set|`copy|`copy_inverted|`noop|`invert|`And|`nand|`Or|`nor
    |`xor|`equiv|`and_reverse|`and_inverted|`or_reverse|`or_inverted]
external logic_op : logic_op -> unit = "ml_glLogicOp"

type read_buffer =
    [`front_left|`front_right|`back_left|`back_right|`front|`back
    |`left|`right|`aux of int]
external read_buffer : read_buffer -> unit = "ml_glReadBuffer"

external stencil_func : cmp_func -> ref:int -> mask:int -> unit
    = "ml_glStencilFunc"
external stencil_mask : int -> unit = "ml_glStencilMask"
type stencil_op = [`keep|`zero|`replace|`incr|`decr|`invert]
external stencil_op :
    fail:stencil_op -> zfail:stencil_op -> zpass:stencil_op -> unit
    = "ml_glStencilOp"
let stencil_op ?(fail=`keep) ?(zfail=`keep) ?(zpass=`keep) () =
  stencil_op ~fail ~zfail ~zpass
