(* $Id: glMat.ml,v 1.11 2005-10-28 02:49:09 garrigue Exp $ *)

type t = [`double] Raw.t

external frustum :
    x:(float * float) -> y:(float * float) -> z:(float * float) -> unit
    = "ml_glFrustum"

external load_identity : unit -> unit = "ml_glLoadIdentity"

external get_matrix : [`modelview_matrix|`projection_matrix|`texture_matrix] -> t -> unit = "ml_glGetDoublev" 
let get_matrix mode = 
  let model = Raw.create `double ~len:16 in
  get_matrix mode model;
  model

external mode : [`modelview|`projection|`texture] -> unit
    = "ml_glMatrixMode"

external pop : unit -> unit = "ml_glPopMatrix"
external push : unit -> unit = "ml_glPushMatrix"

external rotate : angle:float -> x:float -> y:float -> z:float -> unit
    = "ml_glRotated"
let rotate3 ~angle (x,y,z) = rotate ~angle ~x ~y ~z
let rotate ~angle ?(x=0.) ?(y=0.) ?(z=0.) () = rotate ~angle ~x ~y ~z

external scale : x:float -> y:float -> z:float -> unit = "ml_glScaled"
let scale3 (x,y,z) = scale ~x ~y ~z
let scale ?(x=0.) ?(y=0.) ?(z=0.) () = scale ~x ~y ~z

external translate : x:float -> y:float -> z:float -> unit = "ml_glTranslated"
let translate3 (x,y,z) = translate ~x ~y ~z
let translate ?(x=0.) ?(y=0.) ?(z=0.) () = translate ~x ~y ~z
