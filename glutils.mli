val vraw : [< Raw.kind > `float ] Raw.t
val filledrect2 :
  float ->
  float -> float -> float -> float -> float -> float -> float -> unit
val filledrect1 : float -> float -> float -> float -> unit
val filledrect : float -> float -> float -> float -> unit
val linerect : float -> float -> float -> float -> unit
val drawstring : int -> int -> int -> string -> unit
val drawstringf : int -> int -> int -> ('a, unit, string, unit) format4 -> 'a
val redisplay : bool ref
val postRedisplay : string -> unit
