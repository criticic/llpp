val scrollph : int -> int -> float * float
val isbirdseye : Config.mode -> bool
val istextentry : Config.mode -> bool
val vscrollw : unit -> int
val vscrollhit : int -> bool
val firstof : int -> int -> int
val calcfirst : int -> int -> int
val enttext : unit -> unit
val textentrykeyboard :
  int ->
  int ->
  (string * string * Config.onhist option * Config.onkey * Config.ondone *
   Config.cancelonempty) *
  Config.onleave -> unit
class type lvsource =
  object
    method exit :
      uioh:Config.uioh ->
      cancel:bool -> active:int -> first:int -> pan:int -> Config.uioh option
    method getactive : int
    method getfirst : int
    method getitem : int -> string * int
    method getitemcount : int
    method getminfo : (int * int) array
    method getpan : int
    method hasaction : int -> bool
  end
class virtual lvsourcebase :
  object
    val mutable m_active : int
    val mutable m_first : int
    val mutable m_pan : int
    method getactive : int
    method getfirst : int
    method getminfo : (int * int) array
    method getpan : int
  end
val coe :
  < alwaysscrolly : bool;
    button : int -> bool -> int -> int -> int -> #Config.uioh;
    display : unit; eformsgs : bool; infochanged : Config.infochange -> unit;
    key : int -> int -> #Config.uioh; modehash : Config.keyhash;
    motion : int -> int -> #Config.uioh;
    multiclick : int -> int -> int -> int -> #Config.uioh;
    pmotion : int -> int -> #Config.uioh;
    scroll : int -> int -> #Config.uioh; scrollph : int * float * float;
    scrollpw : int * float * float; zoom : float -> int -> int -> unit; .. > ->
  Config.uioh
val setuioh :
  < alwaysscrolly : bool;
    button : int -> bool -> int -> int -> int -> #Config.uioh;
    display : unit; eformsgs : bool; infochanged : Config.infochange -> unit;
    key : int -> int -> #Config.uioh; modehash : Config.keyhash;
    motion : int -> int -> #Config.uioh;
    multiclick : int -> int -> int -> int -> #Config.uioh;
    pmotion : int -> int -> #Config.uioh;
    scroll : int -> int -> #Config.uioh; scrollph : int * float * float;
    scrollpw : int * float * float; zoom : float -> int -> int -> unit; .. > ->
  unit
val changetitle : < title : string; .. > -> unit
class listview :
  zebra:bool ->
  helpmode:bool ->
  source:lvsource ->
  trusted:bool ->
  modehash:Config.keyhash ->
  object ('a)
    val m_active : int
    val m_first : int
    val m_pan : int
    val m_prev_uioh : Config.uioh
    val m_qsearch : string
    method alwaysscrolly : bool
    method button : int -> bool -> int -> int -> int -> Config.uioh
    method display : unit
    method eformsgs : bool
    method private elemunder : int -> int option
    method infochanged : Config.infochange -> unit
    method key : int -> int -> Config.uioh
    method private key1 : int -> int -> Config.uioh
    method modehash : Config.keyhash
    method motion : int -> int -> Config.uioh
    method multiclick : int -> int -> int -> int -> Config.uioh
    method nextcurlevel : int -> 'a
    method pmotion : int -> int -> Config.uioh
    method scroll : int -> int -> Config.uioh
    method scrollph : int * float * float
    method scrollpw : int * float * float
    method updownlevel : int -> 'a
    method zoom : float -> int -> int -> unit
  end
