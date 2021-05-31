open Config

type initparams = (angle * fitmodel * trimparams * texcount * sliceheight *
                     memsize * colorspace * fontpath * redirstderr)
and xoff = int and yoff = int and noff = int
and li = (noff * string * hintfontsize * hintchars)
and hlmask = int and hintchars = string and hintfontsize = int

external init : Unix.file_descr -> initparams -> Unix.file_descr = "ml_init"
external seltext : opaque -> (int * int * int * int) -> unit = "ml_seltext"
external hassel : opaque -> bool = "ml_hassel"
external getpdimrect : int -> float array = "ml_getpdimrect"
external whatsunder : opaque -> x -> y -> under = "ml_whatsunder"
external markunder : opaque -> x -> y -> mark -> bool = "ml_markunder"
external clearmark : opaque -> unit = "ml_clearmark"
external zoomforh : int -> int -> int -> int -> float = "ml_zoom_for_height"
external getmaxw : unit -> float = "ml_getmaxw"
external postprocess :  opaque -> hlmask -> xoff -> yoff -> li -> noff
  = "ml_postprocess"
external setdcf : string -> unit = "ml_setdcf"
external pagebbox : opaque -> irect = "ml_getpagebox"
external setaalevel : int -> unit = "ml_setaalevel"
external setpapercolor : rgba -> unit = "ml_setpapercolor"
external realloctexts : int -> bool = "ml_realloctexts"
external findlink : opaque -> linkdir -> link = "ml_findlink"
external getlink : opaque -> int -> under = "ml_getlink"
external getlinkn : opaque -> string -> string -> int -> int = "ml_getlinkn"
external getlinkrect : opaque -> int -> irect = "ml_getlinkrect"
external findpwl : int -> int -> pagewithlinks = "ml_find_page_with_links"
external unproject : opaque -> int -> int -> (int * int) option
  = "ml_unproject"
external project : opaque -> int -> int -> float -> float -> (float * float)
  = "ml_project"
external drawtile : tileparams -> opaque -> unit = "ml_drawtile"
external rectofblock : opaque -> int -> int -> float array option
  = "ml_rectofblock"
external begintiles : unit -> unit = "ml_begintiles"
external endtiles : unit -> unit = "ml_endtiles"
external addannot : opaque -> int -> int -> string -> unit = "ml_addannot"
external modannot : opaque -> slinkindex -> string -> unit = "ml_modannot"
external delannot : opaque -> slinkindex -> unit = "ml_delannot"
external hasunsavedchanges : unit -> bool = "ml_hasunsavedchanges"
external savedoc : string -> unit = "ml_savedoc"
external gettextannot : opaque -> slinkindex -> string = "ml_gettextannot"
external getfileannot : opaque -> slinkindex -> string = "ml_getfileannot"
external wcmd : Unix.file_descr -> bytes -> int -> unit = "ml_wcmd"
external rcmd : Unix.file_descr -> string = "ml_rcmd"
external uritolocation : string -> (pageno * float * float) = "ml_uritolocation"
external isexternallink : string -> bool = "ml_isexternallink"

(* copysel _will_ close the supplied descriptor *)
external copysel : Unix.file_descr -> opaque -> unit = "ml_copysel"

external drawstr : int -> int -> int -> string -> float = "ml_draw_string"

external fz_version : unit -> string = "ml_fz_version"
external llpp_version : unit -> string = "ml_llpp_version"

external measurestr : int -> string -> float = "ml_measure_string"
external toutf8 : int -> string = "ml_keysymtoutf8"
external mbtoutf8 : string -> string = "ml_mbtoutf8"
