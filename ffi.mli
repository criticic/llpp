type initparams =
    Config.angle * Config.fitmodel * Config.trimparams * Config.texcount *
    Config.sliceheight * Config.memsize * Config.colorspace *
    Config.fontpath * Config.redirstderr
and xoff = int
and yoff = int
and noff = int
and li = noff * string * hintfontsize * hintchars
and hlmask = int
and hintchars = string
and hintfontsize = int
external init : Unix.file_descr -> initparams -> Unix.file_descr = "ml_init"
external seltext : Config.opaque -> int * int * int * int -> unit
  = "ml_seltext"
external hassel : Config.opaque -> bool = "ml_hassel"
external getpdimrect : int -> float array = "ml_getpdimrect"
external whatsunder : Config.opaque -> Config.x -> Config.y -> Config.under
  = "ml_whatsunder"
external markunder :
  Config.opaque -> Config.x -> Config.y -> Config.mark -> bool
  = "ml_markunder"
external clearmark : Config.opaque -> unit = "ml_clearmark"
external zoomforh : int -> int -> int -> int -> float = "ml_zoom_for_height"
external getmaxw : unit -> float = "ml_getmaxw"
external postprocess : Config.opaque -> hlmask -> xoff -> yoff -> li -> noff
  = "ml_postprocess"
external setdcf : string -> unit = "ml_setdcf"
external pagebbox : Config.opaque -> Config.irect = "ml_getpagebox"
external setaalevel : int -> unit = "ml_setaalevel"
external setpapercolor : Config.rgba -> unit = "ml_setpapercolor"
external realloctexts : int -> bool = "ml_realloctexts"
external findlink : Config.opaque -> Config.linkdir -> Config.link
  = "ml_findlink"
external getlink : Config.opaque -> int -> Config.under = "ml_getlink"
external getlinkn : Config.opaque -> string -> string -> int -> int
  = "ml_getlinkn"
external getlinkrect : Config.opaque -> int -> Config.irect
  = "ml_getlinkrect"
external findpwl : int -> int -> Config.pagewithlinks
  = "ml_find_page_with_links"
external unproject : Config.opaque -> int -> int -> (int * int) option
  = "ml_unproject"
external project :
  Config.opaque -> int -> int -> float -> float -> float * float
  = "ml_project"
external drawtile : Config.tileparams -> Config.opaque -> unit
  = "ml_drawtile"
external rectofblock : Config.opaque -> int -> int -> float array option
  = "ml_rectofblock"
external begintiles : unit -> unit = "ml_begintiles"
external endtiles : unit -> unit = "ml_endtiles"
external addannot : Config.opaque -> int -> int -> string -> unit
  = "ml_addannot"
external modannot : Config.opaque -> Config.slinkindex -> string -> unit
  = "ml_modannot"
external delannot : Config.opaque -> Config.slinkindex -> unit
  = "ml_delannot"
external hasunsavedchanges : unit -> bool = "ml_hasunsavedchanges"
external savedoc : string -> unit = "ml_savedoc"
external getannotcontents : Config.opaque -> Config.slinkindex -> string
  = "ml_getannotcontents"
external wcmd : Unix.file_descr -> bytes -> int -> unit = "ml_wcmd"
external rcmd : Unix.file_descr -> string = "ml_rcmd"
external uritolocation : string -> Config.pageno * float * float
  = "ml_uritolocation"
external isexternallink : string -> bool = "ml_isexternallink"
external copysel : Unix.file_descr -> Config.opaque -> unit = "ml_copysel"
external drawstr : int -> int -> int -> string -> float = "ml_draw_string"
external fz_version : unit -> string = "ml_fz_version"
external llpp_version : unit -> string = "ml_llpp_version"
external measurestr : int -> string -> float = "ml_measure_string"
external toutf8 : int -> string = "ml_keysymtoutf8"
external mbtoutf8 : string -> string = "ml_mbtoutf8"
