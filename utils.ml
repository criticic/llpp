let tempfailureretry f a =
  let rec g () =
    try f a with Unix.Unix_error (Unix.EINTR, _, _) -> g ()
  in g ()
;;

external cloexec : Unix.file_descr -> unit = "ml_cloexec";;
external hasdata : Unix.file_descr -> bool = "ml_hasdata";;
external toutf8 : int -> string = "ml_keysymtoutf8";;
external mbtoutf8 : string -> string = "ml_mbtoutf8";;

let dolog fmt = Format.kprintf prerr_endline fmt;;

let exntos = function
  | Unix.Unix_error (e, s, a) -> Printf.sprintf "%s(%s) : %s (%d)"
      s a (Unix.error_message e) (Obj.magic e)
  | exn -> Printexc.to_string exn;
;;

let error fmt = Printf.kprintf failwith fmt;;

module IntSet = Set.Make (struct type t = int let compare = (-) end);;
