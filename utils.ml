type platform =
  | Punknown | Plinux | Posx | Psun | Pfreebsd
  | Pdragonflybsd | Popenbsd | Pnetbsd | Pcygwin
;;

let tempfailureretry f a =
  let rec g () =
    try f a with Unix.Unix_error (Unix.EINTR, _, _) -> g ()
  in g ()
;;

external cloexec : Unix.file_descr -> unit = "ml_cloexec";;
external hasdata : Unix.file_descr -> bool = "ml_hasdata";;
external toutf8 : int -> string = "ml_keysymtoutf8";;
external mbtoutf8 : string -> string = "ml_mbtoutf8";;
external popen : string -> (Unix.file_descr * int) list -> unit = "ml_popen";;
external platform : unit -> platform = "ml_platform";;

let now = Unix.gettimeofday;;
let platform = platform ();;
let dolog fmt = Format.kprintf prerr_endline fmt;;

let exntos = function
  | Unix.Unix_error (e, s, a) -> Printf.sprintf "%s(%s) : %s (%d)"
      s a (Unix.error_message e) (Obj.magic e)
  | exn -> Printexc.to_string exn;
;;

let error fmt = Printf.kprintf failwith fmt;;

module IntSet = Set.Make (struct type t = int let compare = (-) end);;

let emptystr s = String.length s = 0;;
let nonemptystr s = String.length s > 0;;
let bound v minv maxv = max minv (min maxv v);;

let popen cmd fda =
  if platform = Pcygwin
  then (
    let sh = "/bin/sh" in
    let args = [|sh; "-c"; cmd|] in
    let rec std si so se = function
      | [] -> si, so, se
      | (fd, 0) :: rest -> std fd so se rest
      | (fd, -1) :: rest ->
          Unix.set_close_on_exec fd;
          std si so se rest
      | (_, n) :: _ ->
          failwith ("unexpected fdn in cygwin popen " ^ string_of_int n)
    in
    let si, so, se = std Unix.stdin Unix.stdout Unix.stderr fda in
    ignore (Unix.create_process sh args si so se)
  )
  else popen cmd fda;
;;
