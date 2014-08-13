module E = struct
  let s = ""
  let a = [||];;
end;;

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

module Opaque :
sig
  type t =  private string
  val of_string : string -> t
  val to_string : t -> string
end
  =
struct
  type t = string
  let of_string s = s
  let to_string t = t
end
;;

let (~<) = Opaque.of_string;;
let (~>) = Opaque.to_string;;

let int_of_string_with_suffix s =
  let l = String.length s in
  let s1, shift =
    if l > 1
    then
      let suffix = Char.lowercase s.[l-1] in
      match suffix with
      | 'k' -> String.sub s 0 (l-1), 10
      | 'm' -> String.sub s 0 (l-1), 20
      | 'g' -> String.sub s 0 (l-1), 30
      | _ -> s, 0
    else s, 0
  in
  let n = int_of_string s1 in
  let m = n lsl shift in
  if m < 0 || m < n
  then raise (Failure "value too large")
  else m
;;

let string_with_suffix_of_int n =
  if n = 0
  then "0"
  else
    let units = [(30, "G"); (20, "M"); (10, "K")] in
    let prettyint n =
      let rec loop s n =
        let h = n mod 1000 in
        let n = n / 1000 in
        if n = 0
        then string_of_int h ^ s
        else (
          let s = Printf.sprintf "_%03d%s" h s in
          loop s n
        )
      in
      loop E.s n
    in
    let rec find = function
      | [] -> prettyint n
      | (shift, suffix) :: rest ->
          if (n land ((1 lsl shift) - 1)) = 0
          then prettyint (n lsr shift) ^ suffix
          else find rest
    in
    find units
;;

let color_of_string s =
  Scanf.sscanf s "%d/%d/%d" (fun r g b ->
    (float r /. 256.0, float g /. 256.0, float b /. 256.0)
  )
;;

let color_to_string (r, g, b) =
  let r = truncate (r *. 256.0)
  and g = truncate (g *. 256.0)
  and b = truncate (b *. 256.0) in
  Printf.sprintf "%d/%d/%d" r g b
;;

let abspath path =
  if Filename.is_relative path
  then
    let cwd = Sys.getcwd () in
    if Filename.is_implicit path
    then Filename.concat cwd path
    else Filename.concat cwd (Filename.basename path)
  else
    path
;;

let nindex s c =
  try String.index s c
  with Not_found -> -1
;;

module Ne = struct
  type 'a t = | Res of 'a | Exn of exn;;

  let res f arg =
    try Res (f arg)
    with exn -> Exn exn
  ;;

  let clo fd f =
    try tempfailureretry Unix.close fd
    with exn -> f (exntos exn)
  ;;

  let dup fd =
    try Res (tempfailureretry Unix.dup fd)
    with exn -> Exn exn
  ;;

  let dup2 fd1 fd2 =
    try Res (tempfailureretry (Unix.dup2 fd1) fd2)
    with exn -> Exn exn
  ;;
end;;
