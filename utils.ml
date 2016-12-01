module E = struct
  let s = "";;
  let b = Bytes.empty;;
  let a = [||];;
end;;

type platform = | Punknown | Plinux | Posx | Psun | Pbsd | Pcygwin;;

let asciilower = let auld = Char.code 'A' - Char.code 'a' in
                 function
                 | ('A'..'Z') as c -> Char.code c - auld |> Char.chr
                 | c -> c
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
external spawn : string -> (Unix.file_descr * int) list -> int = "ml_spawn";;
external platform : unit -> (platform * string array) = "ml_platform";;

let now = Unix.gettimeofday;;
let platform, uname = platform ();;
let dolog fmt = Format.ksprintf prerr_endline fmt;;

let exntos = function
  | Unix.Unix_error (e, s, a) ->
     Printf.sprintf "%s(%s) : %s (%d)"
                    s a (Unix.error_message e) (Obj.magic e)
  | exn -> Printexc.to_string exn
;;

let error fmt = Printf.kprintf (fun s -> failwith s) fmt;;

module IntSet = Set.Make (struct type t = int let compare = (-) end);;

let emptystr s = String.length s = 0;;
let nonemptystr s = String.length s > 0;;
let bound v minv maxv = max minv (min maxv v);;

let spawn cmd fda =
  if platform = Pcygwin
  then failwith "spawn not implemented under cygwin yet"
  else spawn cmd fda;
;;

module Opaque :
sig
  type t = private string
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
      let p = l-1 in
      match s.[p] with
      | 'k' | 'K' -> String.sub s 0 p, 10
      | 'm' | 'M' -> String.sub s 0 p, 20
      | 'g' | 'G' -> String.sub s 0 p, 30
      | _ -> s, 0
    else s, 0
  in
  let n = int_of_string s1 in
  let m = n lsl shift in
  if m < 0 || m < n
  then error "value too large"
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
  let clo fd f =
    try tempfailureretry Unix.close fd
    with exn -> f @@ exntos exn
  ;;
end;;

let getenvwithdef name def =
  match Sys.getenv name with
  | env -> env
  | exception Not_found -> def
;;

let newlinere = Str.regexp "[\r\n]";;
let percentsre = Str.regexp "%s";;
let whitere = Str.regexp "[ \t]";;

let unit () = ();;

let addchar s c =
  let b = Buffer.create (String.length s + 1) in
  Buffer.add_string b s;
  Buffer.add_char b c;
  Buffer.contents b;
;;

let btod b = if b then 1 else 0;;

let splitatchar s c = let open String in
                      match index s c with
                      | pos -> sub s 0 pos, sub s (pos+1) (length s - pos - 1)
                      | exception Not_found -> s, E.s
;;

let boundastep h step =
  if step < 0
  then bound step ~-h 0
  else bound step 0 h
;;

let withoutlastutf8 s =
  let len = String.length s in
  if len = 0
  then s
  else
    let rec find pos =
      if pos = 0
      then pos
      else
        let b = Char.code s.[pos] in
        if b land 0b11000000 = 0b11000000
        then pos
        else find (pos-1)
    in
    let first =
      if Char.code s.[len-1] land 0x80 = 0
      then len-1
      else find (len-1)
    in
    String.sub s 0 first;
;;

let fdcontents fd =
  let l = 4096 in
  let b = Buffer.create l in
  let s = Bytes.create l in
  let rec loop () =
    let n = tempfailureretry (Unix.read fd s 0) l in
    if n = 0
    then Buffer.contents b
    else (
      Buffer.add_subbytes b s 0 n;
      loop ()
    )
  in
  loop ()
;;

let filecontents path =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0o0 in
  match fdcontents fd with
  | exception exn ->
     error "failed to read contents of %s: %s" path @@ exntos exn
  | s ->
     Ne.clo fd @@ error "failed to close descriptor for %s: %s" path;
     s
;;

let getcmdoutput errfun cmd =
  let reperror fmt = Printf.kprintf errfun fmt in
  let clofail s e = error "failed to close %s: %s" s e in
  match Unix.pipe () with
  | exception exn ->
     reperror "pipe failed: %s" @@ exntos exn;
     E.s
  | (r, w) ->
     match spawn cmd [r, -1; w, 1] with
     | exception exn ->
        reperror "failed to execute %S: %s" cmd @@ exntos exn;
        E.s
     | pid ->
        Ne.clo w @@ clofail "write end of the pipe";
        let s =
          match Unix.waitpid [] pid with
          | exception exn ->
             reperror "waitpid on %S %d failed: %s" cmd pid @@ exntos exn;
             E.s
          | _pid, Unix.WEXITED 0 ->
             begin
               match fdcontents r with
               | exception exn ->
                  reperror "failed to read output of %S: %s" cmd @@ exntos exn;
                  E.s
               | s ->
                  let l = String.length s in
                  if l > 0 && s.[l-1] = '\n'
                  then String.sub s 0 (l-1)
                  else s
             end;
          | _pid, Unix.WEXITED n ->
             reperror "%S exited with error code %d" cmd n;
             E.s
          | _pid, Unix.WSIGNALED n ->
             reperror "%S was killed with signal %d" cmd n;
             E.s
          | _pid, Unix.WSTOPPED n ->
             reperror "%S was stopped by signal %d" cmd n;
             E.s
        in
        Ne.clo r @@ clofail "read end of the pipe";
        s
;;

let geturl =
  let re = Str.regexp {|.*\(\(https?\|ftp\|mailto\|file\)://[^ ]+\).*|} in
  fun s ->
  if Str.string_match re s 0
  then Str.matched_group 1 s
  else E.s
;;

let substratis s pos subs =
  let subslen = String.length subs in
  if String.length s - pos >= subslen
  then
    let rec cmp i = i = subslen || (s.[pos+i] = subs.[i]) && cmp (i+1)
    in cmp 0
  else false
;;
