exception Quit;;

module E = struct
  let s = "";;
  let b = Bytes.empty;;
  let a = [||];;
end;;

type platform = | Punknown | Plinux | Pmacos | Pbsd;;

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

external measurestr : int -> string -> float = "ml_measure_string";;
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
     Printf.sprintf "%s(%s) : %s (%d)" s a (Unix.error_message e) (Obj.magic e)
  | exn -> Printexc.to_string exn
;;

let error fmt = Printf.kprintf (fun s -> failwith s) fmt;;

module IntSet = Set.Make (struct type t = int let compare = (-) end);;

let emptystr s = String.length s = 0;;
let nonemptystr s = String.length s > 0;;
let bound v minv maxv = max minv (min maxv v);;

module Opaque : sig
  type t = private string
  val of_string : string -> t
  val to_string : t -> string
end = struct
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
        else loop (Printf.sprintf "_%03d%s" h s) n
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
      (float r /. 255.0, float g /. 255.0, float b /. 255.0)
    )
;;

let rgba_of_string s =
  Scanf.sscanf
    s "%d/%d/%d/%d" (fun r g b a ->
      (float r /. 255.0, float g /. 255.0, float b /. 255.0, float a /. 255.0)
    )
;;

let color_to_string (r, g, b) =
  let r = truncate (r *. 255.0)
  and g = truncate (g *. 255.0)
  and b = truncate (b *. 255.0) in
  Printf.sprintf "%d/%d/%d" r g b
;;

let rgba_to_string (r, g, b, a) =
  let r = truncate (r *. 255.0)
  and g = truncate (g *. 255.0)
  and b = truncate (b *. 255.0)
  and a = truncate (a *. 255.0) in
  Printf.sprintf "%d/%d/%d/%d" r g b a
;;

let abspath path =
  if Filename.is_relative path
  then
    let cwd = Sys.getcwd () in
    if Filename.is_implicit path
    then Filename.concat cwd path
    else Filename.concat cwd (Filename.basename path)
  else path
;;

module Ne = struct
  let index s c = try String.index s c with Not_found -> -1;;
  let clo fd f =
    try tempfailureretry Unix.close fd
    with exn -> f @@ exntos exn
  ;;
end;;

let getoptdef def = function
  | Some a -> a
  | None -> def
;;

let getenvdef name def =
  match Sys.getenv name with
  | env -> env
  | exception Not_found -> def
;;

module Re = struct
  let crlf = Str.regexp "[\r\n]";;
  let percent = Str.regexp "%s";;
  let whitespace = Str.regexp "[ \t]";;
end;;

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
  fun s -> if Str.string_match re s 0
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

let w8 s pos i = Bytes.set s pos (Char.chr (i land 0xff));;
let r8 s pos = Char.code (Bytes.get s pos);;

let w16 s pos i =
  w8 s pos i;
  w8 s (pos+1) (i lsr 8)
;;

let w32 s pos i =
  w16 s pos i;
  w16 s (pos+2) (i lsr 16)
;;

let r16 s pos =
  let rb pos1 = Char.code (Bytes.get s (pos + pos1)) in
  (rb 0) lor ((rb 1) lsl 8)
;;

let r16s s pos =
  let i = r16 s pos in
  i - ((i land 0x8000) lsl 1)
;;

let r32 s pos =
  let rb pos1 = Char.code (Bytes.get s (pos + pos1)) in
  let l = (rb 0) lor ((rb 1) lsl 8)
  and u = (rb 2) lor ((rb 3) lsl 8) in
  (u lsl 16) lor l
;;

let r32s =
  if Sys.word_size > 32
  then fun s pos ->
       let rb pos1 = Char.code (Bytes.get s (pos + pos1)) in
       let v0 = rb 0 and v1 = rb 1 and v2 = rb 2 and v3 = rb 3 in
       let v = v0 lor (v1 lsl 8) lor (v2 lsl 16) lor (v3 lsl 24) in
       if v3 land 0x80 = 0
       then v
       else (v - (1 lsl 32))
  else fun _ _ -> error "r32s: not implemented for word_size <= 32"
;;

let vlogf = ref ignore;;
let vlog fmt = Printf.kprintf !vlogf fmt;;

let pipef ?(closew=true) cap f cmd =
  match Unix.pipe () with
  | exception exn -> dolog "%s cannot create pipe: %S" cap @@ exntos exn
  | (r, w) ->
     begin match spawn cmd [r, 0; w, -1] with
     | exception exn -> dolog "%s: cannot execute %S: %s" cap cmd @@ exntos exn
     | _pid -> f w
     end;
     Ne.clo r (dolog "%s failed to close r: %s" cap);
     if closew then Ne.clo w (dolog "%s failed to close w: %s" cap);
;;

let selstring selcmd s =
  pipef "selstring" (fun w ->
      try
        let l = String.length s in
        let bytes = Bytes.unsafe_of_string s in
        let n = tempfailureretry (Unix.write w bytes 0) l in
        if n != l
        then dolog "failed to write %d characters to sel pipe, wrote %d" l n;
      with exn -> dolog "failed to write to sel pipe: %s" @@ exntos exn
    ) selcmd
;;
