let start = Unix.gettimeofday ();;

open List;;
open Typs;;
open Utils;;
open State;;
open Helpers;;

let jobs, targets, dodeplist, dotarlist = getopt ();;

let get key msg =
  match getval key with | None -> failwith msg | Some s -> s
;;

let getdef key def =
  match getval key with | None -> def | Some s -> s
;;

let srcdir = get "src" "no source dir";;
let cc = getdef "cc" "cc";;
let ccopt = getdef "ccopt" "";;
let mupdflibpath = get "mupdflibpath" "no mupdf libpath";;
let libs = getdef "libs" "";;

let boc flags src =
  let o = src ^ ".o" in
  let c = src ^ ".c" in
  ocaml
    "ocamlc.opt"
    ("-cc '" ^ cc ^ "' -ccopt '" ^ flags ^ " " ^ ccopt ^ " -o " ^ o ^ "'")
    o
    (StrSet.singleton o)
    [Filename.concat srcdir c]
    StrSet.empty
  ;
;;

let ocaml1 prog flags1 flags2 mainoutput outputs inputs deps f =
  let flags = flags1 ^ " -o " ^ Filename.quote mainoutput in
  let build =
    let commands _ =
      let s = String.concat " " (map Filename.quote (f ())) in
      [Run (prog ^ " " ^ flags ^ " " ^ s ^ flags2)]
    in
    let cookie _ = prog ^ flags in
    let presentation _ = "OCAMLC " ^ mainoutput in
    { get_commands = commands
    ; get_cookie = cookie
    ; get_presentation = presentation
    }
  in
  let inputs = fold_right StrSet.add inputs StrSet.empty in
  let deps = StrSet.union inputs deps in
  StrSet.iter (fun output ->
    put_build_info output build;
    let deps = StrSet.remove output deps in
    add_target output inputs outputs deps
  ) outputs;
;;

let ocaml' prog flags1 flags2 mainoutput outputs inputs deps =
  ocaml1 prog flags1 flags2 mainoutput outputs inputs deps (fun () -> inputs)
;;

let bso name objs =
  let so = name ^ ".so" in
  let mupdf_libs =
    " -L" ^ mupdflibpath ^ " " ^ libs
  in
  let o = List.map (fun s -> s ^ ".o") objs in
  ocaml'
    cc
    ("-shared -lpthread -o " ^ so ^ mupdf_libs)
    (mupdf_libs)
    so
    (StrSet.singleton so)
    o
    (StrSet.add
        (Filename.concat mupdflibpath "libfitz.a")
        (StrSet.singleton (Filename.concat mupdflibpath "libmupdf.a")))
  ;
  so
;;

let () =
  boc "-g" "link";
  let _ =
    let mkhelp = Filename.concat srcdir "mkhelp.sh" in
    let keystoml = Filename.concat srcdir "keystoml.ml" in
    let keys = Filename.concat srcdir "KEYS" in
    let cmd = "sh " ^ mkhelp ^ " " ^ keystoml ^ " " ^ keys ^ "> help.ml" in
    let build =
      { get_commands = (fun _ -> [Run cmd])
      ; get_cookie = (fun _ -> get_cmd_output "git describe --tags")
      ; get_presentation = (fun _ -> "KEYSTOML KEYS")
      }
    in
    put_build_info "help.ml" build;
    let sing s = StrSet.singleton s in
    add_target
      "help.ml"
      (sing "KEYS")
      (sing "help.ml")
      (StrSet.add keys (StrSet.add mkhelp (sing keystoml)))
  in
  let so = bso "link" ["link"] in
  let prog name cmos =
    ocaml
      "ocamlc.opt"
      ("-g -I +lablGL lablgl.cma lablglut.cma str.cma unix.cma -dllpath "
        ^ Sys.getcwd ())
      name
      (StrSet.singleton name)
      (State.dep_sort cmos)
      StrSet.empty
  in
  let mkcmo name =
    let dirname = if name = "help" then Sys.getcwd () else srcdir in
    cmopp ~flags:"-g -w Alze -I +lablGL -thread" ~dirname name;
    (name ^ ".cmo")
  in
  let cmos = so :: List.map mkcmo ["help"; "parser"; "main"] in
  prog "llpp" cmos;
;;

let () =
  run start jobs ["help.ml"] false dotarlist;
  run start jobs targets dodeplist dotarlist;
;;
