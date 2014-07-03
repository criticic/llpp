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
  let d =
    (* Since we are using ocaml instead of gcc to compile this
       the dep scanning passs is not done, hence this cludge *)
    List.fold_left
      (fun s p -> StrSet.add (Filename.concat srcdir p) s)
      StrSet.empty
      ["glfont.c"; "keysym2ucs.c"]
  in
  ocaml
    "ocamlc.opt"
    ("-cc '" ^ cc ^ "' -ccopt '" ^ flags ^ " " ^ ccopt ^ " -o " ^ o ^ "'")
    o
    (StrSet.singleton o)
    [Filename.concat srcdir c]
    d
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
  let libs = ["mupdf"] in
  let set =
    List.fold_left (fun set s ->
      let l = "lib" ^ s ^ ".a" in
      let l = Filename.concat mupdflibpath l in
      StrSet.add l set)
      StrSet.empty libs
  in
  ocaml'
    cc
    ("-shared -lpthread -lfontconfig")
    (mupdf_libs)
    so
    (StrSet.singleton so)
    o
    set
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
      ; get_cookie = (fun _ ->
        let cwd = Sys.getcwd () in
        Sys.chdir srcdir;
        let s = get_cmd_output "git describe --tags --dirty || echo" in
        Sys.chdir cwd;
        s)
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
      ("-g -I +lablGL lablgl.cma str.cma unix.cma -dllpath "
        ^ Sys.getcwd ())
      name
      (StrSet.singleton name)
      (State.dep_sort cmos)
      StrSet.empty
  in
  let mkcmo name =
    let dirname = if name = "help" then Sys.getcwd () else srcdir in
    cmopp ~flags:"-g -w A-7-6-4 -I +lablGL -thread" ~dirname name;
    (name ^ ".cmo")
  in
  let cmos = so :: List.map mkcmo
                    ["help"; "utils"; "parser"; "wsi"; "config"; "main"] in
  prog "llpp" cmos;
;;

let () =
  run start jobs ["help.ml"] false dotarlist;
  run start jobs targets dodeplist dotarlist;
;;
