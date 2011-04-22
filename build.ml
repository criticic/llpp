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
  let main =
    cmopp ~flags:"-g -w y -I +lablGL -thread" ~dirname:srcdir "main";
    "main.cmo"
  in
  prog "llpp" [so; main];
;;

let () =
  let objs =
    c cc "-c -Wall -Werror -Wextra -g" srcdir "test";
    ["test.o"]
  in
  let objsgl =
    c cc "-c -Wall -Werror -Wextra -g" srcdir "testgl";
    ["testgl.o"]
  in
  let prog name objs =
    gcc
      ~cc
      ~doscan:false
      ~flags1:""
      ~flags2:"-lX11 -lXext"
      ~output:name
      objs
  in
  let proggl name objs =
    gcc
      ~cc
      ~doscan:false
      ~flags1:""
      ~flags2:"-lX11 -lXext -lGL"
      ~output:name
      objs
  in
  prog "test" objs;
  proggl "testgl" objsgl;
;;

let () =
  run start jobs targets dodeplist dotarlist;
;;
