open Ocamlbuild_plugin

let lablGL_headers =
  let l =
    [
      "raw_tags.h";
      "ml_raw.h";
      "gl_tags.h";
      "ml_gl.h";
      "gl_tags.c";
    ]
  in
  List.map (fun h -> "lablGL/" ^ h) l

let command fmt =
  let buffer_size = 2048 in
  let buffer = Buffer.create buffer_size in
  let string = Bytes.create buffer_size in
  Printf.ksprintf (fun cmd ->
      try
        let in_channel = Unix.open_process_in cmd in
        let chars_read = ref 1 in
        while !chars_read <> 0 do
          chars_read := input in_channel string 0 buffer_size;
          Buffer.add_substring buffer string 0 !chars_read
        done;
        ignore (Unix.close_process_in in_channel);
        Buffer.contents buffer
      with _ ->
        Printf.ksprintf failwith "Fatal error: %S failed." cmd
    ) fmt

let read_process_lines fmt =
  Printf.ksprintf (fun cmd ->
      try
        let lines = ref [] in
        let in_channel = Unix.open_process_in cmd in
        begin
          try
            while true do
              lines := input_line in_channel :: !lines
            done;
          with End_of_file ->
            ignore (Unix.close_process_in in_channel)
        end;
        List.rev !lines
      with _ ->
        Printf.ksprintf failwith "Fatal error: %S failed." cmd
    ) fmt

let uname =
  String.trim (command "uname")

let opengl_include =
  if uname = "Darwin" then
    S [A "-ccopt"; A "-I /opt/X11/include"]
  else
    N

let build_mupdf _ _ =
  let l =
    [
      rm_f (Pathname.mk "mupdf");
      ln_s (Pathname.pwd / Pathname.mk "mupdf") (Pathname.mk "mupdf");
    ]
  in
  Seq l

let version =
  let s = String.trim (command "git describe --tags 2>/dev/null") in
  if s = "" then
    "unknown"
  else
    s

(* let () = *)
(*   Printf.eprintf "VERSION = %s\n%!" version *)

let mkhelp _ _ =
  Cmd (S [A "sh"; A "./mkhelp.sh"; P "KEYS"; A version; Sh ">"; P "help.ml"])

let mkmain _ _ =
  Cmd (S [A "sed"; A "-f"; P "pp.sed"; P "main.in.ml"; Sh ">"; P "main.ml"])

let main () =
  rule "build_mupdf" ~prod:"build_mupdf" build_mupdf;
  dep ["mupdf"] ["build_mupdf"];
  dep ["c"; "compile"; "lablGL_headers"] lablGL_headers;
  pflag ["c"; "compile"] "ccopt" (fun name -> S [A "-ccopt"; A name]);
  flag ["c"; "compile"; "use_opengl"] opengl_include;
  pdep [] "autodep" (fun param -> [Pathname.mk param]);
  rule "make help.ml" ~insert:`top ~prod:"help.ml" ~deps:["KEYS"; "mkhelp.sh"] mkhelp;
  rule "make main.ml" ~insert:`top ~prod:"main.ml" ~deps:["main.in.ml"; "pp.sed"] mkmain;
  pflag ["link"] "cclib" (fun name -> S[A"-cclib"; A name]);
  pflag ["link"] "link" (fun name -> P name)

let () =
  dispatch (function
      | After_options -> main ()
      | _ -> ()
    )
