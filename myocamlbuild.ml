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

let build_mupdf _ _ =
  ln_s (Pathname.parent_dir_name / Pathname.mk "mupdf") (Pathname.mk "mupdf")

let () =
  dispatch (function
      | After_options ->
        (* rule "build mupdf" ~prod:"build_mupdf" build_mupdf; *)
        dep ["mupdf"] ["build_mupdf"];
        dep ["c"; "compile"] lablGL_headers;
        pflag ["c"; "compile"] "add_c_include"
          (fun name -> S [A "-ccopt"; A ("-I " ^ name)])
      | _ -> ()
    )
