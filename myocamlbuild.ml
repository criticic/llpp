open Ocamlbuild_plugin

let () =
  Options.make_links := false

let build_mupdf _ _ =
  let open Pathname in
  let mupdf = mk "mupdf" in
  let l =
    [
      (* Cmd (S [A "make"; A "-C"; P (to_string (pwd / mupdf)); Sh "build=native"]); *)
      rm_f mupdf;
      ln_s (pwd / mupdf) mupdf;
    ]
  in
  Seq l

let version =
  getenv ~default:"" "VERSION"

let mkhelp _ _ =
  Cmd (S [A "sh"; A "./mkhelp.sh"; P "KEYS"; A version; Sh ">"; P "help.ml"])

let mkmain _ _ =
  Cmd (S [A "sed"; A "-f"; P "pp.sed"; P "main.in.ml"; Sh ">"; P "main.ml"])

let main () =
  rule "build_mupdf" ~prod:"build_mupdf" build_mupdf;
  dep ["mupdf"] ["build_mupdf"];
  pflag ["c"; "compile"] "ccopt" (fun name -> S [A "-ccopt"; A name]);
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
