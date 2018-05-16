open Utils;;

external fz_version : unit -> string = "ml_fz_version";;

let gotourl launcher url =
  let command = Str.global_replace percentsre url launcher in
  try ignore @@ spawn command []
  with exn -> dolog "failed to execute `%s': %s" command @@ exntos exn
;;

let gotouri launcher uri =
  if emptystr launcher
  then dolog "%s" uri
  else
    match geturl uri with
    | "" -> dolog "obtained empty url from uri %S" uri
    | url -> gotourl launcher url
;;

let version () =
  Printf.sprintf "llpp version %s, fitz %s, ocaml %s/%d bit"
                 Help.version (fz_version ()) Sys.ocaml_version Sys.word_size
;;

let makehelp launcher =
  let strings =
    version ()
    :: "(searching in this text works just by typing (i.e. no initial '/'))"
    :: E.s :: Help.keys
  in
  List.map (fun s ->
      match geturl s with
      | "" -> (s, 0, Config.Noaction)
      | url -> (s, 0, Action (fun uioh -> gotourl launcher url; uioh))
    ) strings;
;;
