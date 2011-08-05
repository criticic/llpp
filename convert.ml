open Format;;

let enent s pos len =
  let b = Buffer.create len in
  let rec loop i =
    if i - pos = len
    then Buffer.contents b
    else (
      begin match s.[i] with
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '\'' -> Buffer.add_string b "&apos;"
      | '"' -> Buffer.add_string b "&quot;"
      | '&' -> Buffer.add_string b "&amp;"
      | c ->
          let code = Char.code c in
          if code = 0 || code > 0x7f
          then (
            Buffer.add_string b "&#";
            Buffer.add_string b (string_of_int code);
            Buffer.add_char b ';';
          )
          else Buffer.add_char b c
      end;
      loop (i+1)
    )
  in
  loop pos
;;

let main statepath =
  let hash =
    try
      let ic = open_in_bin statepath in
      let hash = input_value ic in
      close_in ic;
      hash
    with exn ->
      if false
      then
        prerr_endline ("Error loading state " ^ Printexc.to_string exn)
      ;
      Hashtbl.create 1
  in
  print_endline "<llppconfig>";
  Hashtbl.iter (fun path (bookmarks, w, h) ->
    if false
    then (
      printf "<doc width=\"%d\" height=\"%d\">\n"
        w h
      ;
      printf "  <![CDATA[%s]]>\n" path;
    )
    else (
      printf "<doc path=\"%s\" width=\"%d\" height=\"%d\">\n"
        (enent path 0 (String.length path)) w h
      ;
    );

    if bookmarks <> []
    then (
      printf "  <bookmarks>\n";

      List.iter (fun (title, level, page, rely) ->
        printf
          (* "    <bookmark page=\"%d\" rely=\"%f\"><![CDATA[%s]]></bookmark>\n" *)
          "    <item title=\"%s\" page=\"%d\" rely=\"%f\"/>\n"
          title page rely) bookmarks;
      printf "  </bookmarks>";
    );
    printf "</doc>\n@.";
  ) hash;
  print_endline "</llppconfig>";
;;

let _ = main Sys.argv.(1);;
