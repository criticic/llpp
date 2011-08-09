open Printf

let lines =
  let lines =
    match (try Some (open_in Sys.argv.(1)) with _ -> None) with
    | None -> []
    | Some ic ->
        let lines =
          let rec fold accu =
            match (try Some (input_line ic) with _ -> None) with
            | Some line -> fold (line :: accu)
            | None -> List.rev accu
          in
          fold []
        in
        close_in ic;
        lines
  in
  lines;
;;

let _ =
  printf "let keys = [\n";
  List.iter (fun l -> printf "  %S;\n" l) lines;
  printf "];;\n"
;;
