#!/bin/sh
set -e
ocaml str.cma -stdin "$1" "$2" <<EOF
let fixup = let open Str in
  let dash = regexp {|\([^ ]*\) +- +\(.*\)|}
  and head = regexp {|-----\(.*\)-----|} in fun s ->
  String.escaped s |> global_replace dash {|\1\t\2|}
                   |> global_replace head {|\xc2\xb7\1|};;
let rec iter ic = match input_line ic with
| s -> Printf.printf "\"%s\";\\n" @@ fixup s; iter ic
| exception End_of_file -> ();;
Printf.printf "let keys = [\\n";
iter @@ open_in Sys.argv.(1);;
Printf.printf "];;\\nlet version = %S;;\\n" Sys.argv.(2);;
EOF
