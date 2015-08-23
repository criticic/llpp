let r_dash = Str.regexp " - ";;
let tabify s =
  let dashpos = try Str.search_forward r_dash s 0 with Not_found -> -1 in
  if dashpos < 1
  then
    let l = String.length s in
    if l > 11 && String.sub s 0 5 = "-----"
    then
      "\xc2\xb7" ^ String.sub s 5 (l - 10)
    else
      s
  else
    let rec findnonwsback i =
      if i = -1 then 0 else
        if s.[i] = ' '
        then findnonwsback (i-1)
        else i
    in
    let nonwspos = findnonwsback dashpos in
    let b = Buffer.create 80 in
    Buffer.add_substring b s 0 (nonwspos+1);
    Buffer.add_char b '\t';
    Buffer.add_substring b s (dashpos+1) (String.length s - dashpos - 1);
    Buffer.contents b
;;

let lines =
  let lines =
    let rec fold accu =
      match input_line stdin with
      | line -> fold (tabify line :: accu)
      | exception End_of_file -> List.rev accu
    in
    fold []
  in
  lines
;;

let _ =
  print_endline "let keys = [";
  List.iter (Printf.printf "  %S;\n") lines;
  print_endline "];;"
;;
