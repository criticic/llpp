(* based on Tor Andersson's XML parser from MuPDF's XPS module *)

let r_comment_terminator = Str.regexp "-->";;
let r_CDATA_terminator = Str.regexp "\\]\\]>";;
let r_q_terminator = Str.regexp "\\?>";;

let iswhite = function
  | '\r' | '\n' | '\t' | ' ' -> true
  | _ -> false
;;

let isname = function
  | '.' | '-' | '_' | ':' -> true
  | c -> (c >= '0' && c <= '9')
         || (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
;;

exception Parse_error of string * string * int;;

let parse_error msg s pos =
  raise (Parse_error (msg, s, pos))
;;

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
      | '\"' -> Buffer.add_string b "&quot;"
      | '&' -> Buffer.add_string b "&amp;"
      | c ->
         let code = Char.code c in
         if code < 32 || code > 127
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

let unent b s pos len =
  let rec loop i =
    if i = pos + len
    then ()
    else
      let amppos =
        try
          String.index_from s i '&'
        with Not_found -> -1
      in
      if amppos = -1 || amppos >= pos + len
      then (
        Buffer.add_substring b s i (pos + len - i)
      )
      else (
        Buffer.add_substring b s i (amppos - i);
        if amppos = i + len then failwith "lonely amp";

        let semipos =
          try
            let semipos = String.index_from s (amppos+1) ';' in
            if semipos >= pos + len then raise Not_found;
            semipos
          with Not_found -> failwith "amp not followed by semicolon"
        in

        let subslen = semipos-amppos-1 in
        if subslen = 0 then failwith "empty amp";

        let subs = String.sub s (amppos+1) subslen in

        if subs.[0] = '#'
        then (
          if subslen = 1 then failwith "empty amp followed by hash";
          let code =
            if subs.[1] = 'x'
            then (
              Scanf.sscanf subs "#x%x" (fun n -> n)
            )
            else (
              int_of_string (String.sub subs 1 (subslen-1))
            )
          in
          let c = Char.unsafe_chr code in
          Buffer.add_char b c
        )
        else (
          match subs with
          | "lt" -> Buffer.add_char b '<'
          | "gt" -> Buffer.add_char b '>'
          | "amp" -> Buffer.add_char b '&'
          | "apos" -> Buffer.add_char b '\''
          | "quot" -> Buffer.add_char b '\"'
          | _ -> failwith ("unknown amp " ^ String.escaped subs)
        );
        loop (semipos+1)
      )
  in
  loop pos
;;

let subs s pos =
  let len = String.length s in
  let left = len - pos in
  if left < 0
  then
    Printf.sprintf "(pos=%d len=%d left=%d)"
                   pos len left
  else
    let len = min left 10 in
    let s = String.sub s pos len in
    s;
;;

let ts = function
  | `text -> "text"
  | `lt -> "lt"
  | `close -> "close"
  | `exclam -> "exclam"
  | `question -> "question"
  | `doctype -> "doctype"
  | `comment -> "comment"
  | `tag -> "tag"
;;

type attr = string * string
 and attrs = attr list
 and vp =
   | Vdata
   | Vcdata
   | Vopen of string * attrs * bool
   | Vclose of string
   | Vend
 and 'a v = { f : 'a v -> vp -> int -> int -> 'a v; accu : 'a }
;;

let parse v s =
  let slen = String.length s in

  let find_substr pos subs r =
    let pos =
      try
        Str.search_forward r s pos
      with Not_found ->
        parse_error ("cannot find substring " ^ subs) s pos
    in
    pos
  in
  let begins_with pos prefix = Utils.substratis s pos prefix in
  let find_non_white pos =
    let rec forward i =
      if i >= slen
      then parse_error "cannot find non white space character" s pos;
      if iswhite s.[i] then forward (i+1) else i in
    forward pos
  in

  let getname pos =
    let non_name_pos =
      let rec find_non_name i =
        if i >= slen then parse_error "cannot find non name character" s pos;
        if isname s.[i] then find_non_name (i+1) else i
      in
      find_non_name pos
    in
    non_name_pos, String.sub s pos (non_name_pos - pos)
  in

  let rec collect v pos t =
    if pos >= slen && t != `text
    then parse_error ("not enough data for " ^ ts t) s pos;

    match t with
    | `text ->
       let ltpos =
         try
           String.index_from s pos '<'
         with Not_found ->
           let rec trailsbywhite i =
             if pos+i = String.length s
             then -1
             else (
               if not (iswhite s.[pos+i])
               then parse_error "garbage at the end" s pos
               else trailsbywhite (i+1)
             )
           in
           trailsbywhite 0
       in
       if ltpos = -1
       then
         v.f v Vend pos slen, slen
       else
         let start_of_text_pos = find_non_white pos in
         let end_of_text_pos =
           if start_of_text_pos < ltpos
           then
             let rec find i =
               if i = start_of_text_pos || not (iswhite s.[i])
               then i+1
               else find (i-1)
             in
             find (ltpos-1)
           else start_of_text_pos
         in
         let v =
           if start_of_text_pos != end_of_text_pos
           then v.f v Vdata start_of_text_pos end_of_text_pos
           else v
         in
         collect v (ltpos+1) `lt

    | `lt ->
       let pos, t =
         match s.[pos] with
         | '/' -> (pos+1), `close
         | '!' -> (pos+1), `exclam
         | '?' -> (pos+1), `question
         | c when isname c -> pos, `tag
         | _ -> parse_error "invalid data after <" s pos
       in
       collect v pos t

    | `close ->
       let tag_name_pos = find_non_white pos in
       let tag_name_end_pos, close_tag_name = getname tag_name_pos in
       let close_tag_pos = find_non_white tag_name_end_pos in
       if s.[close_tag_pos] != '>'
       then parse_error "missing >" s pos;
       let pos' = close_tag_pos + 1 in
       let v = v.f v (Vclose close_tag_name) pos pos' in
       collect v pos' `text

    | `doctype ->
       let close_tag_pos =
         try
           String.index_from s pos '>'
         with Not_found ->
           parse_error "doctype is not terminated" s pos
       in
       collect v (close_tag_pos+1) `text

    | `comment ->
       let pos =
         try
           find_substr pos "-->" r_comment_terminator
         with Not_found ->
           parse_error "comment is not terminated" s pos
       in
       collect v (pos+3) `text

    | `exclam ->
       if begins_with pos "[CDATA["
       then
         let cdata_start = pos+7 in
         let cdata_end = find_substr cdata_start "]]>" r_CDATA_terminator in
         let v = v.f v Vcdata cdata_start cdata_end in
         collect v (cdata_end+3) `text
       else (
         if begins_with pos "DOCTYPE"
         then
           collect v (pos+7) `doctype
         else (
           if begins_with pos "--"
           then collect v (pos+2) `comment
           else parse_error "unknown shit after exclamation mark" s pos
         )
       )

    | `question ->
       let pos = find_substr pos "?>" r_q_terminator in
       collect v (pos+2) `text

    | `tag ->
       let pos', name = getname pos in
       let attrs, pos', closed = collect_attributes pos' in
       let v = v.f v (Vopen (name, attrs, closed)) pos pos' in
       collect v pos' `text

  and collect_attributes pos =
    let rec f accu pos =
      let nameval pos =
        let pos, name = getname pos in
        let pos = find_non_white pos in
        if s.[pos] = '='
        then
          let qpos = pos+1 in
          if qpos = slen
          then parse_error "not enough data for attribute" s pos;

          let qc = s.[qpos] in
          if not (qc = '\'' || qc = '\"')
          then parse_error "assignment is not followed by a quote" s pos;

          let closing_q_pos =
            let rec find i =
              if i = slen
              then parse_error "not enough data for attribute value" s pos;

              if s.[i] = qc then i else find (i+1)
            in
            find (qpos+1)
          in

          let vallen = closing_q_pos - (qpos+1) in
          let val' = String.sub s (qpos+1) vallen in
          (name, val'), closing_q_pos+1

          else parse_error "attribute name not followed by '='" s pos
      in

      let pos = find_non_white pos in
      if s.[pos] = '>'
      then
        accu, pos+1, false
      else (
        if slen - pos > 2 && s.[pos] = '/' && s.[pos+1] = '>'
        then
          accu, pos+2, true
        else (
          if isname s.[pos]
          then (
            let nameval, pos = nameval pos in
            let accu = nameval :: accu in
            f accu pos
          )
          else parse_error "malformed attribute list" s pos;
        )
      )
    in
    f [] pos
  in
  let _, _ = collect v 0 `text in
  v.accu;
;;
