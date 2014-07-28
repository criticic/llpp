let w8 s pos i = String.set s pos (Char.chr (i land 0xff));;
    let r8 s pos = Char.code (String.get s pos)
;;

let ordermagic = 'B';;

let w16 s pos i =
  w8 s pos (i lsr 8);
  w8 s (pos+1) i
;;

let w32 s pos i =
  w16 s pos (i lsr 16);
  w16 s (pos+2) i
;;

let r16 s pos =
  let rb pos1 = Char.code (String.get s (pos + pos1)) in
  (rb 1) lor ((rb 0) lsl 8)
;;

let r16s s pos =
  let i = r16 s pos in
  i - ((i land 0x8000) lsl 1)
;;

let r32 s pos =
  let u = r16 s pos in
  let l = r16 s (pos+2) in
  (u lsl 16) lor l
;;
