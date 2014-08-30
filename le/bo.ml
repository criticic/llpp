let w8 s pos i = Bytes.set s pos (Char.chr (i land 0xff));;
let r8 s pos = Char.code (Bytes.get s pos);;

let ordermagic = 'l';;

let w16 s pos i =
  w8 s pos i;
  w8 s (pos+1) (i lsr 8)
;;

let w32 s pos i =
  w16 s pos i;
  w16 s (pos+2) (i lsr 16)
;;

let r16 s pos =
  let rb pos1 = Char.code (Bytes.get s (pos + pos1)) in
  (rb 0) lor ((rb 1) lsl 8)
;;

let r16s s pos =
  let i = r16 s pos in
  i - ((i land 0x8000) lsl 1)
;;

let r32 s pos =
  let rb pos1 = Char.code (Bytes.get s (pos + pos1)) in
  let l = (rb 0) lor ((rb 1) lsl 8)
  and u = (rb 2) lor ((rb 3) lsl 8) in
  (u lsl 16) lor l
;;
