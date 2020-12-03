type t =
  | Ascii of char | Code of int | Ctrl of int | Fn of int
  | Backspace | Delete | Escape | Insert | Enter
  | Up | Down | Left | Right | Next | Prior | Home | End

let to_string = function
  | Ascii c -> Printf.sprintf "'%c'" c
  | Code c -> string_of_int c
  | Ctrl c -> Printf.sprintf "ctrl(%#x)" c
  | Fn n -> "F" ^ string_of_int n
  | Backspace -> "backspace"
  | Delete -> "delete"
  | Escape -> "escape"
  | Insert -> "insert"
  | Enter -> "enter"
  | Up -> "up"
  | Down -> "down"
  | Left -> "left"
  | Right -> "right"
  | Next -> "next"
  | Prior -> "prior"
  | Home -> "home"
  | End -> "end"
