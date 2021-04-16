type t =
    Ascii of char
  | Code of int
  | Ctrl of int
  | Fn of int
  | Backspace
  | Delete
  | Escape
  | Insert
  | Enter
  | Up
  | Down
  | Left
  | Right
  | Next
  | Prior
  | Home
  | End
val to_string : t -> string
