external println_str : string -> unit = "ff_builtin_println_str"

type t =
  | Cons1
  | Cons2

let _ =
  match Cons1 with
  | Cons2 -> println_str "internal error, this should never matched"

let _ = assert false
