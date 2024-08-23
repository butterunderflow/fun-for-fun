external println_str : string -> unit = "ff_builtin_println_str"

let x = assert true

let x = println_str "A true asserted!"

let x = assert false
