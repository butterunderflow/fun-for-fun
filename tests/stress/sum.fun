external add : int -> int -> int = "ff_builtin_add"

external minus : int -> int -> int = "ff_builtin_minus"

external println_int : int -> unit = "ff_builtin_println_int"

external read_int : unit -> int = "ff_builtin_read_int"

let rec sum = fun x ->
    (if x = 0
    then 0
    else add x (sum (minus x 1)))

let result = println_int (sum (read_int ()))

