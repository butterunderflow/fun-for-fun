external add : int -> int -> int = "ff_builtin_add"

external minus : int -> int -> int = "ff_builtin_minus"

external print_int : int -> unit = "ff_builtin_print_int"

let rec sum = fun x ->
    (if x = 0
    then 0
    else add x (sum (minus x 1)))

let result = print_int (sum 4)

