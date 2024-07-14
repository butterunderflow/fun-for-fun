external add : int -> int -> int = "ff_builtin_add"

external minus : int -> int -> int = "ff_builtin_minus"

external println_int : int -> unit = "ff_builtin_println_int"

external read_int : unit -> int = "ff_builtin_read_int"

let rec fib = fun x ->
    (if x = 0
    then 1
    else (if x = 1
          then 1
          else (add (fib (minus x 1)) (fib (minus x 2)))))


let result = println_int (fib 10)
