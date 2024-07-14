external print_string : string -> unit = "ff_builtin_print_str"

let x = 1

let y = 4

let z = 10

let test = fun x ->
    match x with
    | 1 -> print_string "one\n"
    | 4 -> print_string "four\n"
    | x -> print_string "unhandled case\n"

let result =
    test x;
    test y;
    test z
