
external add : int -> int -> int = "ff_builtin_add"

external print_int : int ->  int = "ff_builtin_print_int"


let y = ((add 1) 2)

let z = print_int y


