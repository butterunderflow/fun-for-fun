external print_bool : bool -> int = "ff_builtin_print_bool"

let x = print_bool (1 = 1)

let x = print_bool (1 <> 1)


let z = (
    let tu1 = (1, 2) in
    let tu2 = (1, 2) in
    print_bool (tu1 = tu2))

let z = 
    let tu1 = (1, 2) in
    let tu2 = (1, 3) in
    print_bool (tu1 = tu2)
