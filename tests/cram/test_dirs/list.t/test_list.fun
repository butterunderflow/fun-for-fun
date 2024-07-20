external println_int : int -> unit = "ff_builtin_println_int"

external println_string : string -> unit = "ff_builtin_println_str"

external add : int -> int -> int = "ff_builtin_add"

type ('a) list =
| Cons of 'a * 'a list
| Nil

let cons = 
    fun x ->
    fun y ->
    Cons (x, y)



let lst = cons 3 (cons 2 (cons 1 Nil))

let rec iter =
    fun lst ->
    fun f ->
    match lst with
    | Cons (x, lst) -> (f x ; iter lst f)
    | Nil -> ()

let print_int_lst = fun lst -> iter lst println_int

let result = print_int_lst lst


let rec map =
    fun lst ->
    fun f ->
    match lst with
    | Cons (x, lst) -> cons (f x) (map lst f)
    | Nil -> Nil

let result = print_int_lst (map lst (add 7))

let lst = cons (2, 4) (cons (1,2) Nil)

let print_int_tu = fun tu ->
    match tu with
    | (x, y) -> println_int x; println_int y

let print_int_tu_lst = fun lst -> iter lst print_int_tu

let result = print_int_tu_lst lst

let tu_adder = fun tu ->
    match tu with
    | (x, y) -> (add x 1, add y 1)

let result = print_int_tu_lst (map lst tu_adder)

