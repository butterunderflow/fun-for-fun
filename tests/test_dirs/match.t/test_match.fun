external print_int : int ->  int = "ff_builtin_print_int"

type () int_l
= Cons of int
| Nil

let x = Nil
let f = (fun x ->
    match x with
    | Cons x -> print_int x
    | Nil    -> print_int 1)

let n = (f Nil)

let n = (f (Cons 998))


type () int_tu
= Tu of (int * int)


let n = Tu (10, 22)

let m = (match n with
         | Tu (a, b) -> print_int a)

let m = (match n with
         | Tu (a, b) -> print_int b)
