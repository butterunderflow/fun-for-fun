external minus : int -> int -> int = "ff_builtin_minus"

external println_str : string -> unit = "ff_builtin_println_str"

let even =
  (let rec even = fun x ->
    match x with
    | 0 -> true
    | _ -> odd (minus x 1)
  and odd = fun x ->
    match x with
    | 0 -> false
    | _ -> even (minus x 1)
  in
  even)


let print_is_even = (fun x ->
  if (even x) then println_str "true" else println_str "false")

let _ = print_is_even 0

let _ = print_is_even 1

let _ = print_is_even 2

let _ = print_is_even 3

let _ = print_is_even 4

let _ = print_is_even 100


