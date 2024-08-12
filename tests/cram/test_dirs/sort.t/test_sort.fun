external greater : int -> int -> bool = "ff_builtin_greater"

external print_int : int -> unit = "ff_builtin_print_int"

external print_string : string -> unit = "ff_builtin_print_str"

module type Cmp = sig
  type () t

  val compare : t -> t -> bool
end

type 'a lst =
  | Cons of ('a * 'a lst)
  | Nil

module Sorting = functor (C : Cmp) -> struct
  let rec bubble_sort = fun lst ->
    let rec pass = fun l ->
      match l with
      | Nil -> Nil
      | Cons (_, Nil) -> l
      | Cons (x, Cons (y, tl)) ->
          if C.compare x y then Cons (y, pass (Cons (x, tl)))
          else Cons (x, pass (Cons (y, tl)))
    in
    let sorted = pass lst in
    if sorted = lst then lst else bubble_sort sorted
end

module Int = struct
  type t = int

  let compare = greater
end

module ListIntSorting = Sorting (Int)

let print_int_list = fun l ->
  (let rec go = fun l ->
    match l with
    | Nil -> ()
    | Cons (head, rest) -> (
        print_int head;
        print_string "; ";
        go rest)
  in (
  print_string "[";
  go l;
  print_string "]\n"))

let l1 =
  Cons
    ( 1,
      Cons
        (9, Cons (9, Cons (8, Cons (1, Cons (0, Cons (2, Cons (2, Nil)))))))
    )

let l2 =
  Cons
    ( 2,
      Cons
        (0, Cons (2, Cons (4, Cons (0, Cons (8, Cons (1, Cons (2, Nil)))))))
    )


let _ = print_int_list l1

let _ = print_int_list (ListIntSorting.bubble_sort l1)

let _ = print_int_list l2

let _ = print_int_list (ListIntSorting.bubble_sort l2)
