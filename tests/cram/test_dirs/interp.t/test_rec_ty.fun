external println_str : string -> unit = "ff_builtin_println_str"

external add : int -> int -> int = "ff_builtin_add"

type 'a tree =
  | Leaf of 'a
  | Node of ('a * 'a forest)

and 'a forest =
  | EmptyForest
  | ConsForest of ('a tree * 'a forest)

let x = Node (1, ConsForest (Leaf 1, EmptyForest))

let _ =
  match x with
  | Node (x, ConsForest (Leaf 2, EmptyForest)) -> println_str "Matched first!"
  | Node (x, ConsForest (Leaf 1, EmptyForest)) -> println_str "Matched second!"
