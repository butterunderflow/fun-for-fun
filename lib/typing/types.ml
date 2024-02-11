open Sexplib.Conv
module Tree = Syntax.Parsetree

type ty = Tree.type_expr

and mod_ty = Tree.mod_type

and ty_def = Tree.type_def

and bind_ty = string list * ty [@@deriving sexp]

let int_ty = Tree.TCons ("int", [])

let string_ty = Tree.TCons ("string", [])

let bool_ty = Tree.TCons ("bool", [])
