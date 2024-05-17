open Sexplib.Conv
module Tree = Syntax.Parsetree

type ty = Tree.type_expr

and mod_ty = Tree.mod_type

and ty_def = Tree.type_def

(* forall x y z. x -> y -> z *)
and bind_ty = Ident.ident list * ty [@@deriving sexp]


let same ty0 ty1 = ty0 = ty1

let int_ty = Tree.TCons ("int", [])

let string_ty = Tree.TCons ("string", [])

let bool_ty = Tree.TCons ("bool", [])
