module Tree = Syntax.Parsetree

type ty = Tree.type_expr

type mod_ty = Tree.mod_type

type ty_def = Tree.type_def

type bind_ty = string list * ty

let int_ty = Tree.TCons ("int", [])

let string_ty = Tree.TCons ("string", [])

let bool_ty = Tree.TCons ("bool", [])
