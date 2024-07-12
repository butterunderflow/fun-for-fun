val free_var_analyze : Tree.expr -> unit

val compile_program : Typing.Typedtree.program -> Tree.expr

val get_pat_vars : Tree.pattern -> string list
