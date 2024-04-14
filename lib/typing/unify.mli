type t

val apply : t -> Types.ty -> Types.ty

val apply_expr : t -> Typedtree.expr -> Typedtree.expr

val apply_expr_untypd : t -> Syntax.Parsetree.expr -> Syntax.Parsetree.expr

val ( <$> ) : t -> Types.ty -> Types.ty

val compose : t -> t -> t

val identity : t

val ( <.> ) : t -> t -> t

val apply_lst : t -> Types.ty list -> Types.ty list

val apply_env : t -> Env.t -> Env.t

val apply_top : t -> Typedtree.top_level -> Typedtree.top_level

val make_subst : Ident.t -> Types.ty -> t

val make_subst_lst : Ident.t list -> Types.ty list -> t

val unify : Types.ty -> Types.ty -> t

val dbg : t -> string

exception UnificationError of (string * string)
