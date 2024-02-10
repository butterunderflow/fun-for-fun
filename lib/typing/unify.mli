type t

val apply : t -> Types.ty -> Types.ty

val ( <$> ) : t -> Types.ty -> Types.ty

val compose : t -> t -> t

val identity : t

val ( <.> ) : t -> t -> t

val apply_lst : t -> Types.ty list -> Types.ty list

val apply_env : t -> Env.t -> Env.t

val make_subst : string -> Types.ty -> t

val make_subst_lst : string list -> Types.ty list -> t

val unify : Types.ty -> Types.ty -> t
