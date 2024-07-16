val unify : Types_in.ty -> Types_in.ty -> unit

val occur : Types_in.tv ref -> Types_in.ty -> bool

exception UnificationError of (Types_in.ty * Types_in.ty)

exception OccurError of (Types_in.tv ref * Types_in.ty)
