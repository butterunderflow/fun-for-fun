val unify : Types_in.ty -> Types_in.ty -> unit

val occur : Types_in.tv ref -> Types_in.ty -> bool

exception UnificationError of (string * string)

exception OccurError of (Types_in.tv ref * Types_in.ty)
