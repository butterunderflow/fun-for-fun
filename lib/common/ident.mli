type t
[@@deriving sexp, compare]

val same : t -> t -> bool

val from : string -> t

val create : hint:string -> t

val rename : t -> t
