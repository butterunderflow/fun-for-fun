val compare_string : String.t -> String.t -> int

val compare_int : int -> int -> int

type ident [@@deriving sexp, show]

type t = ident [@@deriving compare]

val name_of_ident : ident -> string

val index_of_ident : ident -> int

val mk_ident : int -> string -> ident

val same : ident -> ident -> bool

val from : string -> ident

val create : hint:string -> ident

val rename : ident -> ident

val to_string : ident -> string

val refresh : unit -> unit
