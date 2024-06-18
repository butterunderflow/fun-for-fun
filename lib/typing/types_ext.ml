open Sexplib.Conv
open Syntax.Parsetree


(* forall x y z. x -> y -> z *)
type bind_ty = Ident.ident list * ety [@@deriving sexp]

