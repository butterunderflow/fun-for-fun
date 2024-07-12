open Sexplib.Conv
module T = Syntax.Parsetree
module L = Lam.Tree

type constant = T.constant [@@deriving sexp]

type expr =
  | ETuple of expr list
  | EModObject of object_field list
      (** 1. An object can cast to another when possible ;
          2. Field of an object is visible in the scope following its declaration. *)
  | EStruct of (string * expr) list
  | EVar of string
  | EExt of string
  | ECons of int
  | EConsWith of int
  | EConst of constant
  | EApp of expr * expr list
  | ESwitch of expr * branch list
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | EClosure of closure
  | ELetRec of closure_rec * expr
  | EField of expr * string
  | ECmp of T.cmp_op * expr * expr

and pattern = L.pattern

and object_field =
  | FSimple of string * expr
  | FLetRec of closure_rec

and closure = string list * Ident.ident

and closure_rec = string list * (string * Ident.ident) list

and branch = pattern * expr

and func =
  Ident.ident (* function name *)
  * string list (* captures *)
  * string list (* parameters *)
  * expr
[@@deriving sexp]

let dbg (e, fns) =
  let buf = Buffer.create 50 in
  Printf.bprintf buf "Lifted main expression: \n";
  Printf.bprintf buf "%s\n"
    (Sexplib.Sexp.to_string_hum ~indent:2 (sexp_of_expr e));
  Printf.bprintf buf "\nGlobal C functions: \n";
  List.iter
    (fun fn ->
      Printf.bprintf buf "%s"
        (Sexplib.Sexp.to_string_hum ~indent:2 (sexp_of_func fn)))
    fns;
  Buffer.contents buf
