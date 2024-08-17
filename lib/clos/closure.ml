open Sexplib.Conv
module T = Syntax.Parsetree
module L = Lam.Tree

type constant = T.constant [@@deriving sexp]

type expr =
  | Exp_tuple of expr list
  | Exp_mod_obj of object_field list
      (** 1. An object can cast to another when possible ;
          2. Field of an object is visible in the scope following its declaration. *)
  | Exp_struct of (string * expr) list
  | Exp_var of string
  | Exp_external of string
  | Exp_constr of int
  | Exp_payload_constr of int
  | Exp_const of constant
  | Exp_app of expr * expr list
  | Exp_switch of expr * branch list
  | Exp_let of string * expr * expr
  | Exp_if of expr * expr * expr
  | Exp_closure of closure
  | Exp_letrec of closure_rec * expr
  | Exp_field of expr * string
  | Exp_cmp of T.cmp_op * expr * expr
  | Exp_seq of expr * expr

and pattern = L.pattern

and object_field =
  | Field_simple of string * expr
  | Field_letrec of closure_rec

and closure = string list * Ident.ident

and closure_rec = string list * (string * Ident.ident) list

and branch = pattern * expr

and func =
  Ident.ident (* function name *)
  * string list (* captures *)
  * string list (* parameters *)
  * expr
[@@deriving sexp]

let dbg (main, fns) =
  let buf = Buffer.create 50 in
  Printf.bprintf buf "Main function: \n";
  Printf.bprintf buf "%s" (Ident.to_string main);
  Printf.bprintf buf "\nGlobal C functions: \n";
  List.iter
    (fun fn ->
      Printf.bprintf buf "%s"
        (Sexplib.Sexp.to_string_hum ~indent:2 (sexp_of_func fn)))
    fns;
  Buffer.contents buf
