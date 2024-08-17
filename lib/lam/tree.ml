open Sexplib.Conv
module T = Syntax.Parsetree

type constant = T.constant [@@deriving sexp]

type expr =
  | Exp_tuple of expr list
  | Exp_mod_obj of object_field list
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
  | Exp_lam of lambda
  | Exp_letrec of (string * lambda) list * expr
  | Exp_field of expr * string
  | Exp_cmp of T.cmp_op * expr * expr
  | Exp_seq of expr * expr

and pattern =
  | Pat_var of string
  | Pat_val of constant
  | Pat_constr of int * pattern option
  | Pat_tuple of pattern list

and object_field =
  | Field_simple of string * expr
  | Field_letrec of (string * lambda) list

and lambda =
  string list (* lambda parameters *)
  * expr
  * string list ref (* free variables, filled in free varialble analysis *)

and branch = pattern * expr [@@deriving sexp]

let dbg e =
  let s = sexp_of_expr e in
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s
