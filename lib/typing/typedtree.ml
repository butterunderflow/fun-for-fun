open Sexplib.Conv
open Types_in
module T = Syntax.Parsetree

type constant = T.constant [@@deriving sexp]

type expr =
  | Exp_const of constant * ty
  | Exp_var of string * ty
  | Exp_let of string * expr * expr * ty
  | Exp_letrec of (string * lambda_typed) list * expr * ty
  | Exp_lam of lambda_typed
  | Exp_if of expr * expr * expr * ty
  | Exp_case of expr * (pattern * expr) list * ty
  | Exp_app of expr * expr * ty
  | Exp_ann of expr * ty
  | Exp_tuple of expr list * ty
  | Exp_field of mod_expr * string * ty
  | Exp_constr of
      (* constructor like Cons *)
      string * int (* constructor id *) * ty
  | Exp_field_constr of
      (* constructor like M.Cons *)
      mod_expr
      * string
      * int (* constructor id *)
      * ty
  | Exp_cmp of T.cmp_op * expr * expr * ty
  | Exp_seq of expr * expr * ty

and lambda_typed = string * expr * ty

and functor_para = string * mod_ty

and mod_body = top_level list

and mod_expr =
  | Mod_name of string * mod_ty (* M *)
  | Mod_struct of mod_body * mod_ty (* struct ... end *)
  | Mod_functor of functor_para * mod_expr (* functor (M: MT) -> ... *)
  | Mod_field of mod_expr * string * mod_ty (* M1.M2 *)
  | Mod_apply of mod_expr * mod_expr * mod_ty (* M1(...) *)
  | Mod_restrict of mod_expr * mod_ty * mod_ty

and top_level =
  | Top_let of string * expr
  | Top_letrec of (string * lambda_typed) list
  | Top_type_def of ty_def
  | Top_mod of string * mod_expr
  | Top_mod_sig of string * mod_ty
  | Top_external of string * ty * string

and program = top_level list

and pattern =
  (* simplest pattern is enough after type info has been filled *)
  | Pat_val of constant
  | Pat_constr of string * int * pattern option (* Cons (1, 2) *)
  | Pat_var of string * ty
  | Pat_tuple of pattern list (* (x, y, z) *)
[@@deriving sexp]

let get_ty = function
  | Exp_const (_, ty)
  | Exp_var (_, ty)
  | Exp_let (_, _, _, ty)
  | Exp_letrec (_, _, ty)
  | Exp_lam (_, _, ty)
  | Exp_if (_, _, _, ty)
  | Exp_case (_, _, ty)
  | Exp_app (_, _, ty)
  | Exp_ann (_, ty)
  | Exp_tuple (_, ty)
  | Exp_field (_, _, ty)
  | Exp_constr (_, _, ty)
  | Exp_field_constr (_, _, _, ty)
  | Exp_cmp (_, _, _, ty)
  | Exp_seq (_, _, ty) ->
      ty

let rec get_mod_ty (me : mod_expr) =
  match me with
  | Mod_name (_, ty)
  | Mod_struct (_, ty)
  | Mod_field (_, _, ty)
  | Mod_restrict (_, _, ty)
  | Mod_apply (_, _, ty) ->
      ty
  | Mod_functor ((_, mt0), me1) -> Mod_ty_functor (mt0, get_mod_ty me1)
