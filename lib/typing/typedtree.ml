open Sexplib.Conv
open Types_in
module T = Syntax.Parsetree

type constant = T.constant [@@deriving sexp]

type expr =
  | EConst of constant * ty
  | EVar of string * ty
  | ELet of string * expr * expr * ty
  | ELetrec of (string * lambda_typed) list * expr * ty
  | ELam of lambda_typed
  | EIf of expr * expr * expr * ty
  | ECase of expr * (pattern * expr) list * ty
  | EApp of expr * expr * ty
  | EAnn of expr * ty
  | ETuple of expr list * ty
  | EField of mod_expr * string * ty
  | ECons of
      (* constructor like Cons *)
      string * int (* constructor id *) * ty
  | EFieldCons of
      (* constructor like M.Cons *)
      mod_expr
      * string
      * int (* constructor id *)
      * ty
  | ECmp of T.cmp_op * expr * expr * ty
  | ESeq of expr * expr * ty

and lambda_typed = string * expr * ty

and functor_para = string * mod_ty

and mod_body = top_level list

and mod_expr =
  | MEName of string * mod_ty (* M *)
  | MEStruct of mod_body * mod_ty (* struct ... end *)
  | MEFunctor of functor_para * mod_expr (* functor (M: MT) -> ... *)
  | MEField of mod_expr * string * mod_ty (* M1.M2 *)
  | MEApply of mod_expr * mod_expr * mod_ty (* M1(...) *)
  | MERestrict of mod_expr * mod_ty * mod_ty

and top_level =
  | TopLet of string * expr
  | TopLetRec of (string * lambda_typed) list
  | TopTypeDef of ty_def
  | TopMod of string * mod_expr
  | TopModSig of string * mod_ty
  | TopExternal of string * ty * string

and program = top_level list

and pattern =
  (* simplest pattern is enough after type info has been filled *)
  | PVal of constant
  | PCons of string * int * pattern option (* Cons (1, 2) *)
  | PVar of string * ty
  | PTuple of pattern list (* (x, y, z) *)
[@@deriving sexp]

let get_ty = function
  | EConst (_, ty)
  | EVar (_, ty)
  | ELet (_, _, _, ty)
  | ELetrec (_, _, ty)
  | ELam (_, _, ty)
  | EIf (_, _, _, ty)
  | ECase (_, _, ty)
  | EApp (_, _, ty)
  | EAnn (_, ty)
  | ETuple (_, ty)
  | EField (_, _, ty)
  | ECons (_, _, ty)
  | EFieldCons (_, _, _, ty)
  | ECmp (_, _, _, ty)
  | ESeq (_, _, ty) ->
      ty

let rec get_mod_ty (me : mod_expr) =
  match me with
  | MEName (_, ty)
  | MEStruct (_, ty)
  | MEField (_, _, ty)
  | MERestrict (_, _, ty)
  | MEApply (_, _, ty) ->
      ty
  | MEFunctor ((_, mt0), me1) -> MTFun (mt0, get_mod_ty me1)
