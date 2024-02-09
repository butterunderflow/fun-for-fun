open Types
module T = Tree

type constant = T.constant

type pattern =
  | PVal of constant
  | PCons of string * pattern list
  | PVar of string

type path = T.path

type expr =
  | EConst of constant * ty
  | EVar of string * ty
  | ELet of pattern * expr * expr * ty
  | ELetrec of (string * lambda_typed) list * expr * ty
  | ELam of lambda_typed
  | EIf of expr * expr * expr * ty
  | ECase of expr * (pattern * expr) list * ty
  | EApp of expr * expr * ty
  | EAnn of expr * ty
  | ETuple of expr list * ty
  | EField of path * ty

and lambda_typed = string * expr * ty

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
  | EField (_, ty) ->
      ty
