open Sexplib.Conv
open Types
module T = Tree

type constant = T.constant [@@deriving sexp]

type pattern = T.pattern [@@deriving sexp]

type path = T.path [@@deriving sexp]

[@@@warning "-17"]

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
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "tree_iter" },
    visitors { variety = "map"; name = "tree_map" }]

class virtual ['self] map =
  object (_self : 'self)
    inherit ['self] Syntax.Parsetree.constant_map

    inherit! ['self] Syntax.Parsetree.pattern_map

    inherit! ['self] Syntax.Parsetree.path_map

    inherit! ['self] Syntax.Parsetree.type_map

    inherit! ['self] tree_map
  end

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
