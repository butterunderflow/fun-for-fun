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
  | ELet of string * expr * expr * ty
  | ELetrec of (string * lambda_typed) list * expr * ty
  | ELam of lambda_typed
  | EIf of expr * expr * expr * ty
  | ECase of expr * (pattern * expr) list * ty
  | EApp of expr * expr * ty
  | EAnn of expr * ty
  | ETuple of expr list * ty
  | EField of path * ty
  | ECons of string * ty
  | EFieldCons of path * string * ty

and lambda_typed = string * expr * ty

and functor_para = string * T.mod_type

and functor_expr = functor_para * mod_expr

and mod_body = top_level list

and mod_expr =
  | MEName of string (* M *)
  | MEStruct of mod_body (* struct ... end *)
  | MEFunctor of functor_expr (* functor (M: MT) -> ... *)
  | MEField of path * string (* M1.M2 *)
  | MEApply of mod_expr * mod_expr (* M1(...) *)
  | MERestrict of mod_expr * T.mod_type (* M: M_ty *)

and top_level =
  | TopLet of string * expr * ty
  | TopLetRec of (string * lambda_typed) list
  | TopTypeDef of T.type_def
  | TopMod of string * mod_expr * T.mod_type

and program = top_level list
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "tree_iter" },
    visitors { variety = "map"; name = "tree_map" }]

class virtual ['self] map =
  object (self : 'self)
    inherit ['self] Syntax.Parsetree.constant_map

    inherit! ['self] Syntax.Parsetree.pattern_map

    inherit! ['self] Syntax.Parsetree.path_map

    inherit! ['self] Syntax.Parsetree.type_map

    method visit_ident env id =
      Ident.mk_ident
        (self#visit_int env (Ident.index_of_ident id))
        (self#visit_string env (Ident.name_of_ident id))

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
  | EField (_, ty)
  | ECons (_, ty)
  | EFieldCons (_, _, ty) ->
      ty
