[@@@warning "-17"]

open Sexplib.Conv

type constant =
  | CBool of bool
  | CInt of int
  | CString of string
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "constant_iter" },
    visitors { variety = "map"; name = "constant_map" }]

type program = top_level list

and top_level =
  | TopLet of string * expr
  | TopLetRec of (string * lambda) list
  | TopTypeDef of ety_def
  | TopMod of string * mod_expr
  | TopModSig of string * emod_ty

and para =
  | PAnn of string * ety
  | PBare of string

and paras = para list

and expr =
  | EConst of constant
  | EVar of string
  | ECons of string
  | ELet of string * expr * expr
  | ELetrec of (string * lambda) list * expr
  | ELam of lambda
  | EIf of expr * expr * expr
  | ECase of expr * (pattern * expr) list
  | EApp of expr * expr
  | EAnn of expr * ety
  | ETuple of expr list
  | EField of mod_expr * string
  | EFieldCons of mod_expr * string

and pattern =
  | PVal of constant
  | PCons of string * pattern option (* Cons (1, 2) *)
  | PFieldCons of mod_expr * string * pattern option (* T.M(M2).C (1, 2) *)
  | PVar of string
  | PTuple of pattern list (* (x, y, z) *)

and lambda = para * expr

and mod_body = top_level list

and mod_expr =
  | MEName of string (* M *)
  | MEStruct of mod_body (* struct ... end *)
  | MEFunctor of functor_expr (* functor (M: MT) -> ... *)
  | MEField of mod_expr * string (* M1.M2 *)
  | MEApply of mod_expr * mod_expr (* M1(...) *)
  | MERestrict of mod_expr * emod_ty (* M: M_ty *)

and functor_para = string * emod_ty

and functor_expr = functor_para * mod_expr

and adt_def = string * ety_paras * evariant list

and ety_comp =
  | TValueSpec of string * ety
  | TAbstTySpec of string * ety_paras
  | TManiTySpec of ety_def
  | TModSpec of (string * emod_ty)

and ety =
  | TField of mod_expr * string * ety list (* T.Cons *)
  | TCons of string * ety list (* Cons x *)
  | TVar of Ident.ident (* 'var *)
  | TArrow of ety * ety
  | TTuple of ety list
  | TRecord of (string * ety) list
  | TInternal of Types_in.ty

and ety_def =
  | TDAdt of string * ety_paras * evariant list
  | TDRecord of string * ety_paras * (string * ety) list

and ety_paras = Ident.ident list

and emod_ty =
  | MTName of string
  | MTField of mod_expr * string
  | MTSig of ety_comp list
  | MTFunctor of string * emod_ty * emod_ty

and evariant = string * ety option
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "tree_iter" },
    visitors { variety = "map"; name = "tree_map" }]

class virtual ['self] map =
  object (self : 'self)
    inherit ['self] constant_map

    inherit! ['self] tree_map

    inherit! ['self] Types_in.ty_map

    method visit_ident env id =
      Ident.mk_ident
        (self#visit_int env (Ident.index_of_ident id))
        (self#visit_string env (Ident.name_of_ident id))
  end

class virtual ['self] iter =
  object (self : 'self)
    inherit ['self] constant_iter

    inherit! ['self] tree_iter

    inherit! ['self] Types_in.ty_iter

    method visit_ident env id =
      self#visit_int env (Ident.index_of_ident id);
      self#visit_string env (Ident.name_of_ident id)
  end
