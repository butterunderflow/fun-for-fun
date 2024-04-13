open Sexplib.Conv

[@@@warning "-17"]

type constant =
  | CBool of bool
  | CInt of int
  | CString of string
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "constant_iter" },
    visitors { variety = "map"; name = "constant_map" }]

type path =
  | PName of string
  | PMem of path * string
  | PApply of path * path
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "path_iter" },
    visitors { variety = "map"; name = "path_map" }]

type pattern =
  | PVal of constant
  | PCons of string * pattern option (* Cons (1, 2) *)
  | PFieldCons of path * string * pattern option (* T.M(M2).C (1, 2) *)
  | PVar of string
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "pattern_iter" },
    visitors { variety = "map"; name = "pattern_map" }]

type type_comp =
  | TValueSpec of string * type_expr
  | TAbstTySpec of string
  | TManiTySpec of type_def

and type_expr =
  | TField of path * string * type_expr list (* T.Cons *)
  | TCons of string * type_expr list (* Cons x *)
  | TVar of Ident.ident (* 'var *)
  | TArrow of type_expr * type_expr
  | TTuple of type_expr list
  | TRecord of (string * type_expr) list

and type_def =
  | TDAdt of string * type_paras * variant list
  | TDRecord of string * type_paras * (string * type_expr) list

and type_paras = string list

and variant = string * type_expr option
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "type_iter" },
    visitors { variety = "map"; name = "type_map" }]

type program = top_level list

and top_level =
  | TopLet of string * expr
  | TopLetRec of (string * lambda) list
  | TopTypeDef of type_def
  | TopMod of string * mod_expr

and para =
  | PAnn of string * type_expr
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
  | EAnn of expr * type_expr
  | ETuple of expr list
  | EField of path * string
  | EFieldCons of path * string

and lambda = para * expr

and mod_body = top_level list

and mod_expr =
  | MERoot
  | MEName of string (* M *)
  | MEStruct of mod_body (* struct ... end *)
  | MEFunctor of functor_expr (* functor (M: MT) -> ... *)
  | MEField of path * string (* M1.M2 *)
  | MEApply of mod_expr * mod_expr (* M1(...) *)
  | MERestrict of mod_expr * mod_type (* M: M_ty *)

and functor_para = string * mod_type

and functor_expr = functor_para * mod_expr

and mod_type =
  | MTName of string
  | MTField of path * string
  | MTSig of type_comp list
  | MTFunctor of string * mod_type * mod_type

and adt_def = string * type_paras * variant list
[@@deriving
  sexp,
    visitors { variety = "iter"; name = "tree_iter" },
    visitors { variety = "map"; name = "tree_map" }]

class virtual ['self] map =
  object (self : 'self)
    inherit ['self] constant_map

    inherit! ['self] pattern_map

    inherit! ['self] path_map

    inherit! ['self] tree_map

    inherit! ['self] type_map

    method visit_ident env id =
      Ident.mk_ident
        (self#visit_int env (Ident.index_of_ident id))
        (self#visit_string env (Ident.name_of_ident id))
  end

class virtual ['self] iter =
  object (self : 'self)
    inherit ['self] constant_iter

    inherit! ['self] pattern_iter

    inherit! ['self] path_iter

    inherit! ['self] tree_iter

    inherit! ['self] type_iter

    method visit_ident env id =
      self#visit_int env (Ident.index_of_ident id);
      self#visit_string env (Ident.name_of_ident id)
  end
