open Sexplib.Conv

type program = top_level list

and top_level =
  | TopLet of pattern * expr
  | TopLetRec of (string * lambda) list
  | TopTypeDef of type_def
  | TopMod of string * mod_expr
  | TopModRec of (string * functor_expr) list

and pattern =
  | PVal of constant
  | PCons of string * pattern list
  | PVar of string

and type_expr =
  | TField of path * string * type_expr list
  | TCons of string * type_expr list
  | TVar of string
  | TArrow of type_expr * type_expr
  | TTuple of type_expr list
  | TRecord of (string * type_expr) list

and para =
  | PAnn of string * type_expr
  | PBare of string

and paras = para list

and constant =
  | CBool of bool
  | CInt of int
  | CString of string

and expr =
  | EConst of constant
  | EVar of string
  | ECons of string
  | ELet of pattern * expr * expr
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

and variant = string * type_expr option

and type_paras = string list

and type_def =
  | TDAdt of string * type_paras * variant list
  | TDAlias of string * type_expr
  | TDRecord of string * type_paras * (string * type_expr) list

and mod_body = top_level list

and path =
  | PName of string
  | PMem of path * string
  | PApply of path * path

and mod_expr =
  | MEName of string
  | MEStruct of mod_body
  | MEFunctor of functor_expr
  | MEField of path * string
  | MEApply of mod_expr * mod_expr list
  | MERestrict of mod_expr * mod_type

and functor_para = string * mod_type

and functor_expr = functor_para * mod_expr

and mod_type =
  | MTName of string
  | MTField of path * string
  | MTSig of type_comp list
  | MTFunctor of string * mod_type * mod_type

and adt_def = string * type_paras * variant list

and type_comp =
  | TValueSpec of string * type_expr
  | TAbstTySpec of string
  | TManiTySpec of type_def
[@@deriving
  sexp, visitors { variety = "iter" }, visitors { variety = "map" }]
