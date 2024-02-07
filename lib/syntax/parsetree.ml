open Sexplib.Conv

type constant = CBool of bool | CInt of int | CString of string
[@@deriving sexp]

type pattern =
  | PVal of constant
  | PCons of string * pattern list
  | PVar of string
[@@deriving sexp]

type type_expr =
  | TCons of string * type_expr list
  | TVar of string
  | TArrow of type_expr * type_expr
  | TTuple of type_expr list
[@@deriving sexp]

type para = PAnn of string * type_expr | PBare of string [@@deriving sexp]

type paras = para list [@@deriving sexp]

type expr =
  | EConst of constant
  | EVar of string
  | ELet of pattern * expr * expr
  | ELetrec of (string * lambda) list * expr
  | ELam of lambda
  | EIf of expr * expr * expr
  | ECase of expr * (pattern * expr) list
  | EApp of expr * expr
  | EAnn of expr * type_expr
  | ETuple of expr list
  | EFetchTuple of expr * int
[@@deriving sexp]

and lambda = para * expr

type variant = string * type_expr option [@@deriving sexp]

type type_paras = string list [@@deriving sexp]

type top_level =
  | TopLet of pattern * expr
  | TopLetRec of (string * lambda) list
  | TopTypeDef of type_def
  | TopMod of string * mod_expr
  | TopModRec of (string * functor_expr) list
[@@deriving sexp]

and type_def =
  | TDAdt of string * type_paras * variant list
  | TDAlias of string * type_expr
[@@deriving sexp]

and mod_body = top_level list [@@deriving sexp]

and path = PName of string | PMem of path * string | PApply of path * path
[@@deriving sexp]

and mod_expr =
  | MEName of string
  | MEStruct of mod_body
  | MEFunctor of functor_expr
  | MEField of path * string
  | MEApply of mod_expr * mod_expr list
  | MERestrict of mod_expr * mod_type
[@@deriving sexp]

and functor_para = string * mod_type [@@deriving sexp]

and functor_expr = functor_para * mod_expr [@@deriving sexp]

and mod_type =
  | MTName of string
  | MTField of path * string
  | MTSig of type_comp list
  | MTFunctor of string * mod_type * mod_type
[@@deriving sexp]

and adt_def = string * type_paras * variant list [@@deriving sexp]

and type_comp =
  | TValueSpec of string * type_expr
  | TAbstTySpec of string
  | TManiTySpec of type_def

type program = top_level list [@@deriving sexp]
