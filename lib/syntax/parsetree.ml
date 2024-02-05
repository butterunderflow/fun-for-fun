open Sexplib.Conv

type constant = CBool of bool | CInt of int | CString of string
[@@deriving sexp]

type pattern =
  | PVal of constant
  | PCons of string * pattern list
  | PVar of string
[@@deriving sexp]

type type_expr = TCons of string * type_expr list | TVar of string
[@@deriving sexp]

type para = PAnn of string * type_expr | PBare of string [@@deriving sexp]

type paras = para list [@@deriving sexp]

type expr =
  | EConst of constant
  | EVar of string
  | ELet of pattern * expr * expr
  | ELetrec of (string * paras * expr) list * expr
  | ELam of para * expr
  | EIf of expr * expr * expr
  | ECase of expr * (pattern * expr) list
  | EApp of expr * expr
  | EAnn of expr * type_expr
  | ETuple of expr list
  | EFetchTuple of expr * int
[@@deriving sexp]

type variant = string * type_expr list [@@deriving sexp]

type type_paras = string list [@@deriving sexp]

type top_level =
  | Top_let of pattern * expr
  | Top_letrec of (string * paras * expr) list
  | Top_type of string * type_paras * variant list
[@@deriving sexp]

type mod_body = top_level list [@@deriving sexp]

type program = top_level list [@@deriving sexp]

type path =
  | PName of string
  | PMem of path * string
  | PApply of path * path
[@@deriving sexp]
