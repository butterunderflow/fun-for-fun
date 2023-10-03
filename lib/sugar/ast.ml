open Sexplib.Conv

type constant = Syntax.Ast.constant
[@@deriving sexp]

type pattern =
  | PVal of constant
  | PCons of Ident.t * pattern list
  | PVar of Ident.t
[@@deriving sexp]

type type_expr = TCons of Ident.t * type_expr list | TVar of Ident.t
[@@deriving sexp]

type para = PAnn of Ident.t * type_expr | PBare of Ident.t [@@deriving sexp]
type paras = para list [@@deriving sexp]

type lambda = para * expr
[@@deriving sexp]

and expr =
  | EConst of constant
  | EVar of Ident.t
  | ELet of Ident.t * expr * expr
  | ELetrec of (Ident.t * lambda) list * expr
  | ELam of lambda
  | EIf of expr * expr * expr
  | ECase of expr * (pattern * expr) list
  | EApp of expr * expr
  | EAnn of expr * type_expr
  | ETuple of expr list
  | EFetchTuple of expr * int
[@@deriving sexp]

type variant = Ident.t * type_expr list [@@deriving sexp]

type type_paras = Ident.t list [@@deriving sexp]

type top_level =
  | Top_let of Ident.t * expr
  | Top_letrec of (Ident.t * lambda) list
  | Top_type of Ident.t * type_paras * variant list
[@@deriving sexp]

type program = top_level list [@@deriving sexp]
