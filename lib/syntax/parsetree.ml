open Sexplib.Conv

type constant =
  | CBool of bool
  | CInt of int
  | CString of string
  | CUnit
[@@deriving sexp]

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[@@deriving sexp]

type 'a node = {
  desc : 'a;
  start_loc : position;
  end_loc : position;
  attrs : string list; (* unused now *)
}

and program = top_level list

and top_level =
  | TopLet of string * expr
  | TopLetRec of (string * lambda) list
  | TopTypeDef of ty_def
  | TopMod of string * mod_expr
  | TopModSig of string * mod_ty
  | TopExternal of string * ty * string

and para =
  | PAnn of string * ty
  | PBare of string

and paras = para list

and cmp_op =
  | Eq
  | Neq

and expr = expr_desc node

and expr_desc =
  | EConst of constant
  | EVar of string
  | ECons of string
  | ELet of string * expr * expr
  | ELetrec of (string * lambda) list * expr
  | ELam of lambda
  | EIf of expr * expr * expr
  | ECase of expr * (pattern * expr) list
  | EApp of expr * expr
  | EAnn of expr * ty
  | ETuple of expr list
  | EField of mod_expr * string
  | EFieldCons of mod_expr * string
  | ECmp of cmp_op * expr * expr
  | ESeq of expr * expr

and pattern =
  | PVal of constant
  | PCons of string * pattern option (* Cons (1, 2) *)
  | PFieldCons of mod_expr * string * pattern option (* T.M(M2).C (1, 2) *)
  | PVar of string
  | PTuple of pattern list (* (x, y, z) *)

and lambda = para * expr

and mod_body = top_level list

and mod_expr = mod_expr_desc node

and mod_expr_desc =
  | MEName of string (* M *)
  | MEStruct of mod_body (* struct ... end *)
  | MEFunctor of functor_expr (* functor (M: MT) -> ... *)
  | MEField of mod_expr * string (* M1.M2 *)
  | MEApply of mod_expr * mod_expr (* M1(...) *)
  | MERestrict of mod_expr * mod_ty (* M: M_ty *)

and functor_para = string * mod_ty

and functor_expr = functor_para * mod_expr

and adt_def = string * ty_paras * evariant list

and spec =
  | SpecVal of string * ty
  | SpecAbstTy of string * ty_paras
  | SpecManiTy of ty_def
  | SpecMod of (string * mod_ty)

and ty =
  | TField of mod_expr * string * ty list (* T.Cons *)
  | TCons of string * ty list (* Cons x *)
  | TVar of Ident.ident (* 'var *)
  | TArrow of ty * ty
  | TTuple of ty list
  | TRecord of (string * ty) list

and ty_def =
  | TDAdt of string * ty_paras * evariant list
  | TDRecord of string * ty_paras * (string * ty) list
  | TDAlias of string * ty

and ty_paras = Ident.ident list

and mod_ty =
  | MTName of string
  | MTField of mod_expr * string
  | MTSig of spec list
  | MTFunctor of string * mod_ty * mod_ty

and evariant = string * ty option [@@deriving sexp]

let make_node desc start_loc end_loc =
  { desc; start_loc; end_loc; attrs = [] }

let dbg prog =
  let s = sexp_of_program prog in
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s
