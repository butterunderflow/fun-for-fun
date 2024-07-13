open Sexplib.Conv

type constant =
  | CBool of bool
  | CInt of int
  | CString of string
  | CUnit
[@@deriving sexp]

type program = top_level list

and top_level =
  | TopLet of string * expr
  | TopLetRec of (string * lambda) list
  | TopTypeDef of ety_def
  | TopMod of string * mod_expr
  | TopModSig of string * emod_ty
  | TopExternal of string * ety * string

and para =
  | PAnn of string * ety
  | PBare of string

and paras = para list

and cmp_op =
  | Eq
  | Neq

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
  | ECmp of cmp_op * expr * expr

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

and ety_def =
  | TDAdt of string * ety_paras * evariant list
  | TDRecord of string * ety_paras * (string * ety) list
  | TDAlias of string * ety

and ety_paras = Ident.ident list

and emod_ty =
  | MTName of string
  | MTField of mod_expr * string
  | MTSig of ety_comp list
  | MTFunctor of string * emod_ty * emod_ty

and evariant = string * ety option [@@deriving sexp]

let dbg prog =
  let s = sexp_of_program prog in
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s
