open Sexplib.Conv

type constant =
  | Const_bool of bool
  | Const_int of int
  | Const_string of string
  | Const_unit
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
  | Top_let of string * expr
  | Top_letrec of (string * lambda) list
  | Top_type_def of surface_ty_def
  | Top_mod of string * mod_expr
  | Top_mod_sig of string * surface_mod_ty
  | Top_external of string * surface_ty * string

and para =
  | Para_ann of string * surface_ty
  | Para_bare of string

and paras = para list

and cmp_op =
  | Eq
  | Neq

and expr = expr_desc node

and expr_desc =
  | Exp_const of constant
  | Exp_var of string
  | Exp_constr of string
  | Exp_let of string * expr * expr
  | Exp_letrec of (string * lambda) list * expr
  | Exp_lam of lambda
  | Exp_if of expr * expr * expr
  | Exp_case of expr * (pattern * expr) list
  | Exp_app of expr * expr
  | Exp_ann of expr * surface_ty
  | Exp_tuple of expr list
  | Exp_field of mod_expr * string
  | Exp_field_constr of mod_expr * string
  | Exp_cmp of cmp_op * expr * expr
  | Exp_seq of expr * expr

and pattern =
  | Pat_val of constant
  | Pat_constr of string * pattern option (* Cons (1, 2) *)
  | Pat_field_constr of
      mod_expr * string * pattern option (* T.M(M2).C (1, 2) *)
  | Pat_var of string
  | Pat_tuple of pattern list (* (x, y, z) *)

and lambda = para * expr

and mod_body = top_level list

and mod_expr = mod_expr_desc node

and mod_expr_desc =
  | Mod_name of string (* M *)
  | Mod_struct of mod_body (* struct ... end *)
  | Mod_functor of functor_expr (* functor (M: MT) -> ... *)
  | Mod_field of mod_expr * string (* M1.M2 *)
  | Mod_apply of mod_expr * mod_expr (* M1(...) *)
  | Mod_restrict of mod_expr * surface_mod_ty (* M: M_ty *)

and functor_para = string * surface_mod_ty

and functor_expr = functor_para * mod_expr

and adt_def = string * surface_ty_paras * evariant list

and surface_spec =
  | Spec_value of string * surface_ty
  | Spec_abstr of string * surface_ty_paras
  | Spec_mani_ty of surface_ty_def
  | Spec_mod of (string * surface_mod_ty)

and surface_ty =
  | Ty_field of mod_expr * string * surface_ty list (* T.Cons *)
  | Ty_cons of string * surface_ty list (* Cons x *)
  | Ty_var of Ident.ident (* 'var *)
  | Ty_arrow of surface_ty * surface_ty
  | Ty_tuple of surface_ty list
  | Ty_record of (string * surface_ty) list

and surface_ty_def =
  | Ty_def_adt of string * surface_ty_paras * evariant list
  | Ty_def_record of string * surface_ty_paras * (string * surface_ty) list
  | Ty_def_alias of string * surface_ty

and surface_ty_paras = Ident.ident list

and surface_mod_ty =
  | Mod_ty_name of string
  | Mod_ty_field of mod_expr * string
  | Mod_ty_sig of surface_spec list
  | Mod_ty_functor of string * surface_mod_ty * surface_mod_ty

and evariant = string * surface_ty option [@@deriving sexp]

let make_node desc start_loc end_loc =
  { desc; start_loc; end_loc; attrs = [] }

let dbg prog =
  let s = sexp_of_program prog in
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s
