(* Internal type used by type system *)

open Sexplib.Conv

[@@@warning "-17"]

type ty_id = int (* module type id *) * string

(* normalized type expression *)
and ty =
  | TCons of ty_id * ty list (* x list *)
  | TVar of tv ref (* 'var *)
  | TQVar of Ident.ident
  | TArrow of ty * ty
  | TTuple of ty list
  | TRecord of (string * ty) list

and tv =
  | Unbound of Ident.ident * int (* level *)
  | Link of ty

and bind_ty = Ident.ident list * ty

and type_paras = Ident.ident list

and variant = string * ty option

and ty_def =
  | TDOpaque of string * type_paras
  | TDAdt of string * type_paras * variant list
  | TDRecord of string * type_paras * (string * ty) list
  | TDAlias of string * ty

and structure = {
  id : int; (* give every module type an identity *)
  val_defs : (string * bind_ty) list;
  constr_defs :
    (string * (bind_ty * int (* constructor id of a adt *))) list;
  ty_defs : ty_def list;
  mod_sigs : (string * mod_ty) list;
  mod_defs : (string * mod_ty) list;
  owned_mods : int list;
}

and mod_ty =
  | MTMod of structure
  | MTFun of (mod_ty * mod_ty)
[@@deriving
  sexp,
    show,
    visitors { variety = "iter"; name = "ty_iter" },
    visitors { variety = "map"; name = "ty_map" }]

class virtual ['self] map =
  object (self : 'self)
    inherit ['self] ty_map

    method! visit_TVar () tv =
      match !tv with
      | Unbound _ -> TVar tv
      | Link te -> self#visit_ty () te

    method visit_ident env id =
      Ident.mk_ident
        (self#visit_int env (Ident.index_of_ident id))
        (self#visit_string env (Ident.name_of_ident id))
  end

class virtual ['self] iter =
  object (self : 'self)
    inherit ['self] ty_iter

    method visit_ident env id =
      self#visit_int env (Ident.index_of_ident id);
      self#visit_string env (Ident.name_of_ident id)
  end

let same ty0 ty1 = ty0 = ty1

let root_id = 0

let mk_root_tid tn = (root_id, tn)

let int_ty = TCons (mk_root_tid "int", [])

let string_ty = TCons (mk_root_tid "string", [])

let bool_ty = TCons (mk_root_tid "bool", [])

let unit_ty = TCons (mk_root_tid "unit", [])
