(* Internal type used by type system *)

open Sexplib.Conv

[@@@warning "-17"]

type ty_id = int (* module type id *) * string

(* normalized type expression *)
and ty =
  | Ty_cons of ty_id * ty list (* x list *)
  | Ty_var of tv ref (* 'var *)
  | Ty_qvar of Ident.ident
  | Ty_arrow of ty * ty
  | Ty_tuple of ty list
  | Ty_record of (string * ty) list

and tv =
  | Unbound of Ident.ident * int (* level *)
  | Link of ty

and bind_ty = Ident.ident list * ty

and type_paras = Ident.ident list

and variant = string * ty option

and ty_def =
  | Ty_def_opaque of string * type_paras
  | Ty_def_adt of string * type_paras * variant list
  | Ty_def_record of string * type_paras * (string * ty) list
  | Ty_def_alias of string * ty

and mod_ty =
  | Mod_ty_struct of {
      id : int; (* give every module type an identity *)
      val_defs : (string * bind_ty) list;
      constr_defs :
        (string * (bind_ty * int (* constructor id of a adt *))) list;
      ty_defs : ty_def list;
      mod_sigs : (string * mod_ty) list;
      mod_defs : (string * mod_ty) list;
      owned_mods : int list;
    }
  | Mod_ty_functor of (mod_ty * mod_ty)
[@@deriving
  sexp,
    show,
    visitors { variety = "iter"; name = "ty_iter" },
    visitors { variety = "map"; name = "ty_map" }]

class virtual ['self] map =
  object (self : 'self)
    inherit ['self] ty_map

    method! visit_Ty_var () tv =
      match !tv with
      | Unbound _ -> Ty_var tv
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

let int_ty = Ty_cons (mk_root_tid "int", [])

let string_ty = Ty_cons (mk_root_tid "string", [])

let bool_ty = Ty_cons (mk_root_tid "bool", [])

let unit_ty = Ty_cons (mk_root_tid "unit", [])

let same_def td0 td1 = td0 = td1

let get_def_name (td : ty_def) =
  match td with
  | Ty_def_opaque (name, _)
  | Ty_def_adt (name, _, _)
  | Ty_def_record (name, _, _)
  | Ty_def_alias (name, _) ->
      name

let get_def name ty_defs =
  List.find
    (fun td ->
      match td with
      | Ty_def_opaque (name', _)
      | Ty_def_adt (name', _, _)
      | Ty_def_record (name', _, _)
      | Ty_def_alias (name', _)
        when name' = name ->
          true
      | _ -> false)
    ty_defs
