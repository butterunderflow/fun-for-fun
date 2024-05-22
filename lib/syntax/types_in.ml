(* Internal type used by type system *)

open Sexplib.Conv

[@@@warning "-17"]

type ty_id = int (* module type id *) * string

(* normalized type expression *)
and ty =
  | TConsI of ty_id * ty list (* x list *)
  | TVarI of tv ref (* 'var *)
  | TQVarI of Ident.ident
  | TArrowI of ty * ty
  | TTupleI of ty list
  | TRecordI of (string * ty) list

and tv =
  | Unbound of Ident.ident
  | Link of ty

and bind_ty = Ident.ident list * ty

and type_paras = Ident.ident list

and variant = string * ty option

and ty_def =
  (* todo: add type alias to type definition *)
  | TDOpaqueI of string
  | TDAdtI of string * type_paras * variant list
  | TDRecordI of string * type_paras * (string * ty) list

and mod_ty =
  | MTMod of {
      id : int; (* give every module type an identity *)
      val_defs : (string * bind_ty) list;
      ty_defs : ty_def list;
      mod_defs : (string * mod_ty) list;
    }
  | MTFun of (mod_ty * mod_ty)
[@@deriving
  sexp,
    show,
    visitors { variety = "iter"; name = "ty_iter" },
    visitors { variety = "map"; name = "ty_map" }]

class virtual ['self] map =
  object (self : 'self)
    inherit ['self] ty_map

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
