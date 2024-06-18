(* Internal type used by type system *)

include Syntax.Types_in

let same ty0 ty1 = ty0 = ty1

let root_id = 0

let mk_root_tid tn = (root_id, tn)

let int_ty = TConsI (mk_root_tid "int", [])

let string_ty = TConsI (mk_root_tid "string", [])

let bool_ty = TConsI (mk_root_tid "bool", [])

let same_def td0 td1 = td0 = td1

let get_def_name (td : ty_def) =
  match td with
  | TDOpaqueI (name, _)
  | TDAdtI (name, _, _)
  | TDRecordI (name, _, _) ->
      name
