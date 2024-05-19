module I = Types_in

type t = {
  values : (string * I.bind_ty) list;
  types : I.ty_def list;
  modules : (string * I.mod_ty) list; (* module bindings *)
  module_dict : (int * I.mod_ty) list;
  curr : int;
}

let add_value x ty env = { env with values = (x, ty) :: env.values }

let add_module m ty env = { env with modules = (m, ty) :: env.modules }

let add_type_def def env = { env with types = def :: env.types }

let get_value_type x env = List.assoc x env.values

let get_module_sig m env = List.assoc m env.modules

let get_module_by_id i env = List.assoc i env.module_dict

let get_type_def tn env =
  List.find
    (function
      | I.TDOpaqueI x
      | TDAdtI (x, _, _)
      | TDRecordI (x, _, _) ->
          x = tn)
    env.types

let init =
  { values = []; types = []; modules = []; module_dict = []; curr = 0 }

let mk_tid tn env = (env.curr, tn)

(***********************)
let dbg env =
  let values =
    env.values
    |> List.map (fun (x, (tvs, te)) ->
           ( x,
             List.map Ident.to_string tvs,
             Sexplib.Sexp.to_string_hum ?indent:(Some 2) (I.sexp_of_ty te) ))
    |> List.map (fun (x, tvs, te) ->
           Printf.sprintf "%s |-> forall %s . %s" x (String.concat ";" tvs)
             te)
    |> String.concat "; \n  "
  in
  let ty_defs =
    env.types
    |> List.map (fun def ->
           match def with
           | I.TDOpaqueI name -> (name, def)
           | I.TDAdtI (name, _, _) -> (name, def)
           | I.TDRecordI (name, _, _) -> (name, def))
    |> List.map (fun (name, def) ->
           ( name,
             I.sexp_of_ty_def def
             |> Sexplib.Sexp.to_string_hum ?indent:(Some 2) ))
    |> List.map (fun (name, def) -> Printf.sprintf "%s |-> %s" name def)
    |> String.concat "; \n  "
  in
  Printf.sprintf
    {|
------------------Envirment Debug Info Begin------------------------
Value Bindings: 
  %s
Type Definitions:
  %s
Current Module Index:
  %d 
------------------Envirment Debug Info End--------------------------
|}
    values ty_defs env.curr
