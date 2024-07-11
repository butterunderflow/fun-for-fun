module I = Types_in

type scope = {
  values : (string * I.bind_ty) list;
  constrs : (string * (I.bind_ty * int (* constructor id of a adt *))) list;
  types : I.ty_def list;
  modules : (string * I.mod_ty) list; (* module bindings *)
  module_sigs : (string * I.mod_ty) list; (* module bindings *)
  module_dict : (int * I.mod_ty) list;
  curr : int;
  history : int list ref;
}

type t = scope list

let add_value x ty env =
  match env with
  | s :: env' -> { s with values = (x, ty) :: s.values } :: env'
  | [] -> failwith "neverreach"

let add_module m ty env =
  match env with
  | s :: env' -> { s with modules = (m, ty) :: s.modules } :: env'
  | [] -> failwith "neverreach"

let add_module_sig m ty env =
  match env with
  | s :: env' -> { s with module_sigs = (m, ty) :: s.module_sigs } :: env'
  | [] -> failwith "neverreach"

let add_type_def def env =
  match env with
  | s :: env' -> { s with types = def :: s.types } :: env'
  | [] -> failwith "neverreach"

let add_values binds env =
  match env with
  | s :: env' -> { s with values = binds @ s.values } :: env'
  | [] -> failwith "neverreach"

let add_constrs binds env =
  match env with
  | s :: env' -> { s with constrs = binds @ s.constrs } :: env'
  | [] -> failwith "neverreach"

let prune env0 env1 =
  match env0 with
  | s :: env0' when env0' == env1 -> s
  | _ -> failwith "neverreach"

let get_top_history (env : t) =
  match env with
  | [] -> failwith "neverreach"
  | s :: _ -> !(s.history)

let rec get_value_type x (env : t) =
  match env with
  | [] -> failwith (Printf.sprintf "name `%s` not found" x)
  | s :: env' -> (
      match List.assoc_opt x s.values with
      | None -> get_value_type x env'
      | Some te -> te)

let rec get_constr_type x (env : t) : I.bind_ty * int =
  match env with
  | [] -> failwith (Printf.sprintf "constructor `%s` not found" x)
  | s :: env' -> (
      match List.assoc_opt x s.constrs with
      | None -> get_constr_type x env'
      | Some te -> te)

let record_history id (env : t) =
  match env with
  | [] -> failwith "neverreach"
  | s :: _ -> s.history := id :: !(s.history)

let record_all_history ids (env : t) =
  match env with
  | [] -> failwith "neverreach"
  | s :: _ -> s.history := ids @ !(s.history)

let rec get_module_def m (env : t) =
  match env with
  | [] -> failwith (Printf.sprintf "name `%s` not found" m)
  | s :: env' -> (
      match List.assoc_opt m s.modules with
      | None -> get_module_def m env'
      | Some te -> te)

let rec get_module_sig m (env : t) =
  match env with
  | [] -> failwith (Printf.sprintf "name `%s` not found" m)
  | s :: env' -> (
      match List.assoc_opt m s.module_sigs with
      | None -> get_module_sig m env'
      | Some te -> te)

let get_root_def tn =
  ( 0,
    match tn with
    | "int" -> I.TDAdtI ("int", [], [])
    | "string" -> I.TDAdtI ("string", [], [])
    | tn -> failwith (Printf.sprintf "cant get type `%s`" tn) )

let rec get_type_def tn env =
  match env with
  | [] -> get_root_def tn
  | s :: env' -> (
      match
        List.find_opt
          (function
            | I.TDOpaqueI (x, _)
            | TDAdtI (x, _, _)
            | TDRecordI (x, _, _)
            | TDAliasI (x, _) ->
                x = tn)
          s.types
      with
      | Some def -> (s.curr, def)
      | None -> get_type_def tn env')

let get_curr env =
  match env with
  | s :: _ -> s.curr
  | _ -> failwith "neverreach"

let get_module_by_id i env = List.assoc i env.module_dict

let init_scope () =
  {
    values = [];
    constrs = [];
    types = [];
    modules = [];
    module_sigs = [];
    module_dict = [];
    curr = 0;
    history = ref [];
  }

let init () = [ init_scope () ]

let mk_tid tn env =
  match env with
  | s :: _ -> (s.curr, tn)
  | _ -> failwith "nevnerreach"

let captured_scope (s : scope) (tpv : Types_in.tv ref) =
  match tpv with
  | { contents = I.Unbound _ } ->
      List.exists (fun (_, (_, te)) -> Unify.occur tpv te) s.values
  | { contents = I.Link _ } -> failwith "neverreach"

let captured (env : t) tpv = List.exists (fun s -> captured_scope s tpv) env

let size = List.length

(**********Debug Function*************)
let dbg (env : t) =
  let scope_values s =
    s.values
    |> List.map (fun (x, (tvs, te)) ->
           ( x,
             List.map Ident.to_string tvs,
             Sexplib.Sexp.to_string_hum ?indent:(Some 2) (I.sexp_of_ty te) ))
    |> List.map (fun (x, tvs, te) ->
           Printf.sprintf "%s |-> forall %s . %s" x (String.concat ";" tvs)
             te)
    |> String.concat "; \n  "
  in
  let scope_ty_defs s =
    s.types
    |> List.map (fun def ->
           match def with
           | I.TDOpaqueI (name, _) -> (name, def)
           | I.TDAdtI (name, _, _) -> (name, def)
           | I.TDRecordI (name, _, _) -> (name, def)
           | I.TDAliasI (name, _) -> (name, def))
    |> List.map (fun (name, def) ->
           ( name,
             I.sexp_of_ty_def def
             |> Sexplib.Sexp.to_string_hum ?indent:(Some 2) ))
    |> List.map (fun (name, def) -> Printf.sprintf "%s |-> %s" name def)
    |> String.concat "; \n  "
  in
  let scope_mod_tys s =
    s.module_sigs
    |> List.map (fun (name, def) ->
           Printf.sprintf "%s |-> %s" name
             (I.sexp_of_mod_ty def
             |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)))
    |> String.concat "; \n "
  in
  let scope_mod_defs s =
    s.modules
    |> List.map (fun (name, def) ->
           Printf.sprintf "%s |-> %s" name
             (I.sexp_of_mod_ty def
             |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)))
    |> String.concat "; \n "
  in
  let scope_history s =
    !(s.history)
    |> List.map (fun id -> Printf.sprintf "%d" id)
    |> String.concat "; \n  "
  in
  Printf.sprintf
    {|
------------------Envirment Debug Info Begin------------------------
%s
------------------Envirment Debug Info End--------------------------
     |}
    (List.map
       (fun s ->
         Printf.sprintf
           {|
++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
Value Bindings: 
  %s
Type Definitions:
  %s
Module Definitions:
  %s
Module Types: 
  %s
Module Creation History: 
  %s
Current Module Index:
  %d 
++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
            |}
           (scope_values s) (scope_ty_defs s) (scope_mod_defs s)
           (scope_mod_tys s) (scope_history s) s.curr)
       env
    |> String.concat "\n")
