open Types

type t = {
  values : (string * bind_ty) list;
  types : ty_def list;
  modules : (string * mod_ty) list;
  curr : int
}

let add_value x ty env = { env with values = (x, ty) :: env.values }

let add_module m ty env = { env with modules = (m, ty) :: env.modules }

let add_type_def def env = { env with types = def :: env.types }

let get_value_type x env = List.assoc x env.values

let get_module_sig m env = List.assoc m env.modules

let get_type_def tn env =
  List.find
    (function
      | Syntax.Parsetree.TDAdt (x, _, _)
      | TDRecord (x, _, _) ->
          x = tn)
    env.types

let init = { values = []; types = []; modules = [] ; curr = 0}

let dbg env =
  let values =
    env.values
    |> List.map (fun (x, (tvs, te)) ->
           ( x,
             List.map Ident.to_string tvs,
             Sexplib.Sexp.to_string_hum ?indent:(Some 2) (sexp_of_ty te) ))
    |> List.map (fun (x, tvs, te) ->
           Printf.sprintf "%s |-> forall %s . %s" x (String.concat ";" tvs)
             te)
    |> String.concat "; \n  "
  in
  let ty_defs =
    env.types
    |> List.map (fun def ->
           match def with
           | Tree.TDAdt (name, _, _) -> (name, def)
           | TDRecord (name, _, _) -> (name, def))
    |> List.map (fun (name, def) ->
           ( name,
             sexp_of_ty_def def
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
