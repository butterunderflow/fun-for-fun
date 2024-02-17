open Types

type t = {
  values : (string * bind_ty) list;
  types : ty_def list;
  modules : (string * mod_ty) list;
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
      | TDAlias (x, _)
      | TDRecord (x, _, _) ->
          x = tn)
    env.types

let empty = { values = []; types = []; modules = [] }
