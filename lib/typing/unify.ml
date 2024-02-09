open Types

type t = ty -> ty

let compose (u0 : t) (u1 : t) : t = fun t -> t |> u0 |> u1

let identity x = x

let ( <$> ) = compose

let apply_lst : t -> ty list -> ty list = List.map

let apply_env u (env : Env.t) : Env.t =
  {
    values = List.map (fun (x, te) -> (x, u te)) env.values;
    (* types and modules have no space for inference *)
    types = env.types;
    modules = env.modules;
  }

let[@warning "-27"] unify (t0 : ty) (t1 : ty) : t = failwith "todo"

let[@warning "-27"] make_subst x t : t = failwith "todo"
let [@warning "-27"] make_subst_lst xs ts: t = failwith "todo"
