open Types
module SMap = Map.Make (String)

type t = ty SMap.t

let apply u x =
  let mapper =
    object (_ : 'self)
      inherit ['self] Syntax.Parsetree.map

      method! visit_TVar _ x =
        SMap.find_opt x u |> Option.value ~default:(Tree.TVar x)
    end
  in
  mapper#visit_type_expr () x

let ( <$> ) = apply

let compose (u0 : t) (u1 : t) : t =
  let renewed = SMap.map (fun te -> u1 <$> te) u0 in
  SMap.fold
    (fun x te acc -> SMap.(if mem x acc then acc else add x te acc))
    u1 renewed

let identity = SMap.empty

let ( <.> ) = compose

let apply_lst (u : t) : ty list -> ty list = List.map (apply u)

let apply_env (u : t) (env : Env.t) : Env.t =
  {
    values = List.map (fun (x, (qvs, te)) -> (x, (qvs, u <$> te))) env.values;
    (* types and modules have no space for inference *)
    types = env.types;
    modules = env.modules;
  }

let[@warning "-27"] unify (t0 : ty) (t1 : ty) : t = failwith "todo"

let[@warning "-27"] make_subst x t : t = failwith "todo"

let[@warning "-27"] make_subst_lst xs ts : t = failwith "todo"
