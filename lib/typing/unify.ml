open Types
module SMap = Map.Make (String)

type t = ty SMap.t

let apply u te =
  let mapper =
    object (_ : 'self)
      inherit ['self] Syntax.Parsetree.map

      method! visit_TVar _ x =
        SMap.find_opt x u |> Option.value ~default:(Tree.TVar x)

      method! visit_type_def _ def = def
    end
  in
  mapper#visit_type_expr () te

let apply_expr u (e : Typedtree.expr) =
  let mapper =
    object (self : 'self)
      inherit ['self] Typedtree.map

      method visit_ty = self#visit_type_expr

      method! visit_TVar _ x =
        SMap.find_opt x u |> Option.value ~default:(Tree.TVar x)
    end
  in
  mapper#visit_expr () e

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

exception UnificationError of (ty * ty)

let make_subst x t : t = SMap.(add x t empty)

let make_subst_lst xs ts : t =
  List.combine xs ts |> List.to_seq |> SMap.of_seq

exception OccurError of (string * ty)

exception IllFormType

let occur x te =
  let occured = ref false in
  let visitor =
    object (_ : 'self)
      inherit ['self] Tree.iter

      method! visit_TVar _ x' = if x = x' then occured := true
    end
  in
  visitor#visit_type_expr () te;
  if !occured then raise (OccurError (x, te))

let rec unify (t0 : ty) (t1 : ty) : t =
  if t0 = t1 then identity
  else
    match (t0, t1) with
    | TVar x, t1 ->
        occur x t1;
        make_subst x t1
    | _, TVar _y -> unify t1 t0
    | TCons (tc0, tes0), TCons (tc1, tes1) when tc0 = tc1 ->
        unify_lst tes0 tes1
    (* by default raise an exception *)
    | _ -> raise (UnificationError (t0, t1))

and unify_lst t0 t1 =
  match (t0, t1) with
  | [], [] -> identity
  | t0 :: tes0, t1 :: tes1 ->
      let u0 = unify t0 t1 in
      unify_lst (apply_lst u0 tes0) (apply_lst u0 tes1)
  | _ -> raise IllFormType
