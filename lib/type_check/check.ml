module I = Sugar.Ast
module IdentSet = Set.Make (Ident)

module rec Env : sig
  type t

  val captured_by : t -> Ident.t -> bool

  val append_type : t -> I.type_def -> t

  val append_var : t -> Ident.t -> TypeScheme.t -> t

  val append_vars : t -> (Ident.t * TypeScheme.t) list -> t

  val type_of_var : t -> Ident.t -> TypeScheme.t

  val apply : Unifier.t -> t -> t
end = struct
  type t = {
    types : I.type_def list;
    var_types : (Ident.t * TypeScheme.t) list;
  }

  let captured_by (env : t) (tv : Ident.t) : bool =
    List.exists
      (fun (_, scheme) -> TypeScheme.capture tv scheme)
      env.var_types

  let append_type env def = { env with types = def :: env.types }

  let append_var env x sch =
    { env with var_types = (x, sch) :: env.var_types }

  let append_vars env binds =
    List.fold_left (fun acc (x, sch) -> append_var acc x sch) env binds

  let type_of_var env x =
    List.assoc x env.var_types

  let apply u env =
    _
end

and TypeScheme : sig
  type t

  val generalize : Env.t -> I.type_expr -> t

  val instantiate : t -> I.type_expr

  val concrete : I.type_expr -> t

  val capture : Ident.t -> t -> bool
end = struct
  type t = Ident.t list * I.type_expr

  module U = Unifier

  let concrete te = ([], te)

  let get_tvs (te : I.type_expr) =
    let rec get_tvs_aux te set =
      match te with
      | I.TCons (_, tes) -> List.fold_right get_tvs_aux tes set
      | I.TVar v -> IdentSet.add v set
    in
    get_tvs_aux te IdentSet.empty |> IdentSet.to_seq |> List.of_seq

  let generalize (env : Env.t) (te : I.type_expr) : t =
    let tvs = get_tvs te in
    let free_tvs = List.filter (Env.captured_by env) tvs in
    let renamed = List.map Ident.rename free_tvs in
    let renamer =
      List.fold_left
        (fun acc (x, r) -> U.append x (I.TVar r) acc)
        U.identity
        (List.combine free_tvs renamed)
    in
    (renamed, U.apply renamer te)

  let apply (u : U.t) ((qvs, te) : t) : t =
    (qvs, U.apply (List.fold_right U.remove qvs u) te)

  let instantiate ((qvs, te) : t) : I.type_expr =
    let u =
      List.fold_left
        (fun acc x -> U.append x (I.TVar (Ident.rename x)) acc)
        U.identity qvs
    in
    U.apply u te

  let capture x (qvs, te) =
    if List.mem x qvs then false else Util.capture x te
end

and Unifier : sig
  type t

  val identity : t

  val compose : t -> t -> t

  val apply : t -> I.type_expr -> I.type_expr

  val append : Ident.t -> I.type_expr -> t -> t

  val remove : Ident.t -> t -> t
end = struct
  type type_var = Ident.t

  module IdentMap = Map.Make (Ident)

  type t = I.type_expr IdentMap.t

  let identity = IdentMap.empty

  let rec compose (u0 : t) (u1 : t) : t =
    let new_u = IdentMap.map (apply u1) u0 in
    IdentMap.fold
      (fun x te acc ->
        match IdentMap.find_opt x new_u with
        | Some _ -> acc
        | None -> IdentMap.add x te acc)
      u1 new_u

  and apply (u : t) (te : I.type_expr) : I.type_expr =
    match te with
    | I.TCons (constr, tes) -> I.TCons (constr, List.map (apply u) tes)
    | I.TVar x as v -> (
        match IdentMap.find_opt x u with Some te' -> te' | None -> v)

  and append (x : type_var) (te : I.type_expr) (u : t) : t =
    IdentMap.add x te u

  let remove (x : type_var) (u : t) = IdentMap.remove x u
end

and Util : sig
  val capture : Ident.t -> I.type_expr -> bool
end = struct
  let rec capture x (te : I.type_expr) =
    match te with
    | I.TCons (_, tes) -> List.exists (capture x) tes
    | I.TVar x' -> x = x'
end

module Builtins = struct
  let bool_type = I.TCons (Ident.from "bool", [])

  let int_type = I.TCons (Ident.from "int", [])

  let string_type = I.TCons (Ident.from "string", [])
end

let type_of_const (c: I.constant) =
  match c with
  | CBool _ -> Builtins.bool_type
  | CInt _ -> Builtins.int_type
  | CString _ -> Builtins.string_type

let rec tc_expr (env : Env.t) (exp : I.expr) : I.type_expr =
  match exp with
  | I.EConst c -> type_of_const c
  | I.EVar x -> TypeScheme.instantiate (Env.type_of_var env x)
  | I.ELet (x, e0, e1) -> _
  | I.ELetrec (binds, e) -> _
  | I.ELam (para, e) -> _
  | I.EIf (cond, e0, e1) -> _
  | I.ECase (_, _) -> _
  | I.EApp (_, _) -> _
  | I.EAnn (_, _) -> _
  | I.ETuple _ -> _
  | I.EFetchTuple (_, _) -> _

let tc_top_let (env : Env.t) (x : Ident.t) (e : I.expr) :
    Ident.t * TypeScheme.t =
  let ty = tc_expr e env in
  let ty_scheme = TypeScheme.generalize env ty in
  (x, ty_scheme)

let tc_top_letrec (env : Env.t) (binds : (Ident.t * I.lambda) list) :
    (Ident.t * TypeScheme.t) list =
  _

let rec tc_top_levels (env : Env.t) (tops : I.top_level list) : unit =
  match tops with
  | [] -> ()
  | Top_let (n, e) :: rest ->
      let x, ty = tc_top_let env n e in
      tc_top_levels (Env.append_var env x ty) rest
  | Top_letrec binds :: rest ->
      let ty_binds = tc_top_letrec env binds in
      tc_top_levels (Env.append_vars env ty_binds) rest
  | Top_type ty :: rest -> tc_top_levels (Env.append_type env ty) rest

let check (program : I.program) : unit = _
