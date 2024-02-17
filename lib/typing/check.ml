[@@@warning "-27"]

open Types
open Typedtree
module U = Unify
module T = Tree
module StrSet = Set.Make (String)

let ( <$$> ) = U.apply_expr

let ( <$> ) = U.( <$> )

let ( <.> ) = U.( <.> )

let tv_index = ref 0

let make_tv () =
  tv_index := !tv_index + 1;
  let name = Printf.sprintf "'_t_%d" !tv_index in
  T.TVar (Printf.sprintf "'_%s__%d" name !tv_index)

let make_tv_of hint =
  tv_index := !tv_index + 1;
  let name = Printf.sprintf "'_%s_%d" hint !tv_index in
  T.TVar (Printf.sprintf "'_%s__%d" name !tv_index)

let dealias (t : ty) env : ty = raise Not_found

let normalize t env = dealias t env

let eq (t1 : ty) (t2 : ty) env =
  if t1 == t2 then true else normalize t1 env = normalize t2 env

let inst (t : bind_ty) : ty =
  (* Make sure captured type variables will never duplicated with free type
     variables *)
  let qvs, te = t in
  let new_tvs = List.map make_tv_of qvs in
  U.make_subst_lst qvs new_tvs <$> te

let get_all_tvs (e : ty) : string list =
  let tvs = ref StrSet.empty in
  let collector =
    object (self : 'self)
      inherit ['self] Syntax.Parsetree.path_iter

      inherit! ['self] Syntax.Parsetree.type_iter

      method! visit_TVar _env tv = tvs := StrSet.add tv !tvs
    end
  in
  collector#visit_type_expr () e;
  StrSet.fold (fun tv acc -> tv :: acc) !tvs []

let generalize (t : ty) env : bind_ty =
  let tvs = get_all_tvs t in
  (tvs, t)

let rec type_check (e : T.expr) (env : Env.t) : expr * U.t =
  match e with
  | T.EConst c -> tc_const c
  | T.EVar x -> tc_var x env
  | T.ELet (x, e0, e1) -> tc_let x e0 e1 env
  | T.ELetrec (binds, body) -> tc_letrec binds body env
  | T.ELam (para, body) -> tc_lambda para body env
  | T.EIf (c, e0, e1) -> tc_if_else c e0 e1 env
  | T.ECase (e, bs) -> tc_cases e bs env
  | T.EApp (e0, e1) -> tc_app e0 e1 env
  | T.EAnn (e, te) -> tc_ann e te env
  | T.ETuple es -> tc_tuple es env
  | T.EField (p, x) -> tc_field p x env
  | T.ECons c -> tc_cons c env
  | T.EFieldCons (p, c) -> tc_field_cons p c env

and tc_const c =
  match c with
  | T.CBool _ -> (EConst (c, bool_ty), U.identity)
  | T.CInt _ -> (EConst (c, int_ty), U.identity)
  | T.CString _ -> (EConst (c, string_ty), U.identity)

and tc_var x env =
  let t = Env.get_value_type x env |> inst in
  (EVar (x, t), U.identity)

and tc_pattern p te env : (string * ty) list =
  match te with
  | T.TCons (tn, t_args) -> failwith "todo"
  | T.TField (path, tn, t_args) -> failwith "todo"
  | _ -> failwith "bad destruction type"

and tc_let p e0 e1 env0 =
  let e0_typed, u = type_check e0 env0 in
  let env1 = U.apply_env u env0 in
  let vars = tc_pattern p (get_ty e0_typed) env1 in
  let env2 =
    List.fold_left
      (fun acc (x, te) -> Env.add_value x (generalize te env1) acc)
      env1 vars
  in
  type_check e1 env2

and tc_letrec binds body env = failwith ""

and tc_lambda para body env0 =
  match para with
  | T.PAnn (x, t) ->
      let env1 = Env.add_value x ([], t) env0 in
      type_check body env1
  | T.PBare x ->
      let env1 = Env.add_value x ([], make_tv ()) env0 in
      type_check body env1

and tc_if_else c e1 e2 env =
  let c_typed, u0 = type_check c env in
  let u1 = U.unify (get_ty c_typed) bool_ty in
  let env1 = U.apply_env (u0 <.> u1) env in
  let e1_typed, u2 = type_check e1 env1 in
  let env2 = U.apply_env u2 env1 in
  let e2_typed, u3 = type_check e2 env2 in
  let u4 = U.unify (get_ty e1_typed) (get_ty e2_typed) in
  ( EIf
      ( u1 <.> u2 <.> u3 <.> u4 <$$> c_typed,
        u3 <.> u4 <$$> e1_typed,
        u4 <$$> e2_typed,
        u4 <$> get_ty e1_typed ),
    u0 <.> u1 <.> u2 <.> u3 <.> u4 )

and tc_cases e bs env = failwith ""

and tc_app e0 e1 env = failwith ""

and tc_ann e te env = failwith ""

and tc_tuple es env = failwith ""

and tc_field p x env = failwith ""

and tc_cons c env = failwith "todo"

and tc_field_cons p c env = failwith ""
