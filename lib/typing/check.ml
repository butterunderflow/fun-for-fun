[@@@warning "-27"]

open Types
module T = Tree
module U = Unify
open Typedtree

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
  let qvs, te = t in
  let new_tvs = List.map make_tv_of qvs in
  te |> U.make_subst_lst qvs new_tvs

let generalize (t : ty) : bind_ty = failwith "todo"

let rec type_check (e : T.expr) (env : Env.t) : expr * U.t =
  match e with
  | T.EConst c -> tc_const c
  | T.EVar x -> tc_var x env
  | T.ELet (x, e0, e1) -> tc_let x e0 e1
  | T.ELetrec (binds, body) -> tc_letrec binds body
  | T.ELam (para, body) -> tc_lambda para body
  | T.EIf (c, e0, e1) -> tc_if_else c e0 e1
  | T.ECase (e, bs) -> tc_cases e bs
  | T.EApp (e0, e1) -> tc_app e0 e1
  | T.EAnn (e, te) -> tc_ann e te
  | T.ETuple es -> tc_tuple es
  | T.EField (p, x) -> tc_field p x

and tc_const c =
  match c with
  | T.CBool _ -> (EConst (c, bool_ty), U.identity)
  | T.CInt _ -> (EConst (c, int_ty), U.identity)
  | T.CString _ -> (EConst (c, string_ty), U.identity)

and tc_var x env =
  let t = Env.get_value_type x env |> inst in
  (EVar (x, t), U.identity)

and tc_let x e0 e1 = failwith ""

and tc_letrec binds body = failwith ""

and tc_lambda para body = failwith ""

and tc_if_else c e1 e2 = failwith ""

and tc_cases e bs = failwith ""

and tc_app e0 e1 = failwith ""

and tc_ann e te = failwith ""

and tc_tuple es = failwith ""

and tc_field p x = failwith ""
