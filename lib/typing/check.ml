[@@@warning "-27"]

open Types
open Typedtree
module U = Unify
module T = Tree
module StrSet = Set.Make (String)

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
      inherit ['self] Syntax.Parsetree.iter

      method! visit_TVar _env tv = tvs := StrSet.add tv !tvs
    end
  in
  collector#visit_type_expr () e;
  StrSet.fold (fun tv acc -> tv :: acc) !tvs []

let generalize (t : ty) : bind_ty =
  let tvs = get_all_tvs t in
  (tvs, t)

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
  | T.ECons _ -> failwith ""
  | T.EFieldCons _ -> failwith ""

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
