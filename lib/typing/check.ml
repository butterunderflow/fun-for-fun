[@@@warning "-27"]

open Typedtree
module U = Unify
module T = Syntax.Parsetree
module I = Types_in
module IdentSet = Set.Make (Ident)

let ( <$$> ) = U.apply_expr

let ( <$> ) = U.( <$> )

let ( <.> ) = U.( <.> )

let make_tv () =
  let name = "'_t" in
  I.TVarI (Ident.create ~hint:name)

let make_tv_of hint = I.TVarI (Ident.create ~hint)

let inst_with (t : I.bind_ty) tes : I.ty =
  let qvs, te = t in
  U.make_subst_lst qvs tes <$> te

let inst (t : I.bind_ty) : I.ty =
  (* Make sure captured type variables will never duplicated with free type
     variables *)
  let qvs, te = t in
  let new_tvs = List.map (fun id -> I.TVarI (Ident.rename id)) qvs in
  inst_with t new_tvs

let get_all_tvs (e : I.ty) : Ident.ident list =
  let tvs = ref IdentSet.empty in
  let collector =
    object (self : 'self)
      inherit ['self] Types_in.iter

      method! visit_TVarI _env tv = tvs := IdentSet.add tv !tvs
    end
  in
  collector#visit_ty () e;
  IdentSet.fold (fun tv acc -> tv :: acc) !tvs []

let generalize (t : I.ty) env : I.bind_ty =
  let tvs = get_all_tvs t in
  (tvs, t)

let rec normalize (t : T.ety) (env : Env.t) : I.ty =
  let { Env.curr; _ } = env in
  match t with
  | T.TField (_, _, _) -> failwith "todo"
  | T.TCons (c, tes) ->
      TConsI ((curr, c), List.map (fun t -> normalize t env) tes)
  | T.TVar x -> TVarI x
  | T.TArrow (t0, t1) -> TArrowI (normalize t0 env, normalize t1 env)
  | T.TTuple ts -> TTupleI (List.map (fun t -> normalize t env) ts)
  | T.TRecord fields ->
      TRecordI (List.map (fun (x, t) -> (x, normalize t env)) fields)
  | T.TInternal ti -> ti

let normalize_def (t : T.ety_def) env : I.ty_def =
  match t with
  | T.TDAdt (n, tvs, vs) ->
      let vs =
        List.map
          (function
            | c, None -> (c, None)
            | c, Some payload -> (c, Some (normalize payload env)))
          vs
      in
      TDAdtI (n, tvs, vs)
  | T.TDRecord (n, tvs, fields) ->
      TDRecordI (n, tvs, List.map (fun (x, t) -> (x, normalize t env)) fields)

(* typing expression *)
let rec tc_expr (e : T.expr) (env : Env.t) : expr * U.t =
  (* look a binding won't unify anything *)
  match e with
  | T.EConst c -> (tc_const c, U.identity)
  | T.EVar x -> (tc_var x env, U.identity)
  | T.ELet (x, e0, e1) -> tc_let x e0 e1 env
  | T.ELetrec (binds, body) -> tc_letrec binds body env
  | T.ELam (para, body) -> tc_lambda para body env
  | T.EIf (c, e0, e1) -> tc_if_else c e0 e1 env
  | T.ECase (e, bs) -> tc_cases e bs env
  | T.EApp (e0, e1) -> tc_app e0 e1 env
  | T.EAnn (e, te) -> tc_ann e te env
  | T.ETuple es -> tc_tuple es env
  | T.EField (p, x) -> tc_field p x env
  | T.ECons c -> (tc_cons c env, U.identity)
  | T.EFieldCons (p, c) -> (tc_field_cons p c env, U.identity)

and tc_const c =
  match c with
  | T.CBool _ -> EConst (c, I.bool_ty)
  | T.CInt _ -> EConst (c, I.int_ty)
  | T.CString _ -> EConst (c, I.string_ty)

and tc_var x env =
  let t = Env.get_value_type x env |> inst in
  EVar (x, t)

(* pattern will create bindings under context's type *)
and tc_pattern p te env : pattern * (string * I.ty) list * U.t =
  let tc_PCons_aux (cons_ty : I.ty) (p (* payload pattern *) : T.pattern) te0
      =
    match cons_ty with
    | I.TArrowI (pay_ty (* payload type *), te1) ->
        let u0 = U.unify te1 te0 in
        let env = U.apply_env u0 env in
        let pay_ty = U.apply u0 pay_ty in
        let p, vars, u1 = tc_pattern p pay_ty env in
        (p, vars, u0 <.> u1)
    | _ -> failwith "wrong type"
  in
  match (p, te) with
  | T.PVar x, te -> (PVar x, [ (x, te) ], U.identity)
  | T.PCons (c, None), te -> (
      let cons_ty_gen (* type of constructor *) = Env.get_value_type c env in
      let cons_ty = inst cons_ty_gen in
      match cons_ty with
      | I.TConsI (_, []) -> (PCons (c, None), [], U.identity)
      | _ -> failwith "wrong type")
  | T.PFieldCons (p, c, None), te -> (
      let cons_typed (* constructor *) = tc_field_cons p c env in
      let cons_ty = get_ty cons_typed in
      (* todo: fix cons_ty should unify with te, and tc_field_cons's return
         type has problem *)
      match cons_ty with
      | I.TConsI (_, _) -> (PCons (c, None), [], U.identity)
      | _ -> failwith "wrong type")
  | T.PCons (c, Some p0 (* pattern *)), te ->
      let cons_ty_gen (* type of constructor *) = Env.get_value_type c env in
      let p0, binds, u = tc_PCons_aux (inst cons_ty_gen) p0 te in
      (PCons (c, Some p0), binds, u)
  | T.PFieldCons (p (* path *), c, Some p0), te ->
      let cons_typed (* typed constructor *) = tc_field_cons p c env in
      let cons_ty = get_ty cons_typed in
      tc_PCons_aux cons_ty p0 te
  | T.PVal v, te ->
      let v_typed = tc_const v in
      let _ = U.unify (get_ty v_typed) te in
      (PVal v, [], U.identity)
  | T.PTuple pats, te -> (
      let u = U.unify te (I.TTupleI (List.map (fun _ -> make_tv ()) pats)) in
      let te = u <$> te in
      let env = U.apply_env u env in
      match te with
      | I.TTupleI tes ->
          let pats, vars, u, _ =
            List.fold_left2
              (fun (pats_acc, vars_acc, u_acc, env) pat te ->
                let pat, vars, u = tc_pattern pat te env in
                let var_acc =
                  List.map (fun (x, t) -> (x, u <$> t)) vars_acc
                in
                let env = U.apply_env u env in
                (pats_acc @ [ pat ], var_acc @ vars, u_acc <.> u, env))
              ([], [], u, env) pats tes
          in
          (PTuple pats, vars, u)
      | _ -> failwith "wrong")

and tc_let x e0 e1 env =
  let e0_typed0, u0, env = tc_let_binding x e0 env in

  let e1_typed1, u1 = tc_expr e1 env in
  (ELet (x, u1 <$$> e0_typed0, e1_typed1, get_ty e1_typed1), u0 <.> u1)

and tc_let_binding x e0 env : expr * U.t * Env.t =
  let e0_typed0, u0 = tc_expr e0 env in
  let e0_ty0 = get_ty e0_typed0 in
  let env = U.apply_env u0 env in
  let gen (* generalized type *) = generalize e0_ty0 env in
  let env = Env.add_value x gen env in
  (e0_typed0, u0, env)

and tc_letrec binds body env =
  let env, vars, lams_typed0, u0 = tc_letrec_binding binds env in
  let body_typed1, u1 = tc_expr body env in
  let lams_typed1 =
    List.map
      (fun (para, lam, te) -> (para, U.apply_expr u1 lam, U.apply u1 te))
      lams_typed0
  in
  ( ELetrec (List.combine vars lams_typed1, body_typed1, get_ty body_typed1),
    u0 <.> u1 )

and tc_letrec_binding binds env =
  let tvs = List.map (fun x -> ([], make_tv ())) binds in
  let env =
    List.fold_left2
      (fun acc (x, _) tv -> Env.add_value x tv acc)
      env binds tvs
  in
  let vars = List.map fst binds in
  let lams = List.map snd binds in
  let env, lams_typed0, u0 =
    List.fold_left2
      (fun (env, acc, u) x (para, body) ->
        let lam_typed0, u0 = tc_lambda para body env in
        let lam_ty0 = get_ty lam_typed0 in
        let env = U.apply_env u0 env in
        let u1 = U.unify lam_ty0 (inst (Env.get_value_type x env)) in
        let env = U.apply_env u1 env in
        let acc = List.map (U.apply_expr (u0 <.> u1)) acc in
        (env, acc @ [ u1 <$$> lam_typed0 ], u0 <.> u1))
      (env, [], U.identity) vars lams
  in
  let lams_typed0 =
    List.map
      (function
        | ELam (x, body, ty) -> (x, body, ty)
        | _ -> failwith "neverreach")
      lams_typed0
  in
  let env =
    List.fold_left2
      (fun acc (_, _, te) x -> Env.add_value x (generalize te env) acc)
      env lams_typed0 vars
  in
  (env, vars, lams_typed0, u0)

and tc_lambda para body env0 =
  match para with
  | T.PAnn (x, t) ->
      let env = Env.add_value x ([], normalize t env0) env0 in
      let body0, u0 = tc_expr body env in
      let body_ty0 = get_ty body0 in
      (ELam (x, body0, I.TArrowI (u0 <$> normalize t env, body_ty0)), u0)
  | T.PBare x ->
      let tv = make_tv () in
      let env = Env.add_value x ([], tv) env0 in
      let body0, u0 = tc_expr body env in
      let body_ty0 = get_ty body0 in
      (ELam (x, body0, I.TArrowI (u0 <$> tv, body_ty0)), u0)

and tc_if_else c e1 e2 env =
  let c_typed, u0 = tc_expr c env in
  let u1 = U.unify (get_ty c_typed) I.bool_ty in
  let env1 = U.apply_env (u0 <.> u1) env in
  let e1_typed, u2 = tc_expr e1 env1 in
  let env2 = U.apply_env u2 env1 in
  let e2_typed, u3 = tc_expr e2 env2 in
  let u4 = U.unify (get_ty e1_typed) (get_ty e2_typed) in
  ( EIf
      ( u1 <.> u2 <.> u3 <.> u4 <$$> c_typed,
        u3 <.> u4 <$$> e1_typed,
        u4 <$$> e2_typed,
        u4 <$> get_ty e1_typed ),
    u0 <.> u1 <.> u2 <.> u3 <.> u4 )

and tc_app op arg env =
  let op_typed0, u0 = tc_expr op env in
  let op_ty0 = get_ty op_typed0 in
  let arg0 = U.apply_expr_untypd u0 arg in
  let env = U.apply_env u0 env in
  let arg_typed1, u1 = tc_expr arg0 env in
  let arg_ty1 = get_ty arg_typed1 in
  let op_ty1 = U.apply u1 op_ty0 in
  let tv = make_tv_of "ret" in
  let u2 = U.unify op_ty1 (I.TArrowI (arg_ty1, tv)) in
  let ret_ty2 = U.apply u2 tv in
  let op_typed2 = u1 <.> u2 <$$> op_typed0 in
  let arg_typed2 = u2 <$$> arg_typed1 in

  (EApp (op_typed2, arg_typed2, ret_ty2), u0 <.> u1 <.> u2)

and tc_cases e bs env =
  let e_typed0, u0 = tc_expr e env in
  let e_ty0 = get_ty e_typed0 in
  let env = U.apply_env u0 env in
  let res_ty = make_tv_of "res" in
  let env, bs_typed1, u1, e_ty1, res_ty1 =
    List.fold_left
      (fun (env, acc, u, e_ty, expect_res_ty)
           ((p : T.pattern) (* pattern *), res) ->
        (* get binding created by pattern *)
        let p, vars, u1 = tc_pattern p e_ty env in
        let expect_res_ty = u1 <$> expect_res_ty in
        let env = U.apply_env u1 env in
        let res = U.apply_expr_untypd u1 res in
        (* check result expression *)
        let env' =
          List.fold_left
            (fun env (v, ty) ->
              Env.add_value v ([], ty (* no generalize here*)) env)
            env vars
        in
        let res_typed2, u2 = tc_expr res env' in
        let res_ty2 = get_ty res_typed2 in
        let env = U.apply_env u2 env in
        let expect_res_ty = u2 <$> expect_res_ty in
        (* unify checked result type with current result type *)
        let u3 = U.unify res_ty2 expect_res_ty in
        let acc3 =
          List.map (fun (p, res) -> (p, u1 <.> u2 <.> u3 <$$> res)) acc
        in
        let env = U.apply_env u3 env in
        let res_typed3 = u3 <$$> res_typed2 in
        let e_ty3 = U.apply (u1 <.> u2 <.> u3) e_ty in
        let res_ty3 = U.apply u3 res_ty2 in
        ( env,
          acc3 @ [ (p, res_typed3) ],
          u <.> u1 <.> u2 <.> u3,
          e_ty3,
          res_ty3 ))
      (env, [], u0, e_ty0, res_ty)
      bs
  in
  let e_typed1 = u1 <$$> e_typed0 in
  (ECase (e_typed1, bs_typed1, res_ty1), u1)

and tc_tuple es env =
  let env, es_typed, u =
    List.fold_left
      (fun (env, acc, u) e ->
        let e = U.apply_expr_untypd u e in
        let e_typed0, u0 = tc_expr e env in
        (U.apply_env u0 env, acc @ [ e_typed0 ], u <.> u0))
      (env, [], U.identity) es
  in
  let tu_te = I.TTupleI (List.map get_ty es_typed) in
  (ETuple (es_typed, tu_te), u)

and tc_cons c env =
  let t = Env.get_value_type c env |> inst in
  ECons (c, t)

and tc_field_cons p c env = failwith "todo"

and tc_field p x env = failwith "todo"

and tc_ann e te env =
  let e_typed0, u0 = tc_expr e env in
  let env = U.apply_env u0 env in
  let te = u0 <$> normalize te env in
  let u1 = U.unify te (get_ty e_typed0) in
  (u1 <$$> e_typed0, u0 <.> u1)

(* typing top levels *)
let rec tc_toplevel (top : T.top_level) env : top_level * U.t * Env.t =
  match top with
  | T.TopLet (x, e) ->
      let e_typed0, u0, env = tc_let_binding x e env in
      let e_ty0 = get_ty e_typed0 in
      (TopLet (x, e_typed0, e_ty0), u0, env)
  | T.TopLetRec binds ->
      let env, vars, lams, u = tc_letrec_binding binds env in
      let binds = List.combine vars lams in
      (TopLetRec binds, u, env)
  | T.TopTypeDef (TDAdt (name, ty_para_names, bs) as def_ext) ->
      let tid = Env.mk_tid name env in
      let def = normalize_def def_ext env in
      let[@warning "-8"] (I.TDAdtI (_, _, bs)) = def in
      let env = Env.add_type_def def env in
      let constructors = analyze_constructors tid ty_para_names bs in
      let env = { env with values = constructors @ env.values } in
      (TopTypeDef def, U.identity, env)
  | T.TopTypeDef (TDRecord (_, _, _) as def_ext) ->
      let def = normalize_def def_ext env in
      (TopTypeDef def, U.identity, Env.add_type_def def env)
  | T.TopMod (_, _) -> failwith "todo"

and analyze_constructors (tid : I.ty_id) para_names (bs : I.variant list) :
    (string * I.bind_ty) list =
  List.map
    (fun (branch : I.variant) ->
      match branch with
      | c, None ->
          let renamed_paras =
            List.map (fun id -> Ident.rename id) para_names
          in
          ( c,
            ( renamed_paras,
              I.TConsI (tid, List.map (fun id -> I.TVarI id) renamed_paras)
            ) )
      | c, Some payload ->
          let renamed_paras =
            List.map (fun id -> Ident.rename id) para_names
          in
          let renamed_para_tys =
            List.map (fun id -> I.TVarI id) renamed_paras
          in
          let payload_gen = (para_names, payload) in
          ( c,
            ( renamed_paras,
              I.TArrowI
                ( inst_with payload_gen renamed_para_tys,
                  TConsI (tid, List.map (fun id -> I.TVarI id) renamed_paras)
                ) ) ))
    bs

(* typing program (content of module) *)
and tc_program (prog : T.program) env : program * U.t * Env.t =
  match prog with
  | [] -> ([], U.identity, env)
  | top :: rest ->
      let top_typed0, u0, env = tc_toplevel top env in
      let rest_typed1, u1, env = tc_program rest env in
      (U.apply_top u1 top_typed0 :: rest_typed1, u0 <.> u1, env)
