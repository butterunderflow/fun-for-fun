open Typedtree
module U = Unify
module T = Syntax.Parsetree
module I = Types_in
module P = Poly
module R = Read_type
module IdMap = Map.Make (Ident)
module IntMap = Map.Make (Int)

let tv_pool = ref IdMap.empty

let reset_pool () = tv_pool := IdMap.empty

let pool_make_tv n =
  let tv =
    match IdMap.find_opt n !tv_pool with
    | Some tpv -> tpv
    | None -> P.make_tv ()
  in
  tv_pool := IdMap.add n tv !tv_pool;
  tv

(* record top history of env0 to env1 *)
let absorb_history (env0 : Env.t) (env1 : Env.t) =
  let s = Env.prune env0 env1 in
  Env.record_all_history !(s.history) env1;
  s

type norm_ctx =
  | Type
  | Let

let check_subtype = Subtype.check_subtype

(* typing expression *)
let rec check_expr (e : T.expr) (env : Env.t) : expr =
  try
    match e.desc with
    | T.EConst c -> check_const c
    | T.EVar x -> check_var x env
    | T.ELet (x, e0, e1) -> check_let x e0 e1 env
    | T.ELetrec (binds, body) -> check_letrec binds body env
    | T.ELam (para, body) -> check_lambda para body env
    | T.EIf (c, e0, e1) -> check_if_else c e0 e1 env
    | T.ECase (e, bs) -> check_cases e bs env
    | T.EApp (e0, e1) -> check_app e0 e1 env
    | T.EAnn (e, te) -> check_ann e te env
    | T.ETuple es -> check_tuple es env
    | T.EField (p, x) -> check_field p x env
    | T.ECons c -> check_cons c env
    | T.EFieldCons (p, c) -> check_field_cons p c env
    | T.ECmp (op, e0, e1) -> check_cmp op e0 e1 env
    | T.ESeq (e0, e1) -> check_seq e0 e1 env
    | T.EAssert e -> check_assert e env
  with
  | err -> Report.wrap_and_reraise err e.start_loc e.end_loc env

and check_const c =
  match c with
  | T.CBool _ -> EConst (c, I.bool_ty)
  | T.CInt _ -> EConst (c, I.int_ty)
  | T.CString _ -> EConst (c, I.string_ty)
  | T.CUnit -> EConst (c, I.unit_ty)

and check_var x env =
  (* lookup a binding won't unify anything *)
  let bind = Env.lookup_var_type x env in
  let t = P.inst bind in
  EVar (x, t)

and check_assert e env =
  let e_typed = check_expr e env in
  U.unify I.bool_ty (get_ty e_typed);
  EAssert (e_typed, I.unit_ty)

(* pattern will create bindings under context's type *)
and check_pattern p te env : pattern * (string * I.ty) list =
  let check_PCons_aux (cons_ty : I.ty) (p (* payload pattern *) : T.pattern)
      te0 =
    match cons_ty with
    | I.TArrow (pay_ty (* payload type *), te1) ->
        U.unify te1 te0;
        let p, vars = check_pattern p pay_ty env in
        (p, vars)
    | _ -> failwith "payload constructor is not arrow type"
  in
  match (p, te) with
  | T.PVar x, te -> (PVar (x, te), [ (x, te) ])
  | T.PCons (c, None), te -> (
      let cons_ty_gen (* type of constructor *), id =
        Env.lookup_constr_type c env
      in
      let cons_ty = P.inst cons_ty_gen in
      U.unify cons_ty te;
      match cons_ty with
      | I.TCons _ -> (PCons (c, id, None), [])
      | _ ->
          failwith
            (Printf.sprintf "wrong no-payload constructor pattern %s" c))
  | T.PCons (c, Some p0 (* pattern *)), te ->
      let cons_ty_gen (* type of constructor *), id =
        Env.lookup_constr_type c env
      in
      let p0, binds = check_PCons_aux (P.inst cons_ty_gen) p0 te in
      (PCons (c, id, Some p0), binds)
  | T.PFieldCons (me, c, None), te -> (
      let cons_typed (* constructor *) = check_field_cons me c env in
      let[@warning "-8"] (EFieldCons (_, _, id, _)) = cons_typed in
      let cons_ty = get_ty cons_typed in
      U.unify cons_ty te;
      (* unify cons_ty with te *)
      match cons_ty with
      | I.TCons (_, _) -> (PCons (c, id, None), [ (* bind nothing *) ])
      | _ -> failwith "wrong type")
  | T.PFieldCons (p (* path *), c, Some p0), te ->
      let cons_typed (* typed constructor *) = check_field_cons p c env in
      let[@warning "-8"] (EFieldCons (_, _, id, cons_ty)) = cons_typed in
      let p0, binds = check_PCons_aux cons_ty p0 te in
      (PCons (c, id, Some p0), binds)
  | T.PVal v, te ->
      let v_typed = check_const v in
      U.unify (get_ty v_typed) te;
      (PVal v, [])
  | T.PTuple pats, te ->
      let payload_tvs = List.map (fun _ -> P.make_tv ()) pats in
      U.unify te (I.TTuple payload_tvs);
      let pats, vars =
        List.fold_left2
          (fun (pats_acc, vars_acc) pat te ->
            let pat, vars = check_pattern pat te env in
            (pats_acc @ [ pat ], vars_acc @ vars))
          ([], []) pats payload_tvs
      in
      (PTuple pats, vars)

and check_let x e0 e1 env =
  let e0_typed, env = check_let_binding x e0 env in
  let e1_typed = check_expr e1 env in
  ELet (x, e0_typed, e1_typed, get_ty e1_typed)

and check_let_binding x e0 env : expr * Env.t =
  Poly.enter_level ();
  let e_typed = check_expr e0 env in
  Poly.exit_level ();
  let e_ty = get_ty e_typed in
  let gen (* generalized type *) = P.generalize e_ty env in
  let env = Env.add_value x gen env in
  (e_typed, env)

and check_letrec binds body env : expr =
  let env, vars, lams_typed = check_letrec_binding binds env in
  let body_typed = check_expr body env in
  ELetrec (List.combine vars lams_typed, body_typed, get_ty body_typed)

and check_letrec_binding binds env =
  let origin_env = env in
  Poly.enter_level ();
  let tvs = List.map (fun _ -> ([], P.make_tv ())) binds in
  let env =
    List.fold_left2
      (fun acc (x, _) tv -> Env.add_value x tv acc)
      env binds tvs
  in
  let vars = List.map fst binds in
  let lams = List.map snd binds in
  let lams_typed =
    List.fold_left2
      (fun acc x (para, body) ->
        let lam_typed = check_lambda para body env in
        let lam_ty = get_ty lam_typed in
        U.unify lam_ty (P.inst (Env.lookup_var_type x env));
        acc @ [ lam_typed ])
      [] vars lams
  in
  Poly.exit_level ();
  let lams_typed =
    List.map
      (function
        | ELam (x, body, ty) -> (x, body, ty)
        | _ -> failwith "neverreach")
      lams_typed
  in
  let env =
    List.fold_left2
      (fun acc_env (_, _, te) x ->
        Env.add_value x (P.generalize te origin_env) acc_env)
      origin_env lams_typed vars
  in
  (env, vars, lams_typed)

and check_lambda para body env0 : expr =
  match para with
  | T.PAnn (x, t) ->
      let ann = normalize_ty t env0 in
      let env = Env.add_value x ([], ann) env0 in
      let body0 = check_expr body env in
      let body_ty0 = get_ty body0 in
      ELam (x, body0, I.TArrow (ann, body_ty0))
  | T.PBare x ->
      let tv = P.make_tv () in
      let env = Env.add_value x ([], tv) env0 in
      let body0 = check_expr body env in
      let body_ty0 = get_ty body0 in
      ELam (x, body0, I.TArrow (tv, body_ty0))

and check_if_else c e1 e2 env : expr =
  let c_typed = check_expr c env in
  U.unify (get_ty c_typed) I.bool_ty;
  let e1_typed = check_expr e1 env in
  let e2_typed = check_expr e2 env in
  U.unify (get_ty e1_typed) (get_ty e2_typed);
  EIf (c_typed, e1_typed, e2_typed, get_ty e1_typed)

and check_app op arg env =
  let op_typed = check_expr op env in
  let op_ty = get_ty op_typed in
  let arg_typed = check_expr arg env in
  let arg_ty = get_ty arg_typed in
  let tv = P.make_tv_of "'ret" in
  U.unify op_ty (I.TArrow (arg_ty, tv));
  EApp (op_typed, arg_typed, tv)

and check_cmp op e0 e1 env =
  let e0_typed = check_expr e0 env in
  let e1_typed = check_expr e1 env in
  U.unify (get_ty e0_typed) (get_ty e1_typed);
  ECmp (op, e0_typed, e1_typed, I.bool_ty)

and check_seq e0 e1 env =
  let e0_typed = check_expr e0 env in
  U.unify (get_ty e0_typed) I.unit_ty;
  let e1_typed = check_expr e1 env in
  let e1_ty = get_ty e1_typed in
  ESeq (e0_typed, e1_typed, e1_ty)

and check_cases e bs env =
  let e_typed = check_expr e env in
  let e_ty = get_ty e_typed in
  let res_ty = P.make_tv_of "'res" in
  let bs_typed =
    List.fold_left
      (fun bs_typed ((p : T.pattern) (* pattern *), res) ->
        (* get binding created by pattern *)
        let p, vars = check_pattern p e_ty env in
        (* we don't need do some thing like [let res = U.apply_expr_untypd u1
           res], the [res_typed] will get unified result when type variable
           pool being maintained correctly *)
        (* check result expression in its corresponding environment *)
        let env =
          List.fold_left
            (fun env (v, ty) ->
              Env.add_value v ([], ty (* no generalize here*)) env)
            env vars
        in
        let res_typed = check_expr res env in
        let res_ty' = get_ty res_typed in
        (* unify checked result type with current result type *)
        U.unify res_ty' res_ty;
        bs_typed @ [ (p, res_typed) ])
      [] bs
  in
  ECase (e_typed, bs_typed, res_ty)

and check_tuple es env =
  let es_typed =
    List.fold_left
      (fun acc e ->
        let e_typed = check_expr e env in
        acc @ [ e_typed ])
      [] es
  in
  let tu_te = I.TTuple (List.map get_ty es_typed) in
  ETuple (es_typed, tu_te)

and check_cons c env =
  let t, id = Env.lookup_constr_type c env in
  ECons (c, id, P.inst t)

and check_field_cons me c env =
  let me_typed = check_mod me env in
  match get_mod_ty me_typed with
  | I.MTMod st ->
      let t, id = R.find_constr_comp c st in
      EFieldCons (me_typed, c, id, P.inst t)
  | I.MTFun _ -> failwith "try get field from functor"

and check_field me x env =
  let me_typed = check_mod me env in
  match get_mod_ty me_typed with
  | I.MTMod st -> EField (me_typed, x, P.inst (R.find_val_comp x st))
  | I.MTFun _ -> failwith "try get field from functor"

and check_ann e te env =
  let e_typed = check_expr e env in
  let te = normalize_ty te env in
  U.unify te (get_ty e_typed);
  e_typed

(* typing top levels *)
and check_top_level (top : T.top_level) env : top_level list * Env.t =
  let old_pool = !tv_pool in
  reset_pool ();
  let tops_typed, env =
    match top with
    | T.TopLet (x, e) ->
        let e_typed0, env = check_let_binding x e env in
        ([ TopLet (x, e_typed0) ], env)
    | T.TopLetRec binds ->
        let env, vars, lams = check_letrec_binding binds env in
        let binds = List.combine vars lams in
        ([ TopLetRec binds ], env)
    | T.TopTypeDef defs ->
        let defs, env = check_ty_def_group defs env in
        (defs, env)
    | T.TopMod (name, me) ->
        let me_typed = check_mod me env in
        ( [ TopMod (name, me_typed) ],
          Env.add_module name (get_mod_ty me_typed) env )
    | T.TopModSig (name, ext_mt) ->
        let mt = normalize_mt ext_mt env in
        ([ TopModSig (name, mt) ], Env.add_module_sig name mt env)
    | T.TopExternal (name, e_ty, ext_name) ->
        P.enter_level ();
        let te = normalize e_ty Let env in
        P.exit_level ();
        let gen = P.generalize te env in
        ([ TopExternal (name, te, ext_name) ], Env.add_value name gen env)
  in
  tv_pool := old_pool;
  (tops_typed, env)

and check_ty_def_group defs env =
  (* Create a temporary special environment for normalizing type definition,
     with typed definition pushed as an opaque type. We push type definitions
     to this environment by first *)
  let normalize_env =
    List.fold_left
      (fun env def ->
        match def with
        | T.TDAdt (name, ty_para_names, _) ->
            Env.add_type_def (I.TDOpaque (name, ty_para_names)) env
        | T.TDRecord (name, _, _)
        | T.TDAlias (name, _) ->
            Env.add_type_def (I.TDOpaque (name, [])) env)
      env defs
  in
  let defs = Util.reorg_ty_defs defs in
  let normalized_defs, env, _ =
    List.fold_left
      (fun (acc, env, normalize_env) def ->
        match def with
        | T.TDAdt (name, ty_para_names, _) as def_ext ->
            let tid = Env.mk_tid name env in
            let def = normalize_def def_ext normalize_env in
            let[@warning "-8"] (I.TDAdt (_, _, bs)) = def in
            let env = Env.add_type_def def env in
            let normalize_env = Env.add_type_def def normalize_env in
            let constructors = analyze_constructors tid ty_para_names bs in
            let env = Env.add_constrs constructors env in
            (TopTypeDef def :: acc, env, normalize_env)
        | _ as def_ext ->
            let def = normalize_def def_ext normalize_env in
            ( TopTypeDef def :: acc,
              Env.add_type_def def env,
              Env.add_type_def def normalize_env ))
      ([], env, normalize_env) defs
  in
  (normalized_defs, env)

and analyze_constructors (tid : I.ty_id) para_names (bs : I.variant list) :
    (string * (I.bind_ty * int)) list =
  List.mapi
    (fun id (branch : I.variant) ->
      match branch with
      | c, None ->
          ( c,
            ( ( para_names,
                I.TCons (tid, List.map (fun id -> I.TQVar id) para_names) ),
              id ) )
      | c, Some payload ->
          ( c,
            ( ( para_names,
                I.TArrow
                  ( payload,
                    TCons (tid, List.map (fun id -> I.TQVar id) para_names)
                  ) ),
              id ) ))
    bs

(* typing program (content of module) *)
and check_top_levels (prog : T.top_level list) env : program * Env.t =
  match prog with
  | [] -> ([], env)
  | top :: rest ->
      let tops_typed0, env = check_top_level top env in
      let rest_tops_typed1, env = check_top_levels rest env in
      (tops_typed0 @ rest_tops_typed1, env)

and make_mt_by_scope
    {
      Env.values;
      constrs;
      types;
      modules;
      module_sigs;
      module_dict = _;
      curr;
      history;
      hints = _;
    } =
  I.MTMod
    {
      id = curr;
      val_defs = values;
      constr_defs = constrs;
      ty_defs = types;
      mod_sigs = module_sigs;
      mod_defs = modules;
      owned_mods = !history;
    }

and check_mod (me : T.mod_expr) (env : Env.t) : mod_expr =
  let me_typed =
    try
      match me.desc with
      | T.MEName name -> check_mod_name name env
      | T.MEStruct body -> check_struct body env
      | T.MEFunctor ((name, ext_mt0), me1) ->
          check_functor name ext_mt0 me1 env
      | T.MEField (me, name) -> check_mod_field me name env
      | T.MEApply (me0, me1) -> check_mod_apply me0 me1 env
      | T.MERestrict (me, mt) -> check_mod_restrict me mt env
    with
    | err -> Report.wrap_and_reraise err me.start_loc me.end_loc env
  in
  Env.try_record_hint me_typed env;
  me_typed

and check_mod_name name env = MEName (name, Env.lookup_module_def name env)

and check_struct body env =
  let body_typed, env' = check_top_levels body (Env.enter_env env) in
  let scope = absorb_history env' env in
  let mt = make_mt_by_scope scope in
  MEStruct (body_typed, mt)

and check_functor name ext_mt0 me1 env =
  let mt0 = normalize_mt ext_mt0 env in
  let me1_typed = check_mod me1 (Env.add_module name mt0 env) in
  MEFunctor ((name, mt0), me1_typed)

and check_mod_field me name env =
  let me_typed = check_mod me env in
  match get_mod_ty me_typed with
  | I.MTMod st -> MEField (me_typed, name, R.find_mod_comp name st)
  | I.MTFun _ -> failwith "try get field from functor"

and check_mod_apply me0 me1 env =
  let me0_typed = check_mod me0 env in
  let me1_typed = check_mod me1 env in
  let mt0 = get_mod_ty me0_typed in
  let mt1 = get_mod_ty me1_typed in
  match mt0 with
  | I.MTMod _ -> failwith "try apply a structure"
  | I.MTFun (para_mt, body_mt) ->
      MEApply (me0_typed, me1_typed, apply_functor para_mt body_mt mt1 env)

and check_mod_restrict me mt env =
  let me_typed = check_mod me env in
  let mt = normalize_mt mt env in
  let _subst = check_subtype (get_mod_ty me_typed) mt in
  MERestrict (me_typed, mt, shift_mt mt env)

(* apply a functor, add returned module type's id into environment *)
and apply_functor para_mt body_mt arg_mt env =
  let subst = check_subtype arg_mt para_mt in
  let substituted = subst body_mt in
  let body_mt = shift_mt substituted env in
  body_mt

and shift_mt (mt : I.mod_ty) env : I.mod_ty =
  (* collect type ids need to be shift, which correspond to all modules
     created by the functor body *)
  let dict =
    let result = ref IntMap.empty in
    let rec go mt =
      match (mt : I.mod_ty) with
      | I.MTMod
          {
            id;
            val_defs = _;
            constr_defs = _;
            ty_defs = _;
            mod_sigs;
            mod_defs;
            owned_mods;
          } ->
          List.iter
            (fun id ->
              if not (IntMap.mem id !result) then
                (* if not a mapped id, map it to a new id *)
                result := IntMap.add id (Env.env_newid env) !result)
            (id :: owned_mods);
          List.iter (fun (_, mt) -> go mt) mod_defs;
          List.iter (fun (_, mt) -> go mt) mod_sigs
      | I.MTFun (_para_mt, _body_mt) ->
          (* It's OK to ignore functor's input and output in shifting,
             because they are not indicate modules "created" by the functor.
             They are just module types indicate compatibility. *)
          ()
    in
    go mt;
    !result
  in
  (* for now dict is read-only *)
  let mapper =
    let get_id_or_default id =
      match IntMap.find_opt id dict with
      | Some id' -> id'
      | None -> id
    in
    let mapper =
      object (_self)
        (* todo: remove this object *)
        inherit [_] Types_in.map as super

        method! visit_MTMod () st =
          let shifted_id = get_id_or_default st.id in
          super#visit_MTMod ()
            {
              st with
              id = shifted_id;
              owned_mods = List.map get_id_or_default st.owned_mods;
            }

        method! visit_ty_id () (id, name) = (get_id_or_default id, name)

        method! visit_tv () tv =
          match tv with
          | I.Unbound _ ->
              failwith
                "neverreach: every module should have empty inference space"
          | I.Link _ -> super#visit_tv () tv
      end
    in
    fun mt -> mapper#visit_mod_ty () mt
  in
  mapper mt

and normalize_def (t : T.ty_def) env : I.ty_def =
  let normed =
    match t with
    | T.TDAdt (n, tvs, vs) ->
        let vs =
          List.map
            (function
              | c, None -> (c, None)
              | c, Some payload -> (c, Some (normalize payload Type env)))
            vs
        in
        I.TDAdt (n, tvs, vs)
    | T.TDRecord (n, tvs, fields) ->
        I.TDRecord
          (n, tvs, List.map (fun (x, t) -> (x, normalize t Type env)) fields)
    | T.TDAlias (n, te) -> I.TDAlias (n, normalize te Type env)
  in
  normed

and normalize_ty t env = normalize t Let env

and normalize (t : T.ty) (ctx : norm_ctx) (env : Env.t) : I.ty =
  match t with
  | T.TField (me, n, tes) -> (
      let tes = List.map (fun te -> normalize te ctx env) tes in
      let me_typed = check_mod me env in
      let mod_ty = get_mod_ty me_typed in
      match mod_ty with
      | I.MTMod st -> (
          match R.find_ty_def n st with
          | I.TDOpaque (_, _)
          | I.TDAdt (_, _, _)
          | I.TDRecord (_, _, _) ->
              TCons ((st.id, n), tes)
          | I.TDAlias (_, te) -> (
              match tes with
              | [] -> te
              | _ :: _ ->
                  failwith "try to provide type parameter to a type alias"))
      | I.MTFun _ -> failwith "try get a field from functor")
  | T.TCons (c, tes) -> (
      let id, def = Env.lookup_type_def c env in
      match def with
      | I.TDOpaque (_, _)
      | I.TDAdt (_, _, _)
      | I.TDRecord (_, _, _) ->
          TCons ((id, c), List.map (fun t -> normalize t ctx env) tes)
      | I.TDAlias (_, te) -> (
          match tes with
          | [] -> te
          | _ -> failwith "apply type to type alias"))
  | T.TVar x -> (
      match ctx with
      | Type -> TQVar x
      | Let -> pool_make_tv x)
  | T.TArrow (t0, t1) -> TArrow (normalize t0 ctx env, normalize t1 ctx env)
  | T.TTuple ts -> TTuple (List.map (fun t -> normalize t ctx env) ts)
  | T.TRecord fields ->
      TRecord (List.map (fun (x, t) -> (x, normalize t ctx env)) fields)

and normalize_mt (me : T.mod_ty) env : I.mod_ty =
  match me with
  | T.MTName name -> Env.lookup_module_sig name env
  | T.MTField (me, name) -> (
      let me_typed = check_mod me env in
      let mt = get_mod_ty me_typed in
      match mt with
      | I.MTMod st -> R.find_mod_sig_comp name st
      | I.MTFun (_mt0, _mt1) -> failwith "try get field from functor")
  | T.MTSig comps ->
      let env' = normalize_msig comps (Env.enter_env env) in
      let scope = absorb_history env' env in
      make_mt_by_scope scope
  | T.MTFunctor (m0, ext_mt0, m1) ->
      let mt0 = normalize_mt ext_mt0 env in
      let mt1 = normalize_mt m1 (Env.add_module m0 mt0 env) in
      MTFun (mt0, mt1)

and normalize_msig comps env =
  match comps with
  | [] -> env
  | comp :: comps ->
      let env =
        match comp with
        | T.SpecVal (name, te) ->
            reset_pool ();
            P.enter_level ();
            let normalized = normalize_ty te env in
            P.exit_level ();
            Env.add_value name (P.generalize normalized env) env
        | T.SpecAbstTy (name, paras) ->
            Env.add_type_def (TDOpaque (name, paras)) env
        | T.SpecManiTy def ->
            let _, env = check_top_level (T.TopTypeDef def) env in
            env
        | T.SpecMod (name, ext_mt) ->
            let mt = normalize_mt ext_mt env in
            Env.add_module name mt env
        | T.SpecModSig (name, ext_mt) ->
            let mt = normalize_mt ext_mt env in
            Env.add_module_sig name mt env
      in
      normalize_msig comps env
