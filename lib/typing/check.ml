open Typedtree
module U = Unify
module T = Syntax.Parsetree
module I = Types_in
module IdMap = Map.Make (Ident)

let make_tv () =
  let name = "'_t" in
  I.TVarI (ref (I.Unbound (Ident.create ~hint:name)))

let make_tv_of hint = I.TVarI (ref (I.Unbound (Ident.create ~hint)))

let inst_with (t : I.bind_ty) tes : I.ty =
  let qvs, te = t in
  let dict = List.combine qvs tes in
  let mapper =
    object (_self : 'self)
      inherit ['self] I.map

      method! visit_TQVarI () qtv = List.assoc qtv dict
    end
  in
  mapper#visit_ty () te

let inst (t : I.bind_ty) : I.ty =
  (* We can gaurantee that captured type variables will never duplicated with
     free type variables *)
  let qvs, _ = t in
  let new_tvs =
    List.map (fun id -> make_tv_of (Ident.name_of_ident id)) qvs
  in
  inst_with t new_tvs

let get_all_tvs (e : I.ty) : I.tv ref list =
  let tvs = ref [] in
  let collector =
    object (self : 'self)
      inherit ['self] Types_in.iter

      method! visit_TVarI () tv =
        match !tv with
        | I.Unbound _ ->
            tvs := tv :: !tvs (* only collect unbound type variable *)
        | I.Link te -> self#visit_ty () te
    end
  in
  collector#visit_ty () e;
  List.rev
    (List.fold_left
       (fun xs x -> if List.memq x xs then xs else x :: xs)
       [] !tvs)

let get_all_qtvs (e : I.ty) : Ident.t list =
  let qtvs = ref [] in
  let collector =
    object (_self : 'self)
      inherit ['self] Types_in.iter

      method! visit_TQVarI () name =
        qtvs := name :: !qtvs (* only collect unbound type variable *)
    end
  in
  collector#visit_ty () e;
  List.rev
    (List.fold_left
       (fun xs x -> if List.mem x xs then xs else x :: xs)
       [] !qtvs)

let generalize (t : I.ty) (env : Env.t) : I.bind_ty =
  let tvs = get_all_tvs t in
  (* get all type variables *)
  let uncaptured_tvs =
    tvs
    |> List.filter (function
         | { contents = I.Unbound _ } -> true
         | _ -> false)
    |> List.filter (fun x -> not (Env.captured env x))
  in
  let qvs = ref [] in
  let rec gen (t : I.ty) =
    match t with
    | I.TVarI ({ contents = I.Unbound x } as tv) ->
        if List.memq tv uncaptured_tvs then (
          if not (List.mem x !qvs) then qvs := x :: !qvs;
          I.TQVarI x)
        else t
    | I.TVarI { contents = I.Link t } -> gen t
    | I.TConsI (c, tes) -> I.TConsI (c, List.map gen tes)
    | I.TQVarI _ -> t
    | I.TArrowI (t1, t2) -> I.TArrowI (gen t1, gen t2)
    | I.TTupleI tes -> I.TTupleI (List.map gen tes)
    | I.TRecordI fields ->
        I.TRecordI (List.map (fun (name, te) -> (name, gen te)) fields)
  in
  (!qvs, gen t)

let env_id = ref 0

let enter_env env =
  env_id := 1 + !env_id;
  { Env.init_scope with curr = !env_id } :: env

let tv_pool = ref IdMap.empty

let reset_pool () = tv_pool := IdMap.empty

let recover_pool pool = tv_pool := pool

let pool_make_tv n =
  match IdMap.find_opt n !tv_pool with
  | Some tpv -> tpv
  | None -> make_tv ()

(* prune one scope from env1 last of env0 *)
let prune (env0 : Env.t) (env1 : Env.t) : Env.scope =
  match env0 with
  | s :: env0' when Env.size env1 = Env.size env0' -> s
  | _ -> failwith "neverreach"

let get_type_id_mod name ty_defs =
  match
    List.find_opt
      (function
        | I.TDOpaqueI (x, _)
        | TDAdtI (x, _, _)
        | TDRecordI (x, _, _) ->
            Ident.name_of_ident x = name)
      ty_defs
  with
  | Some def -> I.get_def_name def
  | None -> failwith "type not found"

type norm_ctx =
  | Type
  | Let

(* typing expression *)
let rec tc_expr (e : T.expr) (env : Env.t) : expr =
  (* look a binding won't unify anything *)
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
  | T.CBool _ -> EConst (c, I.bool_ty)
  | T.CInt _ -> EConst (c, I.int_ty)
  | T.CString _ -> EConst (c, I.string_ty)

and tc_var x env =
  let bind = Env.get_value_type x env in
  let t = inst bind in
  EVar (x, t)

(* pattern will create bindings under context's type *)
and tc_pattern p te env : pattern * (string * I.ty) list =
  let tc_PCons_aux (cons_ty : I.ty) (p (* payload pattern *) : T.pattern) te0
      =
    match cons_ty with
    | I.TArrowI (pay_ty (* payload type *), te1) ->
        U.unify te1 te0;
        let p, vars = tc_pattern p pay_ty env in
        (p, vars)
    | _ -> failwith "wrong type"
  in
  match (p, te) with
  | T.PVar x, te -> (PVar x, [ (x, te) ])
  | T.PCons (c, None), te -> (
      let cons_ty_gen (* type of constructor *) = Env.get_value_type c env in
      let cons_ty = inst cons_ty_gen in
      U.unify cons_ty te;
      match cons_ty with
      | I.TConsI (_, []) -> (PCons (c, None), [])
      | _ -> failwith "wrong type")
  | T.PFieldCons (p, c, None), te -> (
      let cons_typed (* constructor *) = tc_field_cons p c env in
      let cons_ty = get_ty cons_typed in
      U.unify cons_ty te;
      (* unify cons_ty with te *)
      match cons_ty with
      | I.TConsI (_, _) -> (PCons (c, None), [ (* bind nothing *) ])
      | _ -> failwith "wrong type")
  | T.PCons (c, Some p0 (* pattern *)), te ->
      let cons_ty_gen (* type of constructor *) = Env.get_value_type c env in
      let p0, binds = tc_PCons_aux (inst cons_ty_gen) p0 te in
      (PCons (c, Some p0), binds)
  | T.PFieldCons (p (* path *), c, Some p0), te ->
      let cons_typed (* typed constructor *) = tc_field_cons p c env in
      let cons_ty = get_ty cons_typed in
      tc_PCons_aux cons_ty p0 te
  | T.PVal v, te ->
      let v_typed = tc_const v in
      U.unify (get_ty v_typed) te;
      (PVal v, [])
  | T.PTuple pats, te -> (
      U.unify te (I.TTupleI (List.map (fun _ -> make_tv ()) pats));
      match te with
      | I.TTupleI tes ->
          let pats, vars =
            List.fold_left2
              (fun (pats_acc, vars_acc) pat te ->
                let pat, vars = tc_pattern pat te env in
                (pats_acc @ [ pat ], vars_acc @ vars))
              ([], []) pats tes
          in
          (PTuple pats, vars)
      | _ -> failwith "wrong")

and tc_let x e0 e1 env =
  let e0_typed, env = tc_let_binding x e0 env in
  let e1_typed = tc_expr e1 env in
  ELet (x, e0_typed, e1_typed, get_ty e1_typed)

and tc_let_binding x e0 env : expr * Env.t =
  let e_typed = tc_expr e0 env in
  let e_ty = get_ty e_typed in
  let gen (* generalized type *) = generalize e_ty env in
  let env = Env.add_value x gen env in
  (e_typed, env)

and tc_letrec binds body env : expr =
  let env, vars, lams_typed = tc_letrec_binding binds env in
  let body_typed = tc_expr body env in
  ELetrec (List.combine vars lams_typed, body_typed, get_ty body_typed)

and tc_letrec_binding binds env =
  let tvs = List.map (fun _ -> ([], make_tv ())) binds in
  let env =
    List.fold_left2
      (fun acc (x, _) tv -> Env.add_value x tv acc)
      env binds tvs
  in
  let vars = List.map fst binds in
  let lams = List.map snd binds in
  let env, lams_typed =
    List.fold_left2
      (fun (env, acc) x (para, body) ->
        let lam_typed = tc_lambda para body env in
        let lam_ty = get_ty lam_typed in
        U.unify lam_ty (inst (Env.get_value_type x env));
        (env, acc @ [ lam_typed ]))
      (env, []) vars lams
  in
  let lams_typed =
    List.map
      (function
        | ELam (x, body, ty) -> (x, body, ty)
        | _ -> failwith "neverreach")
      lams_typed
  in
  let env =
    List.fold_left2
      (fun acc (_, _, te) x -> Env.add_value x (generalize te env) acc)
      env lams_typed vars
  in
  (env, vars, lams_typed)

and tc_lambda para body env0 : expr =
  match para with
  | T.PAnn (x, t) ->
      let ann = normalize_ty t env0 in
      let env = Env.add_value x ([], ann) env0 in
      let body0 = tc_expr body env in
      let body_ty0 = get_ty body0 in
      ELam (x, body0, I.TArrowI (ann, body_ty0))
  | T.PBare x ->
      let tv = make_tv () in
      let env = Env.add_value x ([], tv) env0 in
      let body0 = tc_expr body env in
      let body_ty0 = get_ty body0 in
      ELam (x, body0, I.TArrowI (tv, body_ty0))

and tc_if_else c e1 e2 env : expr =
  let c_typed = tc_expr c env in
  U.unify (get_ty c_typed) I.bool_ty;
  let e1_typed = tc_expr e1 env in
  let e2_typed = tc_expr e2 env in
  U.unify (get_ty e1_typed) (get_ty e2_typed);
  EIf (c_typed, e1_typed, e2_typed, get_ty e1_typed)

and tc_app op arg env =
  let op_typed = tc_expr op env in
  let op_ty = get_ty op_typed in
  let arg_typed = tc_expr arg env in
  let arg_ty1 = get_ty arg_typed in
  let tv = make_tv_of "ret" in
  U.unify op_ty (I.TArrowI (arg_ty1, tv));

  EApp (op_typed, arg_typed, tv)

and tc_cases e bs env =
  let e_typed = tc_expr e env in
  let e_ty = get_ty e_typed in
  let res_ty = make_tv_of "res" in
  let bs_typed =
    List.fold_left
      (fun bs_typed ((p : T.pattern) (* pattern *), res) ->
        (* get binding created by pattern *)
        let p, vars = tc_pattern p e_ty env in
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
        let res_typed = tc_expr res env in
        let res_ty' = get_ty res_typed in
        (* unify checked result type with current result type *)
        U.unify res_ty' res_ty;
        bs_typed @ [ (p, res_typed) ])
      [] bs
  in
  ECase (e_typed, bs_typed, res_ty)

and tc_tuple es env =
  let es_typed =
    List.fold_left
      (fun acc e ->
        let e_typed = tc_expr e env in
        acc @ [ e_typed ])
      [] es
  in
  let tu_te = I.TTupleI (List.map get_ty es_typed) in
  ETuple (es_typed, tu_te)

and tc_cons c env =
  let t = Env.get_value_type c env |> inst in
  ECons (c, t)

and tc_field_cons me c env =
  let me_typed = tc_mod me env in
  match get_mod_ty me_typed with
  | I.MTMod { val_defs; _ } ->
      EFieldCons (me_typed, c, inst (List.assoc c val_defs))
  | I.MTFun _ -> failwith "try get field from functor"

and tc_field me x env =
  let me_typed = tc_mod me env in
  match get_mod_ty me_typed with
  | I.MTMod { val_defs; _ } ->
      EField (me_typed, x, inst (List.assoc x val_defs))
  | I.MTFun _ -> failwith "try get field from functor"

and tc_ann e te env =
  let e_typed = tc_expr e env in
  let te = normalize_ty te env in
  U.unify te (get_ty e_typed);
  e_typed

(* typing top levels *)
and tc_toplevel (top : T.top_level) env : top_level * Env.t =
  let old_pool = !tv_pool in
  reset_pool ();
  let top_typed =
    match top with
    | T.TopLet (x, e) ->
        let e_typed0, env = tc_let_binding x e env in
        (TopLet (x, e_typed0), env)
    | T.TopLetRec binds ->
        let env, vars, lams = tc_letrec_binding binds env in
        let binds = List.combine vars lams in
        (TopLetRec binds, env)
    | T.TopTypeDef (TDAdt (name, ty_para_names, _) as def_ext) ->
        let tid = Env.mk_tid name env in
        let def = normalize_def def_ext env in
        let[@warning "-8"] (I.TDAdtI (_, _, bs)) = def in
        let env = Env.add_type_def def env in
        let constructors = analyze_constructors tid ty_para_names bs in
        let env = Env.add_values constructors env in
        (TopTypeDef def, env)
    | T.TopTypeDef (TDRecord (_, _, _) as def_ext) ->
        let def = normalize_def def_ext env in
        (TopTypeDef def, Env.add_type_def def env)
    | T.TopMod (name, me) ->
        let me_typed = tc_mod me env in
        ( TopMod (name, me_typed),
          Env.add_module name (get_mod_ty me_typed) env )
    | T.TopModSig (name, emt) ->
        let mt = normalize_mt emt env in
        (TopModSig (name, mt), Env.add_module_sig name mt env)
  in
  tv_pool := old_pool;
  top_typed

and analyze_constructors (tid : I.ty_id) para_names (bs : I.variant list) :
    (string * I.bind_ty) list =
  List.map
    (fun (branch : I.variant) ->
      match branch with
      | c, None ->
          ( c,
            ( para_names,
              I.TConsI (tid, List.map (fun id -> I.TQVarI id) para_names) )
          )
      | c, Some payload ->
          ( c,
            ( para_names,
              I.TArrowI
                ( payload,
                  TConsI (tid, List.map (fun id -> I.TQVarI id) para_names)
                ) ) ))
    bs

(* typing program (content of module) *)
and tc_tops (prog : T.top_level list) env : program * Env.t =
  match prog with
  | [] -> ([], env)
  | top :: rest ->
      let top_typed0, env = tc_toplevel top env in
      let rest_typed1, env = tc_tops rest env in
      (top_typed0 :: rest_typed1, env)

and make_env_mt { Env.values; types; modules; module_sigs; _ } =
  I.MTMod
    {
      val_defs = values;
      ty_defs = types;
      mod_sigs = module_sigs;
      mod_defs = modules;
    }

and tc_mod (me : T.mod_expr) (env : Env.t) : mod_expr =
  match me with
  | T.MEName name -> MEName (name, Env.get_module_def name env)
  | T.MEStruct body ->
      let body_typed, env' = tc_tops body (enter_env env) in
      let env_diff = prune env' env in
      let mt = make_env_mt env_diff in
      MEStruct (body_typed, mt)
  | T.MEFunctor ((name, emt0), me1) ->
      let mt0 = normalize_mt emt0 (enter_env env) in
      let me1_typed = tc_mod me1 (Env.add_module name mt0 (enter_env env)) in
      MEFunctor ((name, mt0), me1_typed)
  | T.MEField (me, name) -> (
      let me_typed = tc_mod me env in
      match get_mod_ty me_typed with
      | I.MTMod { mod_defs; _ } ->
          MEField (me_typed, name, List.assoc name mod_defs)
      | I.MTFun _ -> failwith "try get field from functor")
  | T.MEApply (me0, me1) -> (
      let me0_typed = tc_mod me0 env in
      let me1_typed = tc_mod me1 env in
      let mt0 = get_mod_ty me0_typed in
      let mt1 = get_mod_ty me1_typed in
      match mt0 with
      | I.MTMod _ -> failwith "try apply a structure"
      | I.MTFun (_, _) ->
          MEApply
            ( me0_typed,
              me1_typed,
              applier mt1 (* can we staging this application *) ))
  | T.MERestrict (me, mt) ->
      let me_typed = tc_mod me env in
      let mty = normalize_mt mt env in
      let mty = check_subtype (get_mod_ty me_typed) mty in
      MERestrict (me_typed, mty)

and check_subtype mt0 mt1 : I.mod_ty =
  let get_def name ty_defs =
    List.find
      (fun td ->
        match td with
        | I.TDOpaqueI (name', _)
        | I.TDAdtI (name', _, _)
        | I.TDRecordI (name', _, _)
          when name' = name ->
            true
        | _ -> false)
      ty_defs
  in
  let rec compatible mt0 mt1 : I.mod_ty =
    match (mt0, mt1) with
    | ( I.MTMod
          {
            val_defs = vds0;
            ty_defs = tds0;
            mod_defs = mds0;
            mod_sigs = _ms0;
          },
        I.MTMod
          {
            val_defs = vds1;
            ty_defs = tds1;
            mod_defs = mds1;
            mod_sigs = ms1;
          } ) ->
        I.MTMod
          {
            val_defs =
              List.map
                (fun (name, vd1) ->
                  let vd0 = List.assoc name vds0 in
                  if vd0 <> vd1 then
                    failwith
                      (Printf.sprintf
                         "a value binding component `%s` not compatible" name)
                  else (name, vd0))
                vds1;
            ty_defs =
              List.map
                (fun td ->
                  match td with
                  | I.TDOpaqueI (name, paras) -> (
                      let td0 = get_def name tds0 in
                      match td0 with
                      | I.TDOpaqueI (_, paras0)
                      | I.TDAdtI (_, paras0, _)
                      | I.TDRecordI (_, paras0, _) ->
                          if List.length paras0 <> List.length paras then
                            failwith
                              "number of type parameter not compatible in \
                               opaque type"
                          else td0)
                  | _ ->
                      let td0 = get_def (I.get_def_name td) tds0 in
                      if td <> td0 then
                        failwith "a type def component not compatible"
                      else td0)
                tds1;
            mod_defs =
              List.map
                (fun (name, md1) ->
                  (name, compatible md1 (List.assoc name mds0)))
                mds1;
            mod_sigs =
              List.map
                (fun (name, ms1) ->
                  (name, compatible ms1 (List.assoc name mds0)))
                ms1;
          }
    | I.MTFun (argt0, mt0), I.MTFun (argt1, mt1) ->
        let arg_t = compatible argt1 argt0 in
        let ret_t = compatible mt0 mt1 in
        I.MTFun (arg_t, ret_t)
    | _ -> failwith "subtype check error"
  in
  compatible mt0 mt1

and normalize_def (t : T.ety_def) env : I.ty_def =
  let normed =
    match t with
    | T.TDAdt (n, tvs, vs) ->
        let name = Ident.create ~hint:n in
        let env = Env.add_type_def (TDAdtI (name, tvs, [])) env in
        let vs =
          List.map
            (function
              | c, None -> (c, None)
              | c, Some payload -> (c, Some (normalize payload Type env)))
            vs
        in
        I.TDAdtI (Ident.create ~hint:n, tvs, vs)
    | T.TDRecord (n, tvs, fields) ->
        let name = Ident.create ~hint:n in
        let env = Env.add_type_def (TDRecordI (name, tvs, [])) env in
        I.TDRecordI
          ( Ident.create ~hint:n,
            tvs,
            List.map (fun (x, t) -> (x, normalize t Type env)) fields )
  in
  normed

and normalize_ty t env = normalize t Let env

and normalize (t : T.ety) (ctx : norm_ctx) (env : Env.t) : I.ty =
  match t with
  | T.TField (me, n, tes) -> (
      (* todo: return an transparent type when it get supported *)
      let tes = List.map (fun te -> normalize te ctx env) tes in
      let me_typed = tc_mod me env in
      let mod_ty = get_mod_ty me_typed in
      match mod_ty with
      | I.MTMod { ty_defs; _ } -> TConsI (get_type_id_mod n ty_defs, tes)
      | I.MTFun _ -> failwith "try get a field from functor")
  | T.TCons (c, tes) ->
      (* todo: fix, use lookuped environment index *)
      let def = Env.get_type_def c env in
      TConsI (I.get_def_name def, List.map (fun t -> normalize t ctx env) tes)
  | T.TVar x -> (
      match ctx with
      | Type -> TQVarI x
      | Let -> pool_make_tv x)
  | T.TArrow (t0, t1) -> TArrowI (normalize t0 ctx env, normalize t1 ctx env)
  | T.TTuple ts -> TTupleI (List.map (fun t -> normalize t ctx env) ts)
  | T.TRecord fields ->
      TRecordI (List.map (fun (x, t) -> (x, normalize t ctx env)) fields)
  | T.TInternal ti -> ti

and normalize_mt (me : T.emod_ty) env : I.mod_ty =
  match me with
  | T.MTName name -> Env.get_module_sig name env
  | T.MTField (me, name) -> (
      let me_typed = tc_mod me env in
      let mt = get_mod_ty me_typed in
      match mt with
      | I.MTMod mt -> List.assoc name mt.mod_sigs
      | I.MTFun (_mt0, _mt1, _applier) ->
          failwith "try get field from functor")
  | T.MTSig comps ->
      let env' = normalize_msig comps (enter_env env) in
      make_env_mt (prune env' env)
  | T.MTFunctor (m0, emt0, m1) ->
      let mt0 = normalize_mt emt0 (enter_env env) in
      let mt1 = normalize_mt m1 (Env.add_module m0 mt0 (enter_env env)) in
      MTFun
        ( mt0,
          mt1,
          fun mt0' ->
            normalize_mt m1 (Env.add_module m0 mt0' (enter_env env)) )

and normalize_msig comps env =
  match comps with
  | [] -> env
  | comp :: comps ->
      let env =
        match comp with
        | T.TValueSpec (name, te) ->
            reset_pool ();
            Env.add_value name (generalize (normalize_ty te env) env) env
        | T.TAbstTySpec (name, paras) ->
            Env.add_type_def (TDOpaqueI (Ident.create ~hint:name, paras)) env
        | T.TManiTySpec def -> Env.add_type_def (normalize_def def env) env
        | T.TModSpec (name, emt) ->
            let mt = normalize_mt emt env in
            Env.add_module name mt env
      in
      normalize_msig comps env

let tc_program (prog : T.program) env : program * Env.t =
  env_id := 0;
  (* reset module type id *)
  match prog with
  | [] -> ([], env)
  | top :: rest ->
      let top_typed0, env = tc_toplevel top env in
      let rest_typed1, env = tc_tops rest env in
      (top_typed0 :: rest_typed1, env)
