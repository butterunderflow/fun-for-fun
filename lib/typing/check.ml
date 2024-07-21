open Typedtree
module U = Unify
module T = Syntax.Parsetree
module I = Types_in
module IdMap = Map.Make (Ident)
module IntMap = Map.Make (Int)

let make_tv () =
  let name = "'_t" in
  I.TVarI (ref (I.Unbound (Ident.create ~hint:name)))

let make_tv_of hint = I.TVarI (ref (I.Unbound (Ident.create ~hint)))

let inst_with (t : I.bind_ty) tes : I.ty =
  let qvs, te = t in
  let dict = List.combine qvs tes in
  let rec go te =
    match (te : I.ty) with
    | I.TConsI (tid, tes) -> I.TConsI (tid, List.map go tes)
    | I.TVarI { contents = I.Unbound _ } -> te
    | I.TVarI { contents = I.Link t } -> go t
    | I.TQVarI qtv -> List.assoc qtv dict
    | I.TArrowI (te0, te1) -> TArrowI (go te0, go te1)
    | I.TTupleI tes -> TTupleI (List.map go tes)
    | I.TRecordI fields ->
        I.TRecordI (List.map (fun (name, te) -> (name, go te)) fields)
  in
  go te

let inst (t : I.bind_ty) : I.ty =
  (* We can gaurantee that captured type variables will never duplicated with
     free type variables *)
  let qvs, _ = t in
  let new_tvs =
    List.map (fun id -> make_tv_of (Ident.name_of_ident id)) qvs
  in
  inst_with t new_tvs

let align_inst (t : I.bind_ty) : I.ty =
  (* We can gaurantee that captured type variables will never duplicated with
     free type variables *)
  let qvs, _ = t in
  let new_tvs =
    List.mapi (fun i _id -> I.TQVarI (Ident.mk_ident i "_stable")) qvs
  in
  inst_with t new_tvs

let get_all_tvs (te : I.ty) : I.tv ref list =
  let tvs = ref [] in
  let rec go te =
    match te with
    | I.TConsI (_, tes)
    | I.TTupleI tes ->
        List.iter go tes
    | I.TVarI ({ contents = I.Unbound _ } as tv) ->
        (* only collect unbound type variable *)
        tvs := tv :: !tvs
    | I.TVarI { contents = I.Link te } -> go te
    | I.TQVarI _ -> assert false
    | I.TArrowI (te0, te1) ->
        go te0;
        go te1
    | I.TRecordI fields -> List.iter (fun (_, te) -> go te) fields
  in
  go te;
  List_utils.remove_from_left !tvs

let generalize (t : I.ty) (env : Env.t) : I.bind_ty =
  let qvs = ref [] in
  let cons_uniq x xs = if List.mem x xs then xs else x :: xs in
  let rec gen (t : I.ty) =
    match t with
    | I.TVarI ({ contents = I.Unbound x } as tv) ->
        (* if a type variable not captured by environment, we need to
           generalize it *)
        if not (Env.captured env tv) then (
          qvs := cons_uniq x !qvs;
          I.TQVarI x)
        else t
    | I.TVarI { contents = I.Link t } -> gen t
    | I.TConsI (c, tes) -> I.TConsI (c, List.map gen tes)
    | I.TQVarI _ -> failwith "neverreach"
    | I.TArrowI (t1, t2) -> I.TArrowI (gen t1, gen t2)
    | I.TTupleI tes -> I.TTupleI (List.map gen tes)
    | I.TRecordI fields ->
        I.TRecordI (List.map (fun (name, te) -> (name, gen te)) fields)
  in
  (!qvs, gen t)

let tv_pool = ref IdMap.empty

let reset_pool () = tv_pool := IdMap.empty

let recover_pool pool = tv_pool := pool

let pool_make_tv n =
  match IdMap.find_opt n !tv_pool with
  | Some tpv -> tpv
  | None -> make_tv ()

(* record top history of env0 to env1 *)
let absorb_history (env0 : Env.t) (env1 : Env.t) =
  let s = Env.prune env0 env1 in
  Env.record_all_history !(s.history) env1;
  s

type norm_ctx =
  | Type
  | Let

(* typing expression *)
let rec tc_expr (e : T.expr) (env : Env.t) : expr =
  try
    match e.node with
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
    | T.ECmp (op, e0, e1) -> tc_cmp op e0 e1 env
    | T.ESeq (e0, e1) -> tc_seq e0 e1 env
  with
  | U.UnificationError (t0, t1) ->
      Report.unification_error t0 t1 e.start_loc e.end_loc
  | U.OccurError (tv, te) -> Report.occur_error tv te e.start_loc e.end_loc
  | e -> raise e

and tc_const c =
  match c with
  | T.CBool _ -> EConst (c, I.bool_ty)
  | T.CInt _ -> EConst (c, I.int_ty)
  | T.CString _ -> EConst (c, I.string_ty)
  | T.CUnit -> EConst (c, I.unit_ty)

and tc_var x env =
  (* lookup a binding won't unify anything *)
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
    | _ -> failwith "payload constructor is not arrow type"
  in
  match (p, te) with
  | T.PVar x, te -> (PVar (x, te), [ (x, te) ])
  | T.PCons (c, None), te -> (
      let cons_ty_gen (* type of constructor *), id =
        Env.get_constr_type c env
      in
      let cons_ty = inst cons_ty_gen in
      U.unify cons_ty te;
      match cons_ty with
      | I.TConsI _ -> (PCons (c, id, None), [])
      | _ ->
          failwith
            (Printf.sprintf "wrong no-payload constructor pattern %s" c))
  | T.PFieldCons (me, c, None), te -> (
      let cons_typed (* constructor *) = tc_field_cons me c env in
      let[@warning "-8"] (EFieldCons (_, _, id, _)) = cons_typed in
      let cons_ty = get_ty cons_typed in
      U.unify cons_ty te;
      (* unify cons_ty with te *)
      match cons_ty with
      | I.TConsI (_, _) -> (PCons (c, id, None), [ (* bind nothing *) ])
      | _ -> failwith "wrong type")
  | T.PCons (c, Some p0 (* pattern *)), te ->
      let cons_ty_gen (* type of constructor *), id =
        Env.get_constr_type c env
      in
      let p0, binds = tc_PCons_aux (inst cons_ty_gen) p0 te in
      (PCons (c, id, Some p0), binds)
  | T.PFieldCons (p (* path *), c, Some p0), te ->
      let cons_typed (* typed constructor *) = tc_field_cons p c env in
      let cons_ty = get_ty cons_typed in
      tc_PCons_aux cons_ty p0 te
  | T.PVal v, te ->
      let v_typed = tc_const v in
      U.unify (get_ty v_typed) te;
      (PVal v, [])
  | T.PTuple pats, te ->
      let payload_tvs = List.map (fun _ -> make_tv ()) pats in
      U.unify te (I.TTupleI payload_tvs);
      let pats, vars =
        List.fold_left2
          (fun (pats_acc, vars_acc) pat te ->
            let pat, vars = tc_pattern pat te env in
            (pats_acc @ [ pat ], vars_acc @ vars))
          ([], []) pats payload_tvs
      in
      (PTuple pats, vars)

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
  let origin_env = env in
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
        let lam_typed = tc_lambda para body env in
        let lam_ty = get_ty lam_typed in
        U.unify lam_ty (inst (Env.get_value_type x env));
        acc @ [ lam_typed ])
      [] vars lams
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
      (fun acc_env (_, _, te) x ->
        Env.add_value x (generalize te origin_env) acc_env)
      origin_env lams_typed vars
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
  let arg_ty = get_ty arg_typed in
  let tv = make_tv_of "'ret" in
  U.unify op_ty (I.TArrowI (arg_ty, tv));

  EApp (op_typed, arg_typed, tv)

and tc_cmp op e0 e1 env =
  let e0_typed = tc_expr e0 env in
  let e1_typed = tc_expr e1 env in
  U.unify (get_ty e0_typed) (get_ty e1_typed);
  ECmp (op, e0_typed, e1_typed, I.bool_ty)

and tc_seq e0 e1 env =
  let e0_typed = tc_expr e0 env in
  U.unify (get_ty e0_typed) I.unit_ty;
  let e1_typed = tc_expr e1 env in
  let e1_ty = get_ty e1_typed in
  ESeq (e0_typed, e1_typed, e1_ty)

and tc_cases e bs env =
  let e_typed = tc_expr e env in
  let e_ty = get_ty e_typed in
  let res_ty = make_tv_of "'res" in
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
  let t, id = Env.get_constr_type c env in
  ECons (c, id, inst t)

and tc_field_cons me c env =
  let me_typed = tc_mod me env in
  match get_mod_ty me_typed with
  | I.MTMod { constr_defs; _ } ->
      let t, id = List.assoc c constr_defs in
      EFieldCons (me_typed, c, id, inst t)
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
and tc_top_level (top : T.top_level) env : top_level * Env.t =
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

        let normalize_env =
          (* special environment for normalizing type definition, with typed
             definition pushed as an opaque type *)
          Env.add_type_def (I.TDOpaqueI (name, ty_para_names)) env
        in
        let def = normalize_def def_ext normalize_env in
        let[@warning "-8"] (I.TDAdtI (_, _, bs)) = def in
        let env = Env.add_type_def def env in
        let constructors = analyze_constructors tid ty_para_names bs in
        let env = Env.add_constrs constructors env in
        (TopTypeDef def, env)
    | T.TopTypeDef (_ as def_ext) ->
        let def = normalize_def def_ext env in
        (TopTypeDef def, Env.add_type_def def env)
    | T.TopMod (name, me) ->
        let me_typed = tc_mod me env in
        ( TopMod (name, me_typed),
          Env.add_module name (get_mod_ty me_typed) env )
    | T.TopModSig (name, emt) ->
        let mt = normalize_mt emt env in
        (TopModSig (name, mt), Env.add_module_sig name mt env)
    | T.TopExternal (name, e_ty, ext_name) ->
        let te = normalize e_ty Let env in
        let gen = generalize te env in
        (TopExternal (name, te, ext_name), Env.add_value name gen env)
  in

  tv_pool := old_pool;
  top_typed

and analyze_constructors (tid : I.ty_id) para_names (bs : I.variant list) :
    (string * (I.bind_ty * int)) list =
  List.mapi
    (fun id (branch : I.variant) ->
      match branch with
      | c, None ->
          ( c,
            ( ( para_names,
                I.TConsI (tid, List.map (fun id -> I.TQVarI id) para_names)
              ),
              id ) )
      | c, Some payload ->
          ( c,
            ( ( para_names,
                I.TArrowI
                  ( payload,
                    TConsI (tid, List.map (fun id -> I.TQVarI id) para_names)
                  ) ),
              id ) ))
    bs

(* typing program (content of module) *)
and tc_top_levels (prog : T.top_level list) env : program * Env.t =
  match prog with
  | [] -> ([], env)
  | top :: rest ->
      let top_typed0, env = tc_top_level top env in
      let rest_typed1, env = tc_top_levels rest env in
      (top_typed0 :: rest_typed1, env)

and make_scope_mt
    {
      Env.values;
      constrs;
      types;
      modules;
      module_sigs;
      module_dict = _;
      curr;
      history;
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

and tc_mod (me : T.mod_expr) (env : Env.t) : mod_expr =
  match me.node with
  | T.MEName name -> MEName (name, Env.get_module_def name env)
  | T.MEStruct body ->
      let body_typed, env' = tc_top_levels body (Env.enter_env env) in
      let scope = absorb_history env' env in
      let mt = make_scope_mt scope in
      MEStruct (body_typed, mt)
  | T.MEFunctor ((name, emt0), me1) ->
      let mt0 = normalize_mt emt0 env in
      let me1_typed = tc_mod me1 (Env.add_module name mt0 env) in
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
      | I.MTFun (para_mt, body_mt) ->
          MEApply
            (me0_typed, me1_typed, apply_functor para_mt body_mt mt1 env))
  | T.MERestrict (me, mt) ->
      let me_typed = tc_mod me env in
      let mt = normalize_mt mt env in
      let mt', _ = check_subtype (get_mod_ty me_typed) mt in
      MERestrict (me_typed, mt, mt')

(* apply a functor, add returned module type's id into environment *)
and apply_functor para_mt body_mt arg_mt env =
  let _arg_mt, map = check_subtype arg_mt para_mt in
  let substituted = map body_mt in
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

        method! visit_MTMod () id val_defs constr_defs ty_defs mod_sigs
            mod_defs owned_mods =
          let shifted_id = get_id_or_default id in
          super#visit_MTMod () shifted_id val_defs constr_defs ty_defs
            mod_sigs mod_defs
            (List.map get_id_or_default owned_mods)

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

(* check if mt0 is more specifc than mt1, return an mt1 view of mt0 *)
and check_subtype (mt0 : I.mod_ty) (mt1 : I.mod_ty) :
    I.mod_ty * (I.mod_ty -> I.mod_ty) =
  let map =
    let mid_map = ref [] in
    let rec collect_mid_maping mt0 mt1 =
      match (mt0, mt1) with
      | ( I.MTMod { id = id0; mod_defs = mds0; _ },
          I.MTMod { id = id1; mod_defs = mds1; _ } ) ->
          mid_map := (id1, id0) :: !mid_map;
          List.iter
            (fun (name, md1) ->
              let md0 = List.assoc name mds0 in
              collect_mid_maping md0 md1)
            mds1
      | I.MTFun (argt0, mt0), I.MTFun (argt1, mt1) ->
          collect_mid_maping argt0 argt1;
          collect_mid_maping mt0 mt1
      | _ -> failwith "subtype check error"
    in
    collect_mid_maping mt0 mt1;
    !mid_map
  in
  let subst =
    let mapper =
      object
        (* todo: remove this object *)
        inherit [_] Types_in.map as super

        method! visit_ty_id () (id, name) =
          match List.assoc_opt id map with
          | Some id1 -> (id1, name)
          | _ -> (id, name)

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
  let mt1 = subst mt1 in
  (* When need a map to keep alias between opaque type to it's coreesponding
     transparent type alias *)
  let alias_map : (I.ty_id * I.ty) list ref = ref [] in
  let rec compatible mt0 mt1 : I.mod_ty =
    match (mt0, mt1) with
    | ( I.MTMod
          {
            val_defs = vds0;
            constr_defs = cds0;
            ty_defs = tds0;
            mod_defs = mds0;
            mod_sigs = _ms0;
            id = id0;
            _;
          },
        I.MTMod
          {
            val_defs = vds1;
            constr_defs = cds1;
            ty_defs = tds1;
            mod_defs = mds1;
            mod_sigs = ms1;
            _;
          } ) ->
        I.MTMod
          {
            ty_defs =
              (List.iter
                 (fun td1 ->
                   match td1 with
                   | I.TDOpaqueI (name, paras) -> (
                       let td0 = I.get_def name tds0 in
                       match td0 with
                       | I.TDOpaqueI (_, paras0)
                       | I.TDAdtI (_, paras0, _)
                       | I.TDRecordI (_, paras0, _) ->
                           if List.length paras0 <> List.length paras then
                             failwith
                               "number of type parameter not compatible in \
                                opaque type"
                       | I.TDAliasI (_, ty0) -> (
                           match paras with
                           | [] ->
                               alias_map := ((id0, name), ty0) :: !alias_map
                           | _ :: _ -> failwith "type alias has parameter"))
                   | _ ->
                       let td0 = I.get_def (I.get_def_name td1) tds0 in
                       if td0 <> Alias.dealias_td td1 !alias_map then
                         failwith "a type def component not compatible")
                 tds1;
               tds1);
            val_defs =
              (List.iter
                 (fun (name, vt1) ->
                   let vt0 = List.assoc name vds0 in
                   if
                     align_inst vt0
                     <> align_inst (Alias.dealias vt1 !alias_map)
                   then Report.in_compatible_error name vt0 vt1)
                 vds1;
               vds1);
            constr_defs =
              (List.iter
                 (fun (name, (cd1, cid1)) ->
                   let cd0, cid0 = List.assoc name cds0 in
                   if
                     cid1 <> cid0
                     || align_inst cd0
                        <> align_inst (Alias.dealias cd1 !alias_map)
                   then
                     failwith
                       (Printf.sprintf
                          "a constructor component `%s` not compatible" name))
                 cds1;
               cds1);
            mod_defs =
              List.map
                (fun (name, md1) ->
                  (name, compatible (List.assoc name mds0) md1))
                mds1;
            mod_sigs =
              List.map
                (fun (name, ms1) ->
                  (name, compatible (List.assoc name mds0) ms1))
                ms1;
            id = id0;
            owned_mods = [];
          }
    | I.MTFun (argt0, mt0), I.MTFun (argt1, mt1) ->
        let arg_t = compatible argt1 argt0 in
        let ret_t = compatible mt0 mt1 in
        I.MTFun (arg_t, ret_t)
    | _ -> failwith "subtype check error"
  in
  (compatible mt0 mt1, subst)

and normalize_def (t : T.ety_def) env : I.ty_def =
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
        I.TDAdtI (n, tvs, vs)
    | T.TDRecord (n, tvs, fields) ->
        I.TDRecordI
          (n, tvs, List.map (fun (x, t) -> (x, normalize t Type env)) fields)
    | T.TDAlias (n, te) -> I.TDAliasI (n, normalize te Type env)
  in
  normed

and normalize_ty t env = normalize t Let env

and normalize (t : T.ety) (ctx : norm_ctx) (env : Env.t) : I.ty =
  match t with
  | T.TField (me, n, tes) -> (
      let tes = List.map (fun te -> normalize te ctx env) tes in
      let me_typed = tc_mod me env in
      let mod_ty = get_mod_ty me_typed in
      match mod_ty with
      | I.MTMod { id; ty_defs; _ } -> (
          match I.get_def n ty_defs with
          | I.TDOpaqueI (_, _)
          | I.TDAdtI (_, _, _)
          | I.TDRecordI (_, _, _) ->
              TConsI ((id, n), tes)
          | I.TDAliasI (_, te) -> (
              match tes with
              | [] -> te
              | _ :: _ ->
                  failwith "try to provide type parameter to a type alias"))
      | I.MTFun _ -> failwith "try get a field from functor")
  | T.TCons (c, tes) -> (
      let id, def = Env.get_type_def c env in
      match def with
      | I.TDOpaqueI (_, _)
      | I.TDAdtI (_, _, _)
      | I.TDRecordI (_, _, _) ->
          TConsI ((id, c), List.map (fun t -> normalize t ctx env) tes)
      | I.TDAliasI (_, te) -> (
          match tes with
          | [] -> te
          | _ -> failwith "apply type to type alias"))
  | T.TVar x -> (
      match ctx with
      | Type -> TQVarI x
      | Let -> pool_make_tv x)
  | T.TArrow (t0, t1) -> TArrowI (normalize t0 ctx env, normalize t1 ctx env)
  | T.TTuple ts -> TTupleI (List.map (fun t -> normalize t ctx env) ts)
  | T.TRecord fields ->
      TRecordI (List.map (fun (x, t) -> (x, normalize t ctx env)) fields)

and normalize_mt (me : T.emod_ty) env : I.mod_ty =
  match me with
  | T.MTName name -> Env.get_module_sig name env
  | T.MTField (me, name) -> (
      let me_typed = tc_mod me env in
      let mt = get_mod_ty me_typed in
      match mt with
      | I.MTMod mt -> List.assoc name mt.mod_sigs
      | I.MTFun (_mt0, _mt1) -> failwith "try get field from functor")
  | T.MTSig comps ->
      let env' = normalize_msig comps (Env.enter_env env) in
      let scope = absorb_history env' env in
      make_scope_mt scope
  | T.MTFunctor (m0, emt0, m1) ->
      let mt0 = normalize_mt emt0 env in
      let mt1 = normalize_mt m1 (Env.add_module m0 mt0 env) in
      MTFun (mt0, mt1)

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
            Env.add_type_def (TDOpaqueI (name, paras)) env
        | T.TManiTySpec def -> Env.add_type_def (normalize_def def env) env
        | T.TModSpec (name, emt) ->
            let mt = normalize_mt emt env in
            Env.add_module name mt env
      in
      normalize_msig comps env
