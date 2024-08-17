open Typedtree
module U = Unify
module T = Syntax.Parsetree
module I = Types_in
module P = Poly
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
    | T.Exp_const c -> check_const c
    | T.Exp_var x -> check_var x env
    | T.Exp_let (x, e0, e1) -> check_let x e0 e1 env
    | T.Exp_letrec (binds, body) -> check_letrec binds body env
    | T.Exp_lam (para, body) -> check_lambda para body env
    | T.Exp_if (c, e0, e1) -> check_if_else c e0 e1 env
    | T.Exp_case (e, bs) -> check_cases e bs env
    | T.Exp_app (e0, e1) -> check_app e0 e1 env
    | T.Exp_ann (e, te) -> check_ann e te env
    | T.Exp_tuple es -> check_tuple es env
    | T.Exp_field (p, x) -> check_field p x env
    | T.Exp_constr c -> check_cons c env
    | T.Exp_field_constr (p, c) -> check_field_cons p c env
    | T.Exp_cmp (op, e0, e1) -> check_cmp op e0 e1 env
    | T.Exp_seq (e0, e1) -> check_seq e0 e1 env
  with
  | U.UnificationError (t0, t1) ->
      Report.unification_error t0 t1 e.start_loc e.end_loc env
  | U.OccurError (tv, te) -> Report.occur_error tv te e.start_loc e.end_loc
  | e -> raise e

and check_const c =
  match c with
  | T.Const_bool _ -> Exp_const (c, I.bool_ty)
  | T.Const_int _ -> Exp_const (c, I.int_ty)
  | T.Const_string _ -> Exp_const (c, I.string_ty)
  | T.Const_unit -> Exp_const (c, I.unit_ty)

and check_var x env =
  (* lookup a binding won't unify anything *)
  let bind = Env.lookup_var_type x env in
  let t = P.inst bind in
  Exp_var (x, t)

(* pattern will create bindings under context's type *)
and check_pattern p te env : pattern * (string * I.ty) list =
  let check_PCons_aux (cons_ty : I.ty) (p (* payload pattern *) : T.pattern)
      te0 =
    match cons_ty with
    | I.Ty_arrow (pay_ty (* payload type *), te1) ->
        U.unify te1 te0;
        let p, vars = check_pattern p pay_ty env in
        (p, vars)
    | _ -> failwith "payload constructor is not arrow type"
  in
  match (p, te) with
  | T.Pat_var x, te -> (Pat_var (x, te), [ (x, te) ])
  | T.Pat_constr (c, None), te -> (
      let cons_ty_gen (* type of constructor *), id =
        Env.lookup_constr_type c env
      in
      let cons_ty = P.inst cons_ty_gen in
      U.unify cons_ty te;
      match cons_ty with
      | I.Ty_cons _ -> (Pat_constr (c, id, None), [])
      | _ ->
          failwith
            (Printf.sprintf "wrong no-payload constructor pattern %s" c))
  | T.Pat_constr (c, Some p0 (* pattern *)), te ->
      let cons_ty_gen (* type of constructor *), id =
        Env.lookup_constr_type c env
      in
      let p0, binds = check_PCons_aux (P.inst cons_ty_gen) p0 te in
      (Pat_constr (c, id, Some p0), binds)
  | T.Pat_field_constr (me, c, None), te -> (
      let cons_typed (* constructor *) = check_field_cons me c env in
      let[@warning "-8"] (Exp_field_constr (_, _, id, _)) = cons_typed in
      let cons_ty = get_ty cons_typed in
      U.unify cons_ty te;
      (* unify cons_ty with te *)
      match cons_ty with
      | I.Ty_cons (_, _) -> (Pat_constr (c, id, None), [ (* bind nothing *) ])
      | _ -> failwith "wrong type")
  | T.Pat_field_constr (p (* path *), c, Some p0), te ->
      let cons_typed (* typed constructor *) = check_field_cons p c env in
      let[@warning "-8"] (Exp_field_constr (_, _, id, cons_ty)) =
        cons_typed
      in
      let p0, binds = check_PCons_aux cons_ty p0 te in
      (Pat_constr (c, id, Some p0), binds)
  | T.Pat_val v, te ->
      let v_typed = check_const v in
      U.unify (get_ty v_typed) te;
      (Pat_val v, [])
  | T.Pat_tuple pats, te ->
      let payload_tvs = List.map (fun _ -> P.make_tv ()) pats in
      U.unify te (I.Ty_tuple payload_tvs);
      let pats, vars =
        List.fold_left2
          (fun (pats_acc, vars_acc) pat te ->
            let pat, vars = check_pattern pat te env in
            (pats_acc @ [ pat ], vars_acc @ vars))
          ([], []) pats payload_tvs
      in
      (Pat_tuple pats, vars)

and check_let x e0 e1 env =
  let e0_typed, env = check_let_binding x e0 env in
  let e1_typed = check_expr e1 env in
  Exp_let (x, e0_typed, e1_typed, get_ty e1_typed)

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
  Exp_letrec (List.combine vars lams_typed, body_typed, get_ty body_typed)

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
        | Exp_lam (x, body, ty) -> (x, body, ty)
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
  | T.Para_ann (x, t) ->
      let ann = normalize_ty t env0 in
      let env = Env.add_value x ([], ann) env0 in
      let body0 = check_expr body env in
      let body_ty0 = get_ty body0 in
      Exp_lam (x, body0, I.Ty_arrow (ann, body_ty0))
  | T.Para_bare x ->
      let tv = P.make_tv () in
      let env = Env.add_value x ([], tv) env0 in
      let body0 = check_expr body env in
      let body_ty0 = get_ty body0 in
      Exp_lam (x, body0, I.Ty_arrow (tv, body_ty0))

and check_if_else c e1 e2 env : expr =
  let c_typed = check_expr c env in
  U.unify (get_ty c_typed) I.bool_ty;
  let e1_typed = check_expr e1 env in
  let e2_typed = check_expr e2 env in
  U.unify (get_ty e1_typed) (get_ty e2_typed);
  Exp_if (c_typed, e1_typed, e2_typed, get_ty e1_typed)

and check_app op arg env =
  let op_typed = check_expr op env in
  let op_ty = get_ty op_typed in
  let arg_typed = check_expr arg env in
  let arg_ty = get_ty arg_typed in
  let tv = P.make_tv_of "'ret" in
  U.unify op_ty (I.Ty_arrow (arg_ty, tv));
  Exp_app (op_typed, arg_typed, tv)

and check_cmp op e0 e1 env =
  let e0_typed = check_expr e0 env in
  let e1_typed = check_expr e1 env in
  U.unify (get_ty e0_typed) (get_ty e1_typed);
  Exp_cmp (op, e0_typed, e1_typed, I.bool_ty)

and check_seq e0 e1 env =
  let e0_typed = check_expr e0 env in
  U.unify (get_ty e0_typed) I.unit_ty;
  let e1_typed = check_expr e1 env in
  let e1_ty = get_ty e1_typed in
  Exp_seq (e0_typed, e1_typed, e1_ty)

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
  Exp_case (e_typed, bs_typed, res_ty)

and check_tuple es env =
  let es_typed =
    List.fold_left
      (fun acc e ->
        let e_typed = check_expr e env in
        acc @ [ e_typed ])
      [] es
  in
  let tu_te = I.Ty_tuple (List.map get_ty es_typed) in
  Exp_tuple (es_typed, tu_te)

and check_cons c env =
  let t, id = Env.lookup_constr_type c env in
  Exp_constr (c, id, P.inst t)

and check_field_cons me c env =
  let me_typed = check_mod me env in
  match get_mod_ty me_typed with
  | I.Mod_ty_struct { constr_defs; _ } ->
      let t, id = List.assoc c constr_defs in
      Exp_field_constr (me_typed, c, id, P.inst t)
  | I.Mod_ty_functor _ -> failwith "try get field from functor"

and check_field me x env =
  let me_typed = check_mod me env in
  match get_mod_ty me_typed with
  | I.Mod_ty_struct { val_defs; _ } ->
      Exp_field (me_typed, x, P.inst (List.assoc x val_defs))
  | I.Mod_ty_functor _ -> failwith "try get field from functor"

and check_ann e te env =
  let e_typed = check_expr e env in
  let te = normalize_ty te env in
  U.unify te (get_ty e_typed);
  e_typed

(* typing top levels *)
and check_top_level (top : T.top_level) env : top_level * Env.t =
  let old_pool = !tv_pool in
  reset_pool ();
  let top_typed =
    match top with
    | T.Top_let (x, e) ->
        let e_typed0, env = check_let_binding x e env in
        (Top_let (x, e_typed0), env)
    | T.Top_letrec binds ->
        let env, vars, lams = check_letrec_binding binds env in
        let binds = List.combine vars lams in
        (Top_letrec binds, env)
    | T.Top_type_def (Ty_def_adt (name, ty_para_names, _) as def_ext) ->
        let tid = Env.mk_tid name env in

        let normalize_env =
          (* special environment for normalizing type definition, with typed
             definition pushed as an opaque type *)
          Env.add_type_def (I.Ty_def_opaque (name, ty_para_names)) env
        in
        let def = normalize_def def_ext normalize_env in
        let[@warning "-8"] (I.Ty_def_adt (_, _, bs)) = def in
        let env = Env.add_type_def def env in
        let constructors = analyze_constructors tid ty_para_names bs in
        let env = Env.add_constrs constructors env in
        (Top_type_def def, env)
    | T.Top_type_def (_ as def_ext) ->
        let def = normalize_def def_ext env in
        (Top_type_def def, Env.add_type_def def env)
    | T.Top_mod (name, me) ->
        let me_typed = check_mod me env in
        ( Top_mod (name, me_typed),
          Env.add_module name (get_mod_ty me_typed) env )
    | T.Top_mod_sig (name, ext_mt) ->
        let mt = normalize_mt ext_mt env in
        (Top_mod_sig (name, mt), Env.add_module_sig name mt env)
    | T.Top_external (name, e_ty, ext_name) ->
        P.enter_level ();
        let te = normalize e_ty Let env in
        P.exit_level ();
        let gen = P.generalize te env in
        (Top_external (name, te, ext_name), Env.add_value name gen env)
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
                I.Ty_cons (tid, List.map (fun id -> I.Ty_qvar id) para_names)
              ),
              id ) )
      | c, Some payload ->
          ( c,
            ( ( para_names,
                I.Ty_arrow
                  ( payload,
                    Ty_cons
                      (tid, List.map (fun id -> I.Ty_qvar id) para_names) )
              ),
              id ) ))
    bs

(* typing program (content of module) *)
and check_top_levels (prog : T.top_level list) env : program * Env.t =
  match prog with
  | [] -> ([], env)
  | top :: rest ->
      let top_typed0, env = check_top_level top env in
      let rest_typed1, env = check_top_levels rest env in
      (top_typed0 :: rest_typed1, env)

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
  I.Mod_ty_struct
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
    match me.desc with
    | T.Mod_name name -> check_mod_name name env
    | T.Mod_struct body -> check_struct body env
    | T.Mod_functor ((name, ext_mt0), me1) ->
        check_functor name ext_mt0 me1 env
    | T.Mod_field (me, name) -> check_mod_field me name env
    | T.Mod_apply (me0, me1) -> check_mod_apply me0 me1 env
    | T.Mod_restrict (me, mt) -> check_mod_restrict me mt env
  in
  Env.try_record_hint me_typed env;
  me_typed

and check_mod_name name env = Mod_name (name, Env.lookup_module_def name env)

and check_struct body env =
  let body_typed, env' = check_top_levels body (Env.enter_env env) in
  let scope = absorb_history env' env in
  let mt = make_mt_by_scope scope in
  Mod_struct (body_typed, mt)

and check_functor name ext_mt0 me1 env =
  let mt0 = normalize_mt ext_mt0 env in
  let me1_typed = check_mod me1 (Env.add_module name mt0 env) in
  Mod_functor ((name, mt0), me1_typed)

and check_mod_field me name env =
  let me_typed = check_mod me env in
  match get_mod_ty me_typed with
  | I.Mod_ty_struct { mod_defs; _ } ->
      Mod_field (me_typed, name, List.assoc name mod_defs)
  | I.Mod_ty_functor _ -> failwith "try get field from functor"

and check_mod_apply me0 me1 env =
  let me0_typed = check_mod me0 env in
  let me1_typed = check_mod me1 env in
  let mt0 = get_mod_ty me0_typed in
  let mt1 = get_mod_ty me1_typed in
  match mt0 with
  | I.Mod_ty_struct _ -> failwith "try apply a structure"
  | I.Mod_ty_functor (para_mt, body_mt) ->
      Mod_apply (me0_typed, me1_typed, apply_functor para_mt body_mt mt1 env)

and check_mod_restrict me mt env =
  let me_typed = check_mod me env in
  let mt = normalize_mt mt env in
  let _subst = check_subtype (get_mod_ty me_typed) mt in
  Mod_restrict (me_typed, mt, shift_mt mt env)

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
      | I.Mod_ty_struct
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
      | I.Mod_ty_functor (_para_mt, _body_mt) ->
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

        method! visit_Mod_ty_struct () id val_defs constr_defs ty_defs
            mod_sigs mod_defs owned_mods =
          let shifted_id = get_id_or_default id in
          super#visit_Mod_ty_struct () shifted_id val_defs constr_defs
            ty_defs mod_sigs mod_defs
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

and normalize_def (t : T.surface_ty_def) env : I.ty_def =
  let normed =
    match t with
    | T.Ty_def_adt (n, tvs, vs) ->
        let vs =
          List.map
            (function
              | c, None -> (c, None)
              | c, Some payload -> (c, Some (normalize payload Type env)))
            vs
        in
        I.Ty_def_adt (n, tvs, vs)
    | T.Ty_def_record (n, tvs, fields) ->
        I.Ty_def_record
          (n, tvs, List.map (fun (x, t) -> (x, normalize t Type env)) fields)
    | T.Ty_def_alias (n, te) -> I.Ty_def_alias (n, normalize te Type env)
  in
  normed

and normalize_ty t env = normalize t Let env

and normalize (t : T.surface_ty) (ctx : norm_ctx) (env : Env.t) : I.ty =
  match t with
  | T.Ty_field (me, n, tes) -> (
      let tes = List.map (fun te -> normalize te ctx env) tes in
      let me_typed = check_mod me env in
      let mod_ty = get_mod_ty me_typed in
      match mod_ty with
      | I.Mod_ty_struct { id; ty_defs; _ } -> (
          match I.get_def n ty_defs with
          | I.Ty_def_opaque (_, _)
          | I.Ty_def_adt (_, _, _)
          | I.Ty_def_record (_, _, _) ->
              Ty_cons ((id, n), tes)
          | I.Ty_def_alias (_, te) -> (
              match tes with
              | [] -> te
              | _ :: _ ->
                  failwith "try to provide type parameter to a type alias"))
      | I.Mod_ty_functor _ -> failwith "try get a field from functor")
  | T.Ty_cons (c, tes) -> (
      let id, def = Env.lookup_type_def c env in
      match def with
      | I.Ty_def_opaque (_, _)
      | I.Ty_def_adt (_, _, _)
      | I.Ty_def_record (_, _, _) ->
          Ty_cons ((id, c), List.map (fun t -> normalize t ctx env) tes)
      | I.Ty_def_alias (_, te) -> (
          match tes with
          | [] -> te
          | _ -> failwith "apply type to type alias"))
  | T.Ty_var x -> (
      match ctx with
      | Type -> Ty_qvar x
      | Let -> pool_make_tv x)
  | T.Ty_arrow (t0, t1) ->
      Ty_arrow (normalize t0 ctx env, normalize t1 ctx env)
  | T.Ty_tuple ts -> Ty_tuple (List.map (fun t -> normalize t ctx env) ts)
  | T.Ty_record fields ->
      Ty_record (List.map (fun (x, t) -> (x, normalize t ctx env)) fields)

and normalize_mt (me : T.surface_mod_ty) env : I.mod_ty =
  match me with
  | T.Mod_ty_name name -> Env.lookup_module_sig name env
  | T.Mod_ty_field (me, name) -> (
      let me_typed = check_mod me env in
      let mt = get_mod_ty me_typed in
      match mt with
      | I.Mod_ty_struct mt -> List.assoc name mt.mod_sigs
      | I.Mod_ty_functor (_mt0, _mt1) ->
          failwith "try get field from functor")
  | T.Mod_ty_sig comps ->
      let env' = normalize_msig comps (Env.enter_env env) in
      let scope = absorb_history env' env in
      make_mt_by_scope scope
  | T.Mod_ty_functor (m0, ext_mt0, m1) ->
      let mt0 = normalize_mt ext_mt0 env in
      let mt1 = normalize_mt m1 (Env.add_module m0 mt0 env) in
      Mod_ty_functor (mt0, mt1)

and normalize_msig comps env =
  match comps with
  | [] -> env
  | comp :: comps ->
      let env =
        match comp with
        | T.Spec_value (name, te) ->
            reset_pool ();
            P.enter_level ();
            let normalized = normalize_ty te env in
            P.exit_level ();
            Env.add_value name (P.generalize normalized env) env
        | T.Spec_abstr (name, paras) ->
            Env.add_type_def (Ty_def_opaque (name, paras)) env
        | T.Spec_mani_ty def ->
            let _, env = check_top_level (T.Top_type_def def) env in
            env
        | T.Spec_mod (name, ext_mt) ->
            let mt = normalize_mt ext_mt env in
            Env.add_module name mt env
      in
      normalize_msig comps env
