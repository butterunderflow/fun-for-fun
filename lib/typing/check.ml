[@@@warning "-27"]

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
    object (self : 'self)
      inherit ['self] I.map

      method! visit_TQVarI () qtv = List.assoc qtv dict
    end
  in
  mapper#visit_ty () te

let inst (t : I.bind_ty) : I.ty =
  (* We can gaurantee that captured type variables will never duplicated with
     free type variables *)
  let qvs, te = t in
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
    object (self : 'self)
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

let generalize (t : I.ty) env : I.bind_ty =
  let tvs = get_all_tvs t in
  (* get all type variables *)
  let uncaptured_tvs =
    tvs
    |> List.filter (function
         | { contents = I.Unbound x } -> true
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
    | I.TQVarI x -> t
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
      match cons_ty with
      | I.TConsI (_, []) -> (PCons (c, None), [])
      | _ -> failwith "wrong type")
  | T.PFieldCons (p, c, None), te -> (
      let cons_typed (* constructor *) = tc_field_cons p c env in
      let cons_ty = get_ty cons_typed in
      (* todo: fix cons_ty should unify with te, and tc_field_cons's return
         type has problem *)
      match cons_ty with
      | I.TConsI (_, _) -> (PCons (c, None), [])
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
          (* todo *)
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
  let tvs = List.map (fun x -> ([], make_tv ())) binds in
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
      let env = Env.add_value x ([], normalize t env0) env0 in
      let body0 = tc_expr body env in
      let body_ty0 = get_ty body0 in
      ELam (x, body0, I.TArrowI (normalize t env, body_ty0))
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
        (* todo: do some thing like let res = U.apply_expr_untypd u1 res in *)
        (* check result expression *)
        let env =
          List.fold_left
            (fun env (v, ty) ->
              Env.add_value v ([], ty (* no generalize here*)) env)
            env vars
        in
        let res_typed' = tc_expr res env in
        let res_ty' = get_ty res_typed' in
        (* unify checked result type with current result type *)
        U.unify res_ty' res_ty;
        bs_typed @ [ (p, res_typed') ])
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

and tc_field_cons p c env = failwith "todo"

and tc_field p x env = failwith "todo"

and tc_ann e te env =
  let e_typed = tc_expr e env in
  let te = normalize te env in
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
        let e_ty0 = get_ty e_typed0 in
        (TopLet (x, e_typed0, e_ty0), env)
    | T.TopLetRec binds ->
        let env, vars, lams = tc_letrec_binding binds env in
        let binds = List.combine vars lams in
        (TopLetRec binds, env)
    | T.TopTypeDef (TDAdt (name, ty_para_names, bs) as def_ext) ->
        let tid = Env.mk_tid name env in
        let def = normalize_def def_ext env in
        let[@warning "-8"] (I.TDAdtI (_, _, bs)) = def in
        let env = Env.add_type_def def env in
        let constructors = analyze_constructors tid ty_para_names bs in
        let env = { env with values = constructors @ env.values } in
        (TopTypeDef def, env)
    | T.TopTypeDef (TDRecord (_, _, _) as def_ext) ->
        let def = normalize_def def_ext env in
        (TopTypeDef def, Env.add_type_def def env)
    | T.TopMod (_, _) -> failwith "todo"
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
and tc_program (prog : T.program) env : program * Env.t =
  match prog with
  | [] -> ([], env)
  | top :: rest ->
      let top_typed0, env = tc_toplevel top env in
      let rest_typed1, env = tc_program rest env in
      (top_typed0 :: rest_typed1, env)

and normalize_ty (t : T.ety) indef (env : Env.t) : I.ty =
  let { Env.curr; _ } = env in
  match t with
  | T.TField (_, _, _) -> failwith "todo"
  | T.TCons (c, tes) ->
      TConsI ((curr, c), List.map (fun t -> normalize_ty t indef env) tes)
  | T.TVar x -> if indef then I.TQVarI x else pool_make_tv x
  | T.TArrow (t0, t1) ->
      TArrowI (normalize_ty t0 indef env, normalize_ty t1 indef env)
  | T.TTuple ts -> TTupleI (List.map (fun t -> normalize_ty t indef env) ts)
  | T.TRecord fields ->
      TRecordI
        (List.map (fun (x, t) -> (x, normalize_ty t indef env)) fields)
  | T.TInternal ti -> ti

and normalize_def (t : T.ety_def) env : I.ty_def =
  let normed =
    match t with
    | T.TDAdt (n, tvs, vs) ->
        let vs =
          List.map
            (function
              | c, None -> (c, None)
              | c, Some payload -> (c, Some (normalize_ty payload true env)))
            vs
        in
        I.TDAdtI (n, tvs, vs)
    | T.TDRecord (n, tvs, fields) ->
        I.TDRecordI
          ( n,
            tvs,
            List.map (fun (x, t) -> (x, normalize_ty t true env)) fields )
  in
  normed

and normalize t env = normalize_ty t false env