module T = Typing.Typedtree
module L = Tree

let rec compile_expr (e : T.expr) =
  match e with
  | T.EConst (c, _) -> L.EConst c
  | T.EVar (x, _) -> L.EVar x
  | T.ELet (x, e0, e1, _) -> L.ELet (x, compile_expr e0, compile_expr e1)
  | T.ELetrec (lams, e, _) ->
      L.ELetRec
        (List.map (fun (x, lam) -> (x, compile_lam lam)) lams, compile_expr e)
  | T.ELam lam -> L.ELam (compile_lam lam)
  | T.EIf (e0, e1, e2, _) ->
      L.EIf (compile_expr e0, compile_expr e1, compile_expr e2)
  | T.ECase (e0, bs, _) ->
      L.ESwitch (compile_expr e0, List.map compile_branch bs)
  | T.EApp (e0, e1, _) -> L.EApp (compile_expr e0, compile_expr e1)
  | T.EAnn (e, _) -> compile_expr e
  | T.ETuple (es, _) -> L.ETuple (List.map compile_expr es)
  | T.EField (me, name, _) -> L.EField (compile_mod_expr me, name)
  | T.ECons (_, id, _) -> L.ECons id
  | T.EFieldCons (_, _, id, _) -> L.ECons id

and compile_lam (x, e, _) = (x, compile_expr e, ref [])

and compile_branch (p, e) : L.branch =
  let compile_pattern p =
    let rec go p =
      match p with
      | T.PVal c -> L.PVal c
      | T.PCons (_cname, id, None) -> L.PCons (id, None)
      | T.PCons (_cname, id, Some p') -> L.PCons (id, Some (go p'))
      | T.PVar (x, _) -> L.PVar x
      | T.PTuple ps -> L.PTuple (List.map go ps)
    in
    go p
  in
  (compile_pattern p, compile_expr e)

and compile_mod_expr me =
  match me with
  | T.MEName (x, _) -> L.EVar x
  | T.MEStruct (tops, _) -> compile_top_levels tops
  | T.MEFunctor ((x, _), body) -> L.ELam (x, compile_mod_expr body, ref [])
  | T.MEField (me, name, _) -> L.EField (compile_mod_expr me, name)
  | T.MEApply (me0, me1, _) ->
      L.EApp (compile_mod_expr me0, compile_mod_expr me1)
  | T.MERestrict (me', _, _) -> compile_mod_expr me'

and compile_top_levels tops =
  L.EModObject
    (List.filter_map
       (fun (top : T.top_level) ->
         match top with
         | T.TopLet (x, e) -> Some (L.FSimple (x, compile_expr e))
         | T.TopLetRec binds ->
             Some
               (L.FLetRec
                  (List.map (fun (x, lam) -> (x, compile_lam lam)) binds))
         | T.TopTypeDef _ -> None
         | T.TopMod (x, me) -> Some (L.FSimple (x, compile_mod_expr me))
         | T.TopModSig (_, _) -> None)
       tops)

(* analyze free variables, write free variables to lambda expression *)
let free_var_analyze e : unit =
  let capture fvs vars = List.filter (fun x -> not (List.mem x vars)) fvs in
  let get_pat_vars (p : L.pattern) =
    let all = ref [] in
    let rec go p =
      match p with
      | L.PVar x -> all := x :: !all
      | L.PVal _ -> ()
      | L.PCons (_, None) -> ()
      | L.PCons (_, Some p) -> go p
      | L.PTuple ps -> List.iter go ps
    in
    go p;
    !all
  in
  (* return free variables of e, write free variables to lambda expressions in e*)
  let rec go e vars =
    match e with
    | L.ETuple es -> List.fold_left (fun acc e -> go e vars @ acc) [] es
    | L.EModObject mems ->
        let capture_vars = ref [] in
        let vars = ref vars in
        List.fold_left
          (fun acc mem ->
            match mem with
            | L.FSimple (name, e) ->
                let fv_binds = capture (go e !vars @ acc) !capture_vars in
                vars := name :: !vars;
                capture_vars := name :: !capture_vars;
                fv_binds
            | L.FLetRec binds ->
                let xs, _ = List.split binds in
                capture_vars := xs @ !capture_vars;
                let fvs = capture (aux_letrec binds !vars) !capture_vars in
                vars := xs @ !vars;
                fvs @ acc)
          [] mems
    | L.EStruct fields ->
        List.fold_left (fun acc (_, e) -> acc @ go e vars) [] fields
    | L.EVar x' ->
        assert (List.mem x' vars);
        [ x' ]
    | L.ECons _ -> []
    | L.EConst _ -> []
    | L.EApp (e0, e1) -> go e0 vars @ go e1 vars
    | L.ESwitch (e0, bs) ->
        go e0 vars
        @ (bs
          |> List.map (fun (p, e) ->
                 let p_vars = get_pat_vars p in
                 capture (go e (p_vars @ vars)) p_vars)
          |> List.flatten)
    | L.ELet (x', e0, e1) -> go e0 vars @ capture (go e1 (x' :: vars)) [ x' ]
    | L.EIf (e0, e1, e2) -> go e0 vars @ go e1 vars @ go e2 vars
    | L.ELam (para, e, fvs) ->
        let fvs' = aux_lambda para e vars in
        fvs := fvs';
        fvs'
    | L.ELetRec (binds, e) ->
        let xs, _ = List.split binds in
        let fv_binds = aux_letrec binds vars in
        fv_binds @ capture (go e (xs @ vars)) xs
    | L.EField (e, _) -> go e vars
  and aux_lambda x e vars =
    let vars = x :: vars in
    capture (go e vars) [ x ] |> List_utils.remove_from_left
  and aux_letrec binds vars =
    let xs, _ = List.split binds in
    let vars = xs @ vars in
    let fv_binds =
      binds
      |> List.map (fun (_x, (para, e, fvs)) ->
             let fvs' = capture (aux_lambda para e vars) xs in
             fvs := fvs';
             fvs')
      |> List.flatten
    in
    fv_binds
  in
  let fvs = go e [] in
  assert (fvs = [])

let compile_program program =
  let lam_prog = compile_top_levels program in
  free_var_analyze lam_prog;
  lam_prog
