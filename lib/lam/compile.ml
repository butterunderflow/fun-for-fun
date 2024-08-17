module T = Typing.Typedtree
module L = Tree

let rec compile_expr (e : T.expr) =
  match e with
  | T.Exp_const (c, _) -> L.Exp_const c
  | T.Exp_var (x, _) -> L.Exp_var x
  | T.Exp_let (x, e0, e1, _) ->
      L.Exp_let (x, compile_expr e0, compile_expr e1)
  | T.Exp_letrec (lams, e, _) ->
      L.Exp_letrec
        (List.map (fun (x, lam) -> (x, compile_lam lam)) lams, compile_expr e)
  | T.Exp_lam lam -> L.Exp_lam (compile_lam lam)
  | T.Exp_if (e0, e1, e2, _) ->
      L.Exp_if (compile_expr e0, compile_expr e1, compile_expr e2)
  | T.Exp_case (e0, bs, _) ->
      L.Exp_switch (compile_expr e0, List.map compile_branch bs)
  | T.Exp_app (e0, e1, _) -> L.Exp_app (compile_expr e0, [ compile_expr e1 ])
  | T.Exp_ann (e, _) -> compile_expr e
  | T.Exp_tuple (es, _) -> L.Exp_tuple (List.map compile_expr es)
  | T.Exp_field (me, name, _) -> L.Exp_field (compile_mod_expr me, name)
  | T.Exp_constr (_, id, Typing.Types_in.Ty_arrow (_, _)) ->
      L.Exp_payload_constr id
  | T.Exp_constr (_, id, _) -> L.Exp_constr id
  | T.Exp_field_constr (_, _, id, Typing.Types_in.Ty_arrow (_, _)) ->
      L.Exp_payload_constr id
  | T.Exp_field_constr (_, _, id, _) -> L.Exp_constr id
  | T.Exp_cmp (op, e1, e2, _) ->
      L.Exp_cmp (op, compile_expr e1, compile_expr e2)
  | T.Exp_seq (e0, e1, _) -> L.Exp_seq (compile_expr e0, compile_expr e1)

and compile_lam (x, e, _) = ([ x ], compile_expr e, ref [])

and compile_branch (p, e) : L.branch =
  let compile_pattern p =
    let rec go p =
      match p with
      | T.Pat_val c -> L.Pat_val c
      | T.Pat_constr (_cname, id, None) -> L.Pat_constr (id, None)
      | T.Pat_constr (_cname, id, Some p') -> L.Pat_constr (id, Some (go p'))
      | T.Pat_var (x, _) -> L.Pat_var x
      | T.Pat_tuple ps -> L.Pat_tuple (List.map go ps)
    in
    go p
  in
  (compile_pattern p, compile_expr e)

and compile_mod_expr me =
  match me with
  | T.Mod_name (x, _) -> L.Exp_var x
  | T.Mod_struct (tops, _) -> compile_top_levels tops
  | T.Mod_functor ((x, _), body) ->
      L.Exp_lam ([ x ], compile_mod_expr body, ref [])
  | T.Mod_field (me, name, _) -> L.Exp_field (compile_mod_expr me, name)
  | T.Mod_apply (me0, me1, _) ->
      L.Exp_app (compile_mod_expr me0, [ compile_mod_expr me1 ])
  | T.Mod_restrict (me', _, _) -> compile_mod_expr me'

and compile_top_levels tops =
  L.Exp_mod_obj
    (List.filter_map
       (fun (top : T.top_level) ->
         match top with
         | T.Top_let (x, e) -> Some (L.Field_simple (x, compile_expr e))
         | T.Top_letrec binds ->
             Some
               (L.Field_letrec
                  (List.map (fun (x, lam) -> (x, compile_lam lam)) binds))
         | T.Top_type_def _ -> None
         | T.Top_mod (x, me) ->
             Some (L.Field_simple (x, compile_mod_expr me))
         | T.Top_mod_sig (_, _) -> None
         | T.Top_external (name, _, ext_name) ->
             Some (L.Field_simple (name, L.Exp_external ext_name)))
       tops)

let capture fvs vars = List.filter (fun x -> not (List.mem x vars)) fvs

let get_pat_vars (p : L.pattern) =
  let all = ref [] in
  let rec go p =
    match p with
    | L.Pat_var x -> all := x :: !all
    | L.Pat_val _ -> ()
    | L.Pat_constr (_, None) -> ()
    | L.Pat_constr (_, Some p) -> go p
    | L.Pat_tuple ps -> List.iter go ps
  in
  go p;
  !all

let rec fva_expr e vars =
  match e with
  | L.Exp_tuple es ->
      List.fold_left (fun acc e -> fva_expr e vars @ acc) [] es
  | L.Exp_mod_obj mems ->
      let capture_vars = ref [] in
      let vars = ref vars in
      List.fold_left
        (fun acc mem ->
          match mem with
          | L.Field_simple (name, e) ->
              let fv_binds =
                capture (fva_expr e !vars @ acc) !capture_vars
              in
              vars := name :: !vars;
              capture_vars := name :: !capture_vars;
              fv_binds
          | L.Field_letrec binds ->
              let xs, _ = List.split binds in
              capture_vars := xs @ !capture_vars;
              let fvs_in_binds = fva_letrec binds !vars in
              let fvs = capture fvs_in_binds !capture_vars in
              vars := xs @ !vars;
              fvs @ acc)
        [] mems
  | L.Exp_struct fields ->
      List.fold_left (fun acc (_, e) -> acc @ fva_expr e vars) [] fields
  | L.Exp_var x' ->
      assert (List.mem x' vars);
      [ x' ]
  | L.Exp_external _x' -> []
  | L.Exp_constr _ -> []
  | L.Exp_payload_constr _ -> []
  | L.Exp_const _ -> []
  | L.Exp_app (e0, e1s) ->
      fva_expr e0 vars @ List.concat_map (fun e1 -> fva_expr e1 vars) e1s
  | L.Exp_cmp (_, e0, e1) -> fva_expr e0 vars @ fva_expr e1 vars
  | L.Exp_seq (e0, e1) -> fva_expr e0 vars @ fva_expr e1 vars
  | L.Exp_switch (e0, bs) ->
      fva_expr e0 vars
      @ (bs
        |> List.map (fun (p, e) ->
               let p_vars = get_pat_vars p in
               capture (fva_expr e (p_vars @ vars)) p_vars)
        |> List.flatten)
  | L.Exp_let (x', e0, e1) ->
      fva_expr e0 vars @ capture (fva_expr e1 (x' :: vars)) [ x' ]
  | L.Exp_if (e0, e1, e2) ->
      fva_expr e0 vars @ fva_expr e1 vars @ fva_expr e2 vars
  | L.Exp_lam (para, e, fvs) ->
      let fvs' = fva_lambda para e vars in
      fvs := List_utils.remove_from_left fvs';
      fvs'
  | L.Exp_letrec (binds, e) ->
      let xs, _ = List.split binds in
      let fvs_in_binds = fva_letrec binds vars in
      capture fvs_in_binds xs @ capture (fva_expr e (xs @ vars)) xs
  | L.Exp_field (e, _) -> fva_expr e vars

and fva_lambda x e vars =
  let vars = x @ vars in
  capture (fva_expr e vars) x

and fva_letrec binds vars =
  let xs, _ = List.split binds in
  let vars = xs @ vars in
  let fv_binds =
    binds
    |> List.map (fun (_x, (para, e, fvs)) ->
           let fvs' = fva_lambda para e vars in
           fvs := List_utils.remove_from_left fvs';
           fvs')
    |> List.flatten
  in
  fv_binds

(* analyze free variables, write free variables to lambda expression *)
let free_var_analyze e : unit =
  let fvs = fva_expr e [] in
  assert (fvs = [])

let compile_program program =
  let lam_prog = compile_top_levels program in
  free_var_analyze lam_prog;
  lam_prog
