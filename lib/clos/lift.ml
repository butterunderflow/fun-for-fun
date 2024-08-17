module C = Closure
module L = Lam.Tree

let rec lift ?(hint = "temp") (e : L.expr) (vars : string list) :
    C.expr * C.func list =
  match e with
  | L.Exp_tuple es ->
      es
      |> List.map (fun e -> lift e vars ~hint)
      |> List.split
      |> fun (es, fns) -> (C.Exp_tuple es, List.flatten fns)
  | L.Exp_mod_obj mems ->
      let _, mems, fns =
        List.fold_left
          (fun (vars, mem_acc, fn_acc) mem ->
            match mem with
            | L.Field_simple (x, e) ->
                let e, fns = lift ~hint:x e vars in
                (x :: vars, C.Field_simple (x, e) :: mem_acc, fns @ fn_acc)
            | L.Field_letrec binds ->
                let xs, _ = List.split binds in
                let binds, fns = lift_letrec binds vars in
                (xs @ vars, C.Field_letrec binds :: mem_acc, fns @ fn_acc))
          (vars, [], []) mems
      in
      (C.Exp_mod_obj (List.rev mems), fns)
  | L.Exp_struct mems ->
      let mems, fns =
        List.fold_left
          (fun (acc_mems, acc_fns) (name, e) ->
            let e, fns = lift e vars ~hint in
            ((name, e) :: acc_mems, fns @ acc_fns))
          ([], []) mems
      in
      let mems = List.rev mems in
      (C.Exp_struct mems, fns)
  | L.Exp_var x ->
      assert (List.mem x vars);
      (C.Exp_var x, [])
  | L.Exp_external x -> (C.Exp_external x, [])
  | L.Exp_constr i -> (C.Exp_constr i, [])
  | L.Exp_payload_constr i -> (C.Exp_payload_constr i, [])
  | L.Exp_const c -> (C.Exp_const c, [])
  | L.Exp_app (e0, e1s) ->
      let e0, fns0 = lift e0 vars in
      let e1s, fns1 = List.(split (map (fun e1 -> lift e1 vars) e1s)) in
      let fns1 = List.flatten fns1 in
      (C.Exp_app (e0, e1s), fns0 @ fns1)
  | L.Exp_cmp (op, e0, e1) ->
      let e0, fns0 = lift e0 vars in
      let e1, fns1 = lift e1 vars in
      (C.Exp_cmp (op, e0, e1), fns0 @ fns1)
  | L.Exp_seq (e0, e1) ->
      let e0, fns0 = lift e0 vars in
      let e1, fns1 = lift e1 vars in
      (C.Exp_seq (e0, e1), fns0 @ fns1)
  | L.Exp_switch (e0, bs) ->
      let e0, fns0 = lift e0 vars in
      let es, fns1 =
        bs
        |> List.map (fun (p, e) ->
               lift e (Lam.Compile.get_pat_vars p @ vars))
        |> List.split
        |> fun (e, fns) -> (e, List.flatten fns)
      in
      let ps, _ = List.split bs in
      (C.Exp_switch (e0, List.combine ps es), fns0 @ fns1)
  | L.Exp_let (x, e0, e1) ->
      let e0, fns0 = lift ~hint:x e0 vars in
      let e1, fns1 = lift e1 (x :: vars) ~hint in
      (C.Exp_let (x, e0, e1), fns0 @ fns1)
  | L.Exp_if (e0, e1, e2) ->
      let e0, fns0 = lift ~hint e0 vars in
      let e1, fns1 = lift ~hint e1 vars in
      let e2, fns2 = lift ~hint e2 vars in
      (C.Exp_if (e0, e1, e2), fns0 @ fns1 @ fns2)
  | L.Exp_lam (xs, e, fvs) ->
      let fn_id = Ident.create ~hint in
      let e', fns = lift e (xs @ vars) ~hint in
      let new_fn = (fn_id, !fvs, xs, e') in
      (C.Exp_closure (!fvs, fn_id), new_fn :: fns)
  | L.Exp_letrec (binds, e) ->
      let xs, _ = List.split binds in
      let cls, fns = lift_letrec binds vars in
      let e, fns' = lift e (xs @ vars) ~hint in
      (C.Exp_letrec (cls, e), fns' @ fns)
  | L.Exp_field (e, name) ->
      let e', fns = lift e vars ~hint in
      (C.Exp_field (e', name), fns)

and lift_letrec binds vars =
  let xs = List.map fst binds in
  let vars = xs @ vars in
  (* collect free variables *)
  let lambda_fvs =
    binds
    |> List.map snd
    |> List.map (fun (_x, _e, fvs) -> !fvs)
    |> List.flatten
    |> List_utils.remove_from_left
  in
  let letrec_fvs = List.filter (fun fv -> not (List.mem fv xs)) lambda_fvs in
  let letrec_clos_fvs = xs @ letrec_fvs in
  let cls, fns =
    binds
    |> List.map (fun (x, (paras, e, _fvs)) ->
           let e', fns = lift e (paras @ vars) ~hint:x in
           let fn_id = Ident.create ~hint:x in
           let new_fn = (fn_id, letrec_clos_fvs, paras, e') in
           (fn_id, new_fn :: fns))
    |> List.split
    |> fun (fn_id, fns_l) -> (fn_id, List.flatten fns_l)
  in
  ((letrec_fvs, List.combine xs cls), fns)

let lift e =
  Ident.refresh ();
  let main = Ident.create ~hint:"main" in
  let main_body, fns = lift e [] in
  (main, (main, [], [], main_body) :: fns)
