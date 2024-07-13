module C = Closure
module L = Lam.Tree

let rec lift ?(hint = "temp") (e : L.expr) (vars : string list) :
    C.expr * C.func list =
  match e with
  | L.ETuple es ->
      es
      |> List.map (fun e -> lift e vars ~hint)
      |> List.split
      |> fun (es, fns) -> (C.ETuple es, List.flatten fns)
  | L.EModObject mems ->
      let _, mems, fns =
        List.fold_left
          (fun (vars, mem_acc, fn_acc) mem ->
            match mem with
            | L.FSimple (x, e) ->
                let e, fns = lift ~hint:x e vars in
                (x :: vars, C.FSimple (x, e) :: mem_acc, fns @ fn_acc)
            | L.FLetRec binds ->
                let xs, _ = List.split binds in
                let binds, fns = lift_letrec binds vars in
                (xs @ vars, C.FLetRec binds :: mem_acc, fns @ fn_acc))
          (vars, [], []) mems
      in
      (C.EModObject (List.rev mems), fns)
  | L.EStruct mems ->
      let mems, fns =
        List.fold_left
          (fun (acc_mems, acc_fns) (name, e) ->
            let e, fns = lift e vars ~hint in
            ((name, e) :: acc_mems, fns @ acc_fns))
          ([], []) mems
      in
      let mems = List.rev mems in
      (C.EStruct mems, fns)
  | L.EVar x ->
      assert (List.mem x vars);
      (C.EVar x, [])
  | L.EExt x -> (C.EExt x, [])
  | L.ECons i -> (C.ECons i, [])
  | L.EConsWith i -> (C.EConsWith i, [])
  | L.EConst c -> (C.EConst c, [])
  | L.EApp (e0, e1s) ->
      let e0, fns0 = lift e0 vars in
      let e1s, fns1 = List.(split (map (fun e1 -> lift e1 vars) e1s)) in
      let fns1 = List.flatten fns1 in
      (C.EApp (e0, e1s), fns0 @ fns1)
  | L.ECmp (op, e0, e1) ->
      let e0, fns0 = lift e0 vars in
      let e1, fns1 = lift e1 vars in
      (C.ECmp (op, e0, e1), fns0 @ fns1)
  | L.ESeq (e0, e1) ->
      let e0, fns0 = lift e0 vars in
      let e1, fns1 = lift e1 vars in
      (C.ESeq (e0, e1), fns0 @ fns1)
  | L.ESwitch (e0, bs) ->
      let e0, fns0 = lift e0 vars in
      let es, fns1 =
        bs
        |> List.map (fun (p, e) ->
               lift e (Lam__Compile.get_pat_vars p @ vars))
        |> List.split
        |> fun (e, fns) -> (e, List.flatten fns)
      in
      let ps, _ = List.split bs in
      (C.ESwitch (e0, List.combine ps es), fns0 @ fns1)
  | L.ELet (x, e0, e1) ->
      let e0, fns0 = lift ~hint:x e0 vars in
      let e1, fns1 = lift e1 (x :: vars) ~hint in
      (C.ELet (x, e0, e1), fns0 @ fns1)
  | L.EIf (e0, e1, e2) ->
      let e0, fns0 = lift ~hint e0 vars in
      let e1, fns1 = lift ~hint e1 vars in
      let e2, fns2 = lift ~hint e2 vars in
      (C.EIf (e0, e1, e2), fns0 @ fns1 @ fns2)
  | L.ELam (xs, e, fvs) ->
      let fn_id = Ident.create ~hint in
      let e', fns = lift e (xs @ vars) ~hint in
      let new_fn = (fn_id, !fvs, xs, e') in
      (C.EClosure (!fvs, fn_id), new_fn :: fns)
  | L.ELetRec (binds, e) ->
      let xs, _ = List.split binds in
      let cls, fns = lift_letrec binds vars in
      let e, fns' = lift e (xs @ vars) ~hint in
      (C.ELetRec (cls, e), fns' @ fns)
  | L.EField (e, name) ->
      let e', fns = lift e vars ~hint in
      (C.EField (e', name), fns)

and lift_letrec binds vars =
  let xs = List.map fst binds in
  let vars = xs @ vars in
  let fvs =
    binds
    |> List.map snd
    |> List.map (fun (_x, _e, fvs) -> !fvs)
    |> List.flatten
    |> List_utils.remove_from_left
  in
  let captures = xs @ fvs in
  let cls, fns =
    binds
    |> List.map (fun (x, (paras, e, _fvs)) ->
           let e', fns = lift e (paras @ vars) ~hint:x in
           let fn_id = Ident.create ~hint:x in
           let new_fn = (fn_id, captures, paras, e') in
           (fn_id, new_fn :: fns))
    |> List.split
    |> fun (fn_id, fns_l) -> (fn_id, List.flatten fns_l)
  in
  ((captures, List.combine xs cls), fns)

let lift e =
  Ident.refresh ();
  lift e []
