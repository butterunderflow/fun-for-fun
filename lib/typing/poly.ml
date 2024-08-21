module I = Types_in

let current_level = ref 0

let enter_level () = current_level := !current_level + 1

let exit_level () = current_level := !current_level - 1

let make_tv () =
  let name = "'_t" in
  I.TVar (ref (I.Unbound (Ident.create ~hint:name, !current_level)))

let make_tv_of hint =
  I.TVar (ref (I.Unbound (Ident.create ~hint, !current_level)))

let inst_with (t : I.bind_ty) tes : I.ty =
  let qvs, te = t in
  let dict = List.combine qvs tes in
  let rec go te =
    match (te : I.ty) with
    | I.TCons (tid, tes) -> I.TCons (tid, List.map go tes)
    | I.TVar { contents = I.Unbound _ } -> te
    | I.TVar { contents = I.Link t } -> go t
    | I.TQVar qtv -> List.assoc qtv dict
    | I.TArrow (te0, te1) -> TArrow (go te0, go te1)
    | I.TTuple tes -> TTuple (List.map go tes)
    | I.TRecord fields ->
        I.TRecord (List.map (fun (name, te) -> (name, go te)) fields)
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
    List.mapi (fun i _id -> I.TQVar (Ident.mk_ident i "_stable")) qvs
  in
  inst_with t new_tvs

let generalize (t : I.ty) (_env : Env.t) : I.bind_ty =
  let qvs = ref [] in
  let cons_uniq x xs = if List.mem x xs then xs else x :: xs in
  let rec gen (t : I.ty) =
    match t with
    | I.TVar { contents = I.Unbound (x, level) } ->
        (* if a type variable not captured by environment, we need to
           generalize it *)
        if level > !current_level then (
          (* if a type variable is created(allocated) in a inner
             scope(region), quantify(release) it *)
          qvs := cons_uniq x !qvs;
          I.TQVar x)
        else t
    | I.TVar { contents = I.Link t } -> gen t
    | I.TCons (c, tes) -> I.TCons (c, List.map gen tes)
    | I.TQVar _ -> failwith "neverreach"
    | I.TArrow (t1, t2) -> I.TArrow (gen t1, gen t2)
    | I.TTuple tes -> I.TTuple (List.map gen tes)
    | I.TRecord fields ->
        I.TRecord (List.map (fun (name, te) -> (name, gen te)) fields)
  in
  (!qvs, gen t)
