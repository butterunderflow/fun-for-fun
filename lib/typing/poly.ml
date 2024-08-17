module I = Types_in

let current_level = ref 0

let enter_level () = current_level := !current_level + 1

let exit_level () = current_level := !current_level - 1

let make_tv () =
  let name = "'_t" in
  I.Ty_var (ref (I.Unbound (Ident.create ~hint:name, !current_level)))

let make_tv_of hint =
  I.Ty_var (ref (I.Unbound (Ident.create ~hint, !current_level)))

let inst_with (t : I.bind_ty) tes : I.ty =
  let qvs, te = t in
  let dict = List.combine qvs tes in
  let rec go te =
    match (te : I.ty) with
    | I.Ty_cons (tid, tes) -> I.Ty_cons (tid, List.map go tes)
    | I.Ty_var { contents = I.Unbound _ } -> te
    | I.Ty_var { contents = I.Link t } -> go t
    | I.Ty_qvar qtv -> List.assoc qtv dict
    | I.Ty_arrow (te0, te1) -> Ty_arrow (go te0, go te1)
    | I.Ty_tuple tes -> Ty_tuple (List.map go tes)
    | I.Ty_record fields ->
        I.Ty_record (List.map (fun (name, te) -> (name, go te)) fields)
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
    List.mapi (fun i _id -> I.Ty_qvar (Ident.mk_ident i "_stable")) qvs
  in
  inst_with t new_tvs

let generalize (t : I.ty) (_env : Env.t) : I.bind_ty =
  let qvs = ref [] in
  let cons_uniq x xs = if List.mem x xs then xs else x :: xs in
  let rec gen (t : I.ty) =
    match t with
    | I.Ty_var { contents = I.Unbound (x, level) } ->
        (* if a type variable not captured by environment, we need to
           generalize it *)
        if level > !current_level then (
          (* if a type variable is created(allocated) in a inner
             scope(region), quantify(release) it *)
          qvs := cons_uniq x !qvs;
          I.Ty_qvar x)
        else t
    | I.Ty_var { contents = I.Link t } -> gen t
    | I.Ty_cons (c, tes) -> I.Ty_cons (c, List.map gen tes)
    | I.Ty_qvar _ -> failwith "neverreach"
    | I.Ty_arrow (t1, t2) -> I.Ty_arrow (gen t1, gen t2)
    | I.Ty_tuple tes -> I.Ty_tuple (List.map gen tes)
    | I.Ty_record fields ->
        I.Ty_record (List.map (fun (name, te) -> (name, gen te)) fields)
  in
  (!qvs, gen t)
