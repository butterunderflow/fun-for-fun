module I = Types_in

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
