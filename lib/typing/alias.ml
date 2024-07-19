(** Dealias during module compatible checking *)

module I = Types_in

let dealias_te (te : I.ty) alias_map =
  let rec go (t : I.ty) =
    match t with
    | I.TConsI (id, paras) -> (
        match List.assoc_opt id alias_map with
        | Some t' -> (
            match t' with
            | I.TConsI (id', []) -> I.TConsI (id', paras)
            | _ -> failwith "ill form alias")
        | None -> t)
    | I.TVarI { contents = I.Unbound _ } ->
        t (* it's not a neverreach branch *)
    | I.TVarI { contents = I.Link t' } -> go t'
    | I.TQVarI _ -> t
    | I.TArrowI (t0, t1) -> I.TArrowI (go t0, go t1)
    | I.TTupleI tes -> I.TTupleI (List.map go tes)
    | I.TRecordI fields ->
        I.TRecordI (List.map (fun (name, te) -> (name, go te)) fields)
  in
  go te

let dealias ((qvs, te) : I.bind_ty) alias_map = (qvs, dealias_te te alias_map)

let dealias_td (td : I.ty_def) alias_map =
  match td with
  | I.TDOpaqueI (_, _) -> td
  | I.TDAdtI (name, paras, bs) ->
      I.TDAdtI
        ( name,
          paras,
          List.map
            (fun (cname, t) ->
              (cname, Option.map (fun t' -> dealias_te t' alias_map) t))
            bs )
  | I.TDRecordI (name, paras, fields) ->
      I.TDRecordI
        ( name,
          paras,
          List.map (fun (name, t) -> (name, dealias_te t alias_map)) fields
        )
  | I.TDAliasI (name, t) -> I.TDAliasI (name, dealias_te t alias_map)
