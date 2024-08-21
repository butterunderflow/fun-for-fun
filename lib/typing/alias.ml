(** Dealias during module compatible checking *)

module I = Types_in

let dealias_te (te : I.ty) alias_map =
  let rec go (t : I.ty) =
    match t with
    | I.TCons (id, []) -> (
        match List.assoc_opt id alias_map with
        | Some t' -> (
            match t' with
            | I.TCons (id', []) -> I.TCons (id', [])
            | _ -> failwith "ill form alias")
        | None -> t)
    | I.TCons (id, args) -> (
        match List.assoc_opt id alias_map with
        | Some _t' -> failwith "parameterized type alias is not supported"
        | None -> I.TCons (id, List.map go args))
    | I.TVar { contents = I.Unbound _ } ->
        (* It's a 'never reach' when local module is not supported(that's the
           current implementation), because every normalized module type
           should only capture module level entities, which should have
           normalized and fully inferenced already. *)
        failwith "never reach: deliasing a type not fully inferenced"
    | I.TVar { contents = I.Link t' } -> go t'
    | I.TQVar _ -> t
    | I.TArrow (t0, t1) -> I.TArrow (go t0, go t1)
    | I.TTuple tes -> I.TTuple (List.map go tes)
    | I.TRecord fields ->
        I.TRecord (List.map (fun (name, te) -> (name, go te)) fields)
  in
  go te

let dealias ((qvs, te) : I.bind_ty) alias_map = (qvs, dealias_te te alias_map)

let dealias_td (td : I.ty_def) alias_map =
  match td with
  | I.TDOpaque (_, _) -> td
  | I.TDAdt (name, paras, bs) ->
      I.TDAdt
        ( name,
          paras,
          List.map
            (fun (cname, t) ->
              (cname, Option.map (fun t' -> dealias_te t' alias_map) t))
            bs )
  | I.TDRecord (name, paras, fields) ->
      I.TDRecord
        ( name,
          paras,
          List.map (fun (name, t) -> (name, dealias_te t alias_map)) fields
        )
  | I.TDAlias (name, t) -> I.TDAlias (name, dealias_te t alias_map)
