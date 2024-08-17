(** Dealias during module compatible checking *)

module I = Types_in

let dealias_te (te : I.ty) alias_map =
  let rec go (t : I.ty) =
    match t with
    | I.Ty_cons (id, []) -> (
        match List.assoc_opt id alias_map with
        | Some t' -> (
            match t' with
            | I.Ty_cons (id', []) -> I.Ty_cons (id', [])
            | _ -> failwith "ill form alias")
        | None -> t)
    | I.Ty_cons (id, args) -> (
        match List.assoc_opt id alias_map with
        | Some _t' -> failwith "parameterized type alias is not supported"
        | None -> I.Ty_cons (id, List.map go args))
    | I.Ty_var { contents = I.Unbound _ } ->
        (* It's a 'never reach' when local module is not supported(that's the
           current implementation), because every normalized module type
           should only capture module level entities, which should have
           normalized and fully inferenced already. *)
        failwith "never reach: deliasing a type not fully inferenced"
    | I.Ty_var { contents = I.Link t' } -> go t'
    | I.Ty_qvar _ -> t
    | I.Ty_arrow (t0, t1) -> I.Ty_arrow (go t0, go t1)
    | I.Ty_tuple tes -> I.Ty_tuple (List.map go tes)
    | I.Ty_record fields ->
        I.Ty_record (List.map (fun (name, te) -> (name, go te)) fields)
  in
  go te

let dealias ((qvs, te) : I.bind_ty) alias_map = (qvs, dealias_te te alias_map)

let dealias_td (td : I.ty_def) alias_map =
  match td with
  | I.Ty_def_opaque (_, _) -> td
  | I.Ty_def_adt (name, paras, bs) ->
      I.Ty_def_adt
        ( name,
          paras,
          List.map
            (fun (cname, t) ->
              (cname, Option.map (fun t' -> dealias_te t' alias_map) t))
            bs )
  | I.Ty_def_record (name, paras, fields) ->
      I.Ty_def_record
        ( name,
          paras,
          List.map (fun (name, t) -> (name, dealias_te t alias_map)) fields
        )
  | I.Ty_def_alias (name, t) -> I.Ty_def_alias (name, dealias_te t alias_map)
