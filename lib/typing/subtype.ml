module I = Types_in
module P = Poly

let build_mod_correspond mt0 mt1 =
  (* a map which correspond mt0 with mt1 *)
  let mid_map = ref [] in
  let rec collect_mid_maping mt0 mt1 =
    match (mt0, mt1) with
    | ( I.MTMod { id = id0; mod_defs = mds0; _ },
        I.MTMod { id = id1; mod_defs = mds1; _ } ) ->
        mid_map := (id1, id0) :: !mid_map;
        List.iter
          (fun (name, md1) ->
            let md0 = List.assoc name mds0 in
            collect_mid_maping md0 md1)
          mds1
    | I.MTFun (argt0, mt0), I.MTFun (argt1, mt1) ->
        collect_mid_maping argt0 argt1;
        collect_mid_maping mt0 mt1
    | _ -> failwith "subtype check error"
  in
  collect_mid_maping mt0 mt1;
  !mid_map

(* a substituter substitute module id in mt1 with their correspondence in
   mt0 *)
let create_correspond_subst correspond_map =
  let mapper =
    object
      (* todo: remove this object *)
      inherit [_] Types_in.map as super

      method! visit_ty_id () (id, name) =
        match List.assoc_opt id correspond_map with
        | Some id1 -> (id1, name)
        | _ -> (id, name)

      method! visit_tv () tv =
        match tv with
        | I.Unbound _ ->
            failwith
              "neverreach: every module should have empty inference space"
        | I.Link _ -> super#visit_tv () tv
    end
  in
  fun mt -> mapper#visit_mod_ty () mt

let compatible mt0 mt1 =
  let alias_map : (I.ty_id * I.ty) list ref = ref [] in
  let rec compatible_aux mt0 mt1 : unit =
    match (mt0, mt1) with
    | ( I.MTMod
          {
            val_defs = vds0;
            constr_defs = cds0;
            ty_defs = tds0;
            mod_defs = mds0;
            mod_sigs = _ms0;
            id = id0;
            _;
          },
        I.MTMod
          {
            val_defs = vds1;
            constr_defs = cds1;
            ty_defs = tds1;
            mod_defs = mds1;
            mod_sigs = ms1;
            _;
          } ) ->
        List.iter
          (fun td1 ->
            match td1 with
            | I.TDOpaque (name, paras) -> (
                let td0 = I.get_def name tds0 in
                match td0 with
                | I.TDOpaque (_, paras0)
                | I.TDAdt (_, paras0, _)
                | I.TDRecord (_, paras0, _) ->
                    if List.length paras0 <> List.length paras then
                      failwith
                        "number of type parameter not compatible in opaque \
                         type"
                | I.TDAlias (_, ty0) -> (
                    match paras with
                    | [] -> alias_map := ((id0, name), ty0) :: !alias_map
                    | _ :: _ -> failwith "type alias has parameter"))
            | _ ->
                let td0 = I.get_def (I.get_def_name td1) tds0 in
                if td0 <> Alias.dealias_td td1 !alias_map then
                  failwith "a type def component not compatible")
          tds1;
        List.iter
          (fun (name, vt1) ->
            let vt0 = List.assoc name vds0 in
            if
              P.align_inst vt0 <> P.align_inst (Alias.dealias vt1 !alias_map)
            then Report.error_incompatible name vt0 vt1)
          vds1;
        List.iter
          (fun (name, (cd1, cid1)) ->
            let cd0, cid0 = List.assoc name cds0 in
            if
              cid1 <> cid0
              || P.align_inst cd0
                 <> P.align_inst (Alias.dealias cd1 !alias_map)
            then
              failwith
                (Printf.sprintf "a constructor component `%s` not compatible"
                   name))
          cds1;
        List.iter
          (fun (name, md1) -> compatible_aux (List.assoc name mds0) md1)
          mds1;
        List.iter
          (fun (name, ms1) -> compatible_aux (List.assoc name mds0) ms1)
          ms1
    | I.MTFun (argt0, mt0), I.MTFun (argt1, mt1) ->
        compatible_aux argt1 argt0;
        compatible_aux mt0 mt1
    | _ -> failwith "subtype check error"
  in
  compatible_aux mt0 mt1;
  !alias_map

(* Check if mt0 is more specifc than mt1, return a substituter. This
   substituter tell us how to replace module id in mt1 with their
   correspondence in mt0, and how to reolve aliases introduced by mt0 *)
let check_subtype (mt0 : I.mod_ty) (mt1 : I.mod_ty) : I.mod_ty -> I.mod_ty =
  let correspond_map = build_mod_correspond mt0 mt1 in
  let correspond_subst = create_correspond_subst correspond_map in
  let mt1 = correspond_subst mt1 in
  (* We need a map to keep alias between opaque type to it's coreesponding
     transparent type alias *)
  let alias_map = compatible mt0 mt1 in
  let dealias =
    object
      (* todo: remove this object *)
      inherit [_] Types_in.map

      method! visit_ty () te = Alias.dealias_te te alias_map
    end
  in
  (* a substituter knows how to replace mt, which depends on mt1, with mt0,
     which is more specific than mt1 *)
  fun mt -> mt |> correspond_subst |> dealias#visit_mod_ty ()
