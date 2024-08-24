module I = Types_in

let get_def_name (td : I.ty_def) =
  match td with
  | TDOpaque (name, _)
  | TDAdt (name, _, _)
  | TDRecord (name, _, _)
  | TDAlias (name, _) ->
      name

let find_ty_def name (st : I.structure) =
  try
    List.find
      (fun td ->
        match td with
        | I.TDOpaque (name', _)
        | I.TDAdt (name', _, _)
        | I.TDRecord (name', _, _)
        | I.TDAlias (name', _)
          when name' = name ->
            true
        | _ -> false)
      st.ty_defs
  with
  | Not_found -> Report.(error_comp_not_found Type name st)

let find_component kind name map st =
  try List.assoc name map with
  | Not_found -> Report.error_comp_not_found kind name st

let find_val_comp name (st : I.structure) =
  find_component Value name st.val_defs st

let find_constr_comp name (st : I.structure) =
  find_component Constructor name st.constr_defs st

let find_mod_comp name (st : I.structure) =
  find_component Mod name st.mod_defs st

let find_mod_sig_comp name (st : I.structure) =
  find_component ModSig name st.mod_sigs st
