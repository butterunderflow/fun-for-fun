module P = Syntax.Parsetree

(** Two invariants must hold:
    1. A new type definition can be correctly normalize
    when its componnet type expressionss can be correctly
    normalized
    2. A type expression that indicate a type alias,
    can be correctly normalized when the normalized alias
    definition already added in environment.
 *)
let reorg_ty_defs (defs : P.ty_def list) =
  (* analyze alias dependency *)
  let analyze_deps defs =
    List.fold_left
      (fun acc def ->
        match def with
        | P.TDAlias (_, P.TCons (alias, _)) -> (
            match
              List.find_opt
                (fun def ->
                  match def with
                  | P.TDAdt (name', _, _)
                  | P.TDRecord (name', _, _)
                  | P.TDAlias (name', _) ->
                      alias = name')
                defs
            with
            | Some def' -> (def, def') :: acc
            | None -> acc)
        | _ -> acc)
      [] defs
    |> List.rev
  in
  let graph = analyze_deps defs in
  let alias_defs, no_alias_defs =
    List.fold_left
      (fun (ads, nads) def ->
        match def with
        | P.TDAlias _ -> (def :: ads, nads)
        | _ -> (ads, def :: nads))
      ([], []) defs
  in
  let no_alias_defs = List.rev no_alias_defs in
  let visited = ref [] in
  let rec dfs graph cluster node =
    if not (List.memq node !visited) then (
      let cluster = node :: cluster in
      visited := node :: !visited;
      match List.assoc_opt node graph with
      | Some neighber when List.memq neighber alias_defs ->
          dfs graph cluster neighber
      | _ -> cluster)
    else if List.memq node cluster then
      (* Find a node in current cluster => dircle detected! *)
      failwith "Find a circle in alias dependency"
    else cluster
  in
  let clusters =
    List.fold_left (fun acc def -> dfs graph [] def :: acc) [] alias_defs
  in
  List.flatten (List.rev clusters) @ no_alias_defs
