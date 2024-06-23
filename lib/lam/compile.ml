module T = Typing.Typedtree
module L = Tree

let rec compile_expr (e : T.expr) =
  match e with
  | T.EConst (c, _) -> L.EConst c
  | T.EVar (x, _) -> L.EVar x
  | T.ELet (x, e0, e1, _) -> L.ELet (x, compile_expr e0, compile_expr e1)
  | T.ELetrec (lams, e, _) ->
      L.ELetRec
        (List.map (fun (x, lam) -> (x, compile_lam lam)) lams, compile_expr e)
  | T.ELam lam -> L.ELam (compile_lam lam)
  | T.EIf (e0, e1, e2, _) ->
      L.EIf (compile_expr e0, compile_expr e1, compile_expr e2)
  | T.ECase (e0, bs, _) ->
      L.ESwitch (compile_expr e0, List.map compile_branch bs)
  | T.EApp (e0, e1, _) -> L.EApp (compile_expr e0, compile_expr e1)
  | T.EAnn (e, _) -> compile_expr e
  | T.ETuple (es, _) -> L.ETuple (List.map compile_expr es)
  | T.EField (me, name, _) -> L.EField (compile_mod_expr me, name)
  | T.ECons (_, id, _) -> L.ECons id
  | T.EFieldCons (_, _, id, _) -> L.ECons id

and compile_lam (x, e, _) = (x, compile_expr e, ref [])

and compile_branch (p, e) : L.branch =
  let compile_pattern p =
    let rec go p =
      match p with
      | T.PVal c -> L.PVal c
      | T.PCons (_cname, id, None) -> L.PCons (id, None)
      | T.PCons (_cname, id, Some p') -> L.PCons (id, Some (go p'))
      | T.PVar (x, _) -> L.PVar x
      | T.PTuple ps -> L.PTuple (List.map go ps)
    in
    go p
  in
  (compile_pattern p, compile_expr e)

and compile_mod_expr me =
  match me with
  | T.MEName (x, _) -> L.EVar x
  | T.MEStruct (tops, _) -> compile_top_levels tops
  | T.MEFunctor ((x, _), body) -> L.ELam (x, compile_mod_expr body, ref [])
  | T.MEField (me, name, _) -> L.EField (compile_mod_expr me, name)
  | T.MEApply (me0, me1, _) ->
      L.EApp (compile_mod_expr me0, compile_mod_expr me1)
  | T.MERestrict (me', _, _) -> compile_mod_expr me'

and compile_top_levels tops =
  L.EModObject
    (List.filter_map
       (fun (top : T.top_level) ->
         match top with
         | T.TopLet (x, e) -> Some (L.FSimple (x, compile_expr e))
         | T.TopLetRec binds ->
             Some
               (L.FLetRec
                  (List.map (fun (x, lam) -> (x, compile_lam lam)) binds))
         | T.TopTypeDef _ -> None
         | T.TopMod (x, me) -> Some (L.FSimple (x, compile_mod_expr me))
         | T.TopModSig (_, _) -> None)
       tops)

let compile_program program = compile_top_levels program
