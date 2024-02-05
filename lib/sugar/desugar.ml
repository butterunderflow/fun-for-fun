module E = Syntax.Parsetree (* external syntax programmer faced *)
module I = Ast (* internal syntax compiler see *)
module IdentMap = Map.Make (Ident)

let rec ds_type_expr (type_expr : E.type_expr) =
  match type_expr with
  | TCons (constr, ts) ->
      I.TCons (Ident.from constr, List.map ds_type_expr ts)
  | TVar v -> I.TVar (Ident.from v)

let rec ds_expr (e : E.expr) : I.expr =
  match e with
  | E.EConst c -> I.EConst c
  | E.EVar x -> I.EVar (Ident.from x)
  | E.ELet (p, e0, e1) ->
      let flatten = ds_pat_binding p e0 in
      List.fold_right
        (fun (x, e) acc -> I.ELet (x, e, acc))
        flatten (ds_expr e1)
  | E.ELetrec (defs, e) ->
      let desugared_defs = List.map ds_curry defs in
      I.ELetrec (desugared_defs, ds_expr e)
  | E.ELam (p, e) -> I.ELam (ds_para p, ds_expr e)
  | E.EIf (e0, e1, e2) -> I.EIf (ds_expr e0, ds_expr e1, ds_expr e2)
  | E.ECase (e, bs) ->
      I.ECase (ds_expr e, List.map (fun (p, e) -> (ds_pat p, ds_expr e)) bs)
  | E.EApp (e0, e1) -> I.EApp (ds_expr e0, ds_expr e1)
  | E.EAnn (e, te) -> I.EAnn (ds_expr e, ds_type_expr te)
  | E.ETuple es -> I.ETuple (List.map ds_expr es)
  | E.EFetchTuple (e, i) -> I.EFetchTuple (ds_expr e, i)

and ds_pat (pat : E.pattern) : I.pattern =
  match pat with
  | PVal v -> I.PVal v
  | PCons (constr, ps) -> I.PCons (Ident.from constr, List.map ds_pat ps)
  | PVar x -> I.PVar (Ident.from x)

and ds_pat_binding (pat : E.pattern) (e : E.expr) : (Ident.t * I.expr) list =
  let rec take_binding_vars pat acc =
    match pat with
    | E.PVal _ -> []
    | E.PCons (_constr, ps) ->
        List.fold_left (fun acc1 p -> take_binding_vars p acc1) acc ps
    | E.PVar name -> name :: acc
  in
  match pat with
  | E.PVar x -> [ (Ident.from x, ds_expr e) ]
  | _ ->
      let vars =
        List.map (fun x -> Ident.from x) (take_binding_vars pat [])
      in
      let flatten =
        ( Ident.create ~hint:"flatten",
          I.ECase
            ( ds_expr e,
              [ (ds_pat pat, I.ETuple (List.map (fun x -> I.EVar x) vars)) ]
            ) )
      in
      flatten :: List.mapi (fun i x -> (x, I.EFetchTuple (I.EVar x, i))) vars

and ds_para (p : E.para) =
  match p with
  | E.PAnn (n, te) -> I.PAnn (Ident.from n, ds_type_expr te)
  | E.PBare n -> I.PBare (Ident.from n)

and ds_curry ((name : string), (ps : E.paras), (e : E.expr)) :
    Ident.t * I.lambda =
  match ps with
  | [] -> failwith "never reach"
  | p :: rest ->
      ( Ident.from name,
        ( ds_para p,
          List.fold_right
            (fun p acc -> I.ELam (ds_para p, acc))
            rest (ds_expr e) ) )

let ds_type_def name paras variants =
  I.Top_type
    ( Ident.from name,
      List.map (fun x -> Ident.from x) paras,
      List.map
        (fun (constr, ts) -> (Ident.from constr, List.map ds_type_expr ts))
        variants )

let ds_top_level (top : E.top_level) : I.top_level list =
  match top with
  | E.Top_let (p, e) ->
      let flatten_bindings = ds_pat_binding p e in
      List.map (fun (n, e) -> I.Top_let (n, e)) flatten_bindings
  | E.Top_letrec defs -> [ I.Top_letrec (List.map ds_curry defs) ]
  | E.Top_type (n, paras, vs) -> [ ds_type_def n paras vs ]

let ds_program (prog : E.program) = List.(flatten (map ds_top_level prog))

let desugar prog = ds_program prog
