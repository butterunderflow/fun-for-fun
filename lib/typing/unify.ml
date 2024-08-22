open Types_in
module SMap = Map.Make (Ident)
module Tree = Syntax.Parsetree

exception OccurError of (tv ref * ty)

exception IllFormType

let occurs (tpv : tv ref) (te : ty) : unit =
  let rec go te =
    match te with
    | TTuple tes
    | TCons (_, tes) ->
        List.iter go tes
    | TVar tpv' when tpv == tpv' -> (
        match tpv with
        | { contents = Unbound (name, _) } -> Report.error_occur name te
        | { contents = Link _ } -> failwith "illegal occur check value")
    | TVar { contents = Link te } -> go te
    | TVar ({ contents = Unbound (tvn', level') } as tpv') ->
        let[@warning "-8"] (Unbound (_, level)) = !tpv in
        let min_level = min level level' in
        tpv'.contents <- Unbound (tvn', min_level)
    | TQVar _ -> ()
    | TArrow (te1, te2) ->
        go te1;
        go te2
    | TRecord fields -> List.map snd fields |> List.iter go
  in
  let rec strip te : ty =
    match te with
    | TVar { contents = Link te } -> strip te
    | _ -> te
  in
  go (strip te)

let rec unify (t0 : ty) (t1 : ty) : unit =
  if t0 == t1 then ()
  else
    match (t0, t1) with
    (* strip links *)
    | TVar { contents = Link t0 }, t1 -> unify t0 t1
    | t0, TVar { contents = Link t1 } -> unify t0 t1
    | TVar ({ contents = Unbound _ } as tv0), t1
    | t1, TVar ({ contents = Unbound _ } as tv0) ->
        occurs tv0 t1;
        tv0.contents <- Link t1
    | TCons (tc0, tes0), TCons (tc1, tes1) when tc0 = tc1 ->
        unify_lst tes0 tes1
    | TArrow (op0, arg0), TArrow (op1, arg1) ->
        unify_lst [ op0; arg0 ] [ op1; arg1 ]
    | TTuple tes0, TTuple tes1 -> unify_lst tes0 tes1
    (* by default raise an exception *)
    | _ -> Report.error_unification t0 t1

and unify_lst t0 t1 =
  match (t0, t1) with
  | [], [] -> ()
  | t0 :: tes0, t1 :: tes1 ->
      unify t0 t1;
      unify_lst tes0 tes1
  | _ -> raise IllFormType

let occur (tpv : tv ref) (te : ty) : bool =
  try
    occurs tpv te;
    false
  with
  | OccurError (_tpv, _te) -> true

(* initialize forward definition *)
let () = Env.occur := occur
