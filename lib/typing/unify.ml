open Types_in
module SMap = Map.Make (Ident)
module Tree = Syntax.Parsetree

exception UnificationError of (ty * ty)

exception OccurError of (tv ref * ty)

exception IllFormType

let occurs (tpv : tv ref) (te : ty) : unit =
  let rec go te =
    match te with
    | Ty_tuple tes
    | Ty_cons (_, tes) ->
        List.iter go tes
    | Ty_var tpv' when tpv == tpv' -> (
        match tpv with
        | { contents = Unbound _ } -> raise (OccurError (tpv, te))
        | { contents = Link _ } -> failwith "illegal occur check value")
    | Ty_var { contents = Link te } -> go te
    | Ty_var ({ contents = Unbound (tvn', level') } as tpv') ->
        let[@warning "-8"] (Unbound (_, level)) = !tpv in
        let min_level = min level level' in
        tpv'.contents <- Unbound (tvn', min_level)
    | Ty_qvar _ ->
        failwith "internal error: unify with quantified type variable"
    | Ty_arrow (te1, te2) ->
        go te1;
        go te2
    | Ty_record fields -> List.map snd fields |> List.iter go
  in
  let rec strip te : ty =
    match te with
    | Ty_var { contents = Link te } -> strip te
    | _ -> te
  in
  go (strip te)

let rec unify (t0 : ty) (t1 : ty) : unit =
  if t0 == t1 then ()
  else
    match (t0, t1) with
    (* strip links *)
    | Ty_var { contents = Link t0 }, t1 -> unify t0 t1
    | t0, Ty_var { contents = Link t1 } -> unify t0 t1
    | Ty_var ({ contents = Unbound _ } as tv0), t1
    | t1, Ty_var ({ contents = Unbound _ } as tv0) ->
        occurs tv0 t1;
        tv0.contents <- Link t1
    | Ty_cons (tc0, tes0), Ty_cons (tc1, tes1) when tc0 = tc1 ->
        unify_lst tes0 tes1
    | Ty_arrow (op0, arg0), Ty_arrow (op1, arg1) ->
        unify_lst [ op0; arg0 ] [ op1; arg1 ]
    | Ty_tuple tes0, Ty_tuple tes1 -> unify_lst tes0 tes1
    (* by default raise an exception *)
    | _ -> raise (UnificationError (t0, t1))

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
