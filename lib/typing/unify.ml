open Types_in
module E = Types_ext
module SMap = Map.Make (Ident)
module Tree = Syntax.Parsetree

exception UnificationError of (string * string)

exception OccurError of (string * ty)

exception IllFormType

let rec occurs (tpv : tv ref) (te : ty) : unit =
  match te with
  | TTupleI tes
  | TConsI (_, tes) ->
      List.iter (occurs tpv) tes
  | TVarI tpv' when tpv == tpv' -> failwith "occur check fail"
  | TVarI { contents = Link te } -> occurs tpv te
  | TVarI { contents = _ } -> ()
  | TQVarI _ -> ()
  | TArrowI (te1, te2) ->
      occurs tpv te1;
      occurs tpv te2
  | TRecordI fields -> List.map snd fields |> List.iter (occurs tpv)

let rec unify (t0 : ty) (t1 : ty) : unit =
  if t0 == t1 then ()
  else
    match (t0, t1) with
    | TVarI ({ contents = Unbound _ } as tv0), t1 ->
        occurs tv0 t1;
        tv0.contents <- Link t1
    | TVarI { contents = Link t0 }, t1 -> unify t0 t1
    | _, TVarI _y -> unify t1 t0
    | TConsI (tc0, tes0), TConsI (tc1, tes1) when tc0 = tc1 ->
        unify_lst tes0 tes1
    | TArrowI (op0, arg0), TArrowI (op1, arg1) ->
        unify_lst [ op0; arg0 ] [ op1; arg1 ]
    | TTupleI tes0, TTupleI tes1 -> unify_lst tes0 tes1
    (* by default raise an exception *)
    | _ ->
        let t0_str =
          sexp_of_ty t0 |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)
        in
        let t1_str =
          sexp_of_ty t1 |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)
        in
        raise (UnificationError (t0_str, t1_str))

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
    true
  with
  | OccurError _ -> false
