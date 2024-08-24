module T = Types_in
module PP = Render.NoTypeHintPP

exception UnificationError of (T.ty * T.ty)

exception OccurError of string * T.ty

type incompatible_kind =
  | Value
  | Type
  | Mod
  | ModSig
  | Constructor

exception ComponentInCompatible0 of string * T.bind_ty * T.bind_ty

exception ComponentNotFound of incompatible_kind * string * T.structure

exception LocatedErr of exn * Lexing.position * Lexing.position * Env.t

let error_incompatible0 name vt0 vt1 =
  raise (ComponentInCompatible0 (name, vt0, vt1))

let error_comp_not_found kind name mt =
  raise (ComponentNotFound (kind, name, mt))

let error_unification t0 t1 = raise (UnificationError (t0, t1))

let error_occur name te = raise (OccurError (Ident.to_string name, te))

let wrap_and_reraise (kind : exn) start last env =
  match kind with
  | LocatedErr _ ->
      (* An already wraped exception *)
      raise kind
  | _ -> raise (LocatedErr (kind, start, last, env))

let print_code_range (start : Lexing.position) (last : Lexing.position) =
  let start_col = start.pos_cnum - start.pos_bol in
  let last_col = start.pos_cnum - start.pos_bol in
  Printf.printf "%d:%d-%d:%d " start.pos_lnum start_col last.pos_lnum
    last_col

let unknown_location () = Printf.printf "At some unknown location: "

let report_error ?env (err : exn) =
  match err with
  | UnificationError (t0, t1) ->
      Printf.printf "Can't unify `%s` with `%s`" (PP.pp_str_of_ty ?env t0)
        (PP.pp_str_of_ty ?env t1);
      None
  | OccurError (tvn, te) ->
      Printf.printf "internal error: occur check error\n";
      Printf.printf "type variable %s occured in " tvn;
      PP.pp_ty Format.std_formatter te;
      None
  | ComponentInCompatible0 (name, (_, t0), (_, t1)) ->
      Printf.printf
        "Value component %s has type `%s`, which is not compatible with `%s`"
        name (PP.pp_str_of_ty t0) (PP.pp_str_of_ty t1);
      None
  | ComponentNotFound (kind, name, { id = mid; _ }) ->
      Printf.printf "%s component %s not exists in module %s"
        (match kind with
        | Value -> "Value"
        | Type -> "Type definition"
        | Mod -> "Module"
        | ModSig -> "Module Signature"
        | Constructor -> "Constructor")
        name
        (match env with
        | Some env -> (
            match Env.lookup_hint mid env with
            | Some me -> PP.pp_str_of_mod_expr me
            | None -> Printf.sprintf "{ id = %d}" mid)
        | None -> Printf.sprintf "%d" mid);
      None
  | Failure msg ->
      Printf.printf "%s\n" msg;
      None
  | _ ->
      Printf.printf "unknown error";
      None

let wrap_with_error_report f =
  try Some (f ()) with
  | LocatedErr (e, start, last, env) ->
      print_code_range start last;
      report_error ~env e
  | Failure msg ->
      unknown_location ();
      Printf.printf "%s\n" msg;
      None
  | _ ->
      unknown_location ();
      Printf.printf "Some unknown erroer occured";
      None
