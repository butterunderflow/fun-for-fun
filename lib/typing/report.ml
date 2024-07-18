module T = Types_in
module PP = Render.DefaultPP

type err =
  | UnificationError of (T.ty * T.ty * Lexing.position * Lexing.position)

(* error reportings *)
(* todo: support error tolerable type checking *)
exception UnificationError of T.ty * T.ty * Lexing.position * Lexing.position

let unification_error t0 t1 loc1 loc2 =
  raise (UnificationError (t0, t1, loc1, loc2))

exception ComponentInCompatible of string * T.bind_ty * T.bind_ty

let in_compatible_error name t0 t1 =
  raise (ComponentInCompatible (name, t0, t1))

exception OccurError of string * T.ty * Lexing.position * Lexing.position

let occur_error tv te loc1 loc2 =
  match !tv with
  | T.Unbound v -> raise (OccurError (Ident.to_string v, te, loc1, loc2))
  | T.Link _ -> failwith "neverreach"

let print_code_range start last =
  Printf.printf "%d:%d-%d:%d " start.Lexing.pos_lnum start.Lexing.pos_cnum
    last.Lexing.pos_lnum last.Lexing.pos_cnum

let unknown_location () = Printf.printf "At some unknown location: "

let wrap_with_error_report f =
  try Some (f ()) with
  | UnificationError (t0, t1, start, last) ->
      print_code_range start last;
      Printf.printf "Can't unify `%s` with `%s`" (PP.pp_str_of_ty t0)
        (PP.pp_str_of_ty t1);
      None
  | OccurError (tv, te, start, last) ->
      print_code_range start last;
      Printf.printf "internal error: occur check error\n";
      Printf.printf "type variable %s occured in " tv;
      PP.pp_ty Format.std_formatter te;
      None
  | ComponentInCompatible (name, (_, t0), (_, t1)) ->
      unknown_location ();
      Printf.printf
        "Value component %s has type `%s`, which is not compatible with `%s`"
        name (PP.pp_str_of_ty t0) (PP.pp_str_of_ty t1);
      None
