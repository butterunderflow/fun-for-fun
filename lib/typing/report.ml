module T = Types_in
module PP = Render.DefaultPP

type err =
  | UnificationError of (T.ty * T.ty * Lexing.position * Lexing.position)

(* error reportings *)
(* todo: support error tolerable type checking *)
exception UnificationError of T.ty * T.ty * Lexing.position * Lexing.position

let unification_error t0 t1 loc1 loc2 =
  raise (UnificationError (t0, t1, loc1, loc2))

exception OccurError of string * T.ty * Lexing.position * Lexing.position

let occur_error tv te loc1 loc2 =
  match !tv with
  | T.Unbound v -> raise (OccurError (Ident.to_string v, te, loc1, loc2))
  | T.Link _ -> failwith "neverreach"

let print_code_range start last =
  Printf.printf "%d:%d-%d:%d " start.Lexing.pos_lnum start.Lexing.pos_cnum
    last.Lexing.pos_lnum last.Lexing.pos_cnum

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
  | _ ->
      Printf.printf "some raised error unhandled!";
      None
