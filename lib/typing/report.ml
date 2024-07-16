module T = Types_in
module PP = Render.DefaultPP

type err =
  | UnificationError of (T.ty * T.ty * Lexing.position * Lexing.position)

let errors = ref []

let refresh () = errors := []

let report_err err =
  match err with
  | UnificationError (t0, t1, start, last) ->
      Printf.printf "%d:%d - %d:%d Can't unify " start.Lexing.pos_lnum
        start.Lexing.pos_cnum last.Lexing.pos_lnum last.Lexing.pos_cnum;
      PP.pp_ty Format.std_formatter t0;
      Printf.printf " with ";
      PP.pp_ty Format.std_formatter t1;
      Printf.printf "\n"

let report () = List.iter report_err !errors
