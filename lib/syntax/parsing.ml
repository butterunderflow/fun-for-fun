open Sexplib.Std

type pos = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

let sexp_of_pos (p : pos) =
  sexp_of_string
    (Printf.sprintf "%s:%d:%d" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol))

exception
  ParseError of {
    exn : exn;
    line : int;
    cnum : int;
    tok : string;
    tail : string;
  }

let gen_parse_string parse string =
  let lexbuf = Lexing.from_string string in
  try
    let out = parse Lexer.token lexbuf in
    out
  with exn ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    let tail = Lexer.tail "" lexbuf in
    raise (ParseError { exn; line; cnum; tok; tail })



let parse_string_program = gen_parse_string Parser.program

let parse_string_path = gen_parse_string Parser.path_dbg

let parse_string_type_expr = gen_parse_string Parser.type_expr_dbg

let parse_string_mod_expr = gen_parse_string Parser.mod_expr_dbg

