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
  with
  | exn ->
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      let tail = Lexer.tail "" lexbuf in
      raise (ParseError { exn; line; cnum; tok; tail })

let parse_string_expr = gen_parse_string Parser.expr_dbg

let parse_string_program = gen_parse_string Parser.program

let parse_string_pattern = gen_parse_string Parser.pattern_dbg

let parse_string_type_expr = gen_parse_string Parser.type_expr_dbg

let parse_string_mod_expr = gen_parse_string Parser.mod_expr_dbg

let parse_string_mod_type = gen_parse_string Parser.mod_type_dbg

(******************************************************************)
(* most of the following are copied from:
   https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc-syntax-errors/calc.ml *)

module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20
(* max width 43 *)

(* [env checkpoint] extracts a parser environment out of a checkpoint, which
   must be of the form [HandlingError env]. *)
let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false

(* [get text checkpoint i] extracts and shows the range of the input text
   that corresponds to the [i]-th stack cell. The top stack cell is numbered
   zero. *)
let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i] keywords
         are correctly inside the syntax error message database. The integer
         [i] should always be a valid offset into the known suffix of the
         stack. *)
      "???"

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory and
         should be fixed in the future. *)
      0

let message id =
  try ParserMessages.message id with
  | Not_found -> "(Curated message for this state is not supported)"

let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  let show_at = show text in
  (* Show the tokens just before and just after the error. *)
  let indication =
    Printf.sprintf "Syntax error %s.\n" (E.show show_at buffer)
  in
  (* Fetch an error message from the database. *)
  let message = message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  Printf.eprintf "%s%s%s!" location indication message;
  None

let attempt2 filename text =
  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = L.init filename (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a function
     of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions of
     the last two tokens. This is useful when a syntax error occurs, as these
     are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = Parser.Incremental.program lexbuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  I.loop_handle Option.some (fail text buffer) supplier checkpoint
