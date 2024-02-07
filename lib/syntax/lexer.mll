{
  open Parser
}

let upper_case = ['A' - 'Z']
let underline = '_'
let lower_case = ['a' - 'z']
let digit = ['0' - '9']
let blank_space = [' ' '\t']
let end_of_line = '\n'
let atom_quote = '''
let string_quote = '"'
let line_comment = '%' [^ '\n'] *
let open_comment = "/*"
let close_comment = "*/"
let escape = '\\'
let sign = ['+' '-']

let alphanumerical = upper_case | underline | lower_case | digit

(* module identifiers*)
let mod_ident = (underline | upper_case) alphanumerical *

(* Variables *)
let ident = (underline | lower_case) alphanumerical *

(* Type Variables *)
let type_var = '\'' (upper_case | underline | lower_case) alphanumerical*

(* Numbers *)
let digits = digit +
let integers = (sign?) digits

let boolean = "True" | "False"

let strings = '\"' _* '\"'

rule token = parse
    (* Meta-characters *)
    | [' ' '\t' '\n']   { token lexbuf }
    | eof               { EOF }
    | "let"             { LET }
    | "in"              { IN }
    | "rec"             { REC }
    | "type"            { TYPE }
    | "end"             { END }
    | "and"             { AND }
    | "module"          { MODULE }
    | "struct"          { STRUCT }
    | "sig"             { SIG }
    | "val"             { VAL }
    | "functor"         { FUNCTOR }
    | "fun"             { FUN }
    | "of"              { OF }
    | "->"              { ARROW }
    | "="               { EQ }
    | "*"               { STAR }
    | "|"               { OR }
    | '('               { LPAREN }
    | ')'               { RPAREN }
    | '['               { LBRACKET }
    | ']'               { RBRACKET }
    | ','               { COMMA }
    | ':'               { COLON }
    | mod_ident as m    { MIDENT m}
    | ident as i        { IDENT i }
    | type_var as t     { TYPEVAR t } 
    | integers as n     { INT   (int_of_string n) }
    | boolean as b      { BOOL (bool_of_string b)}
    | "."             { DOT}
    | strings as s      { STRING s}

and tail acc = parse
   | eof { acc }
   | _* as str { tail (acc ^ str) lexbuf }


{
    (* Takes a string s and returns a list of tokens generated by lexing s *)
    let get_all_tokens s =
        let b = Lexing.from_string (s ^ "\n") in
            let rec g () =
                match token b with
                | EOF -> []
                | t -> t :: g () in
                    g ()

    (* Takes a string s and returns Some of a list of tokens or None *)
    let try_get_all_tokens s =
        try Some (get_all_tokens s) with
        | Failure _ -> None
}

