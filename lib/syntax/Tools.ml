let parse_string_program_opt s =
  try Some (Parsing.gen_parse_string Parser.program s) with
  | _ -> None
