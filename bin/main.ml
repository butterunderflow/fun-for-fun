[@@@warning "-32"]

module S = Syntax
module C = Typing.Check
module Lo = Lam.Compile
module Li = Clos.Lift

let input_file = ref None

let output_file = ref ""

let output_stdout = ref false

let store_input filename =
  match !input_file with
  | Some _ -> assert false
  | None -> input_file := Some filename

let debug = ref false

let speclist =
  [
    ("-o", Arg.Set_string output_file, "Set output file name");
    ("--debug", Arg.Set debug, "Enable debug");
    ("--stdout", Arg.Set output_stdout, "Output to stdout");
  ]

let help_msg = "fun4fun: a functional programming language build for fun"

let usage () = Arg.usage speclist help_msg

let read_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let out_sexp oc s =
  Printf.fprintf oc "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let out_clos_prog oc e fns =
  Printf.fprintf oc "Lifted main expression: \n";
  out_sexp oc (Clos.Closure.sexp_of_expr e);
  Printf.fprintf oc "\nGlobal C functions: \n";
  List.iter (fun fn -> out_sexp oc (Clos.Closure.sexp_of_func fn)) fns

let default_output_file = "a.out"

let () =
  Arg.parse speclist store_input help_msg;
  let input_file =
    match !input_file with
    | None -> assert false
    | Some f -> f
  in
  let main_prog, c_fns =
    input_file
    |> read_file
    |> S.Parsing.parse_string_program
    |> (fun prog -> fst (Typing.Check.tc_program prog (Typing.Env.init ())))
    |> (fun prog -> Lo.compile_program prog)
    |> fun prog -> Li.lift prog
  in
  if !output_stdout then out_clos_prog Stdlib.stdout main_prog c_fns
  else
    let output_file =
      if !output_file = "" then default_output_file else !output_file
    in
    let oc = open_out output_file in
    out_clos_prog oc main_prog c_fns;
    close_out oc
