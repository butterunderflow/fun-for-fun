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

let help_msg = "fff: a functional programming language build for fun"

let usage () = Arg.usage speclist help_msg

let read_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let out_sexp oc s =
  Printf.fprintf oc "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let default_output_file = "a.out"

let get_output_file ?postfix:(post = "") () =
  let output_basename =
    if !output_file = "" then default_output_file else !output_file
  in
  Printf.sprintf "%s%s" output_basename post

let debug_compose prog (pass, dbg, post) =
  let result = pass prog in
  if !debug then (
    let oc = open_out (get_output_file ~postfix:post ()) in
    Printf.fprintf oc "%s" (dbg result);
    close_out oc);
  result

let ( |-> ) = debug_compose

let () =
  Arg.parse speclist store_input help_msg;
  let input_file =
    match !input_file with
    | None -> assert false
    | Some f -> f
  in
  let prog =
    input_file
    |> read_file
    |-> (S.Parsing.parse_string_program, S.Parsetree.dbg, ".parsing")
    |-> ( (fun prog ->
            let typed, _env =
              Typing.Check.tc_program prog (Typing.Env.init ())
            in
            typed),
          Typing.Render.default_dbg,
          ".typed" )
    |-> (Lo.compile_program, Lam.Tree.dbg, ".lambda")
    |-> (Li.lift, Clos.Closure.dbg, ".closure")
  in
  if !output_stdout then print_string (Clos.Closure.dbg prog)
  else
    let output_file = get_output_file () in
    let oc = open_out output_file in
    Stdlib.output_string oc (Clos.Closure.dbg prog);
    close_out oc
