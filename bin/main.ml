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

let version = "%%VERSION%%"

let build_date = "%%BUILD_DATE%%"

let show_version = ref false

let speclist =
  [
    ("-o", Arg.Set_string output_file, "Set output file name");
    ("--version", Arg.Set show_version, "Show version number");
    ("--debug", Arg.Set debug, "Enable debug");
    ("--stdout", Arg.Set output_stdout, "Output to stdout");
  ]

let help_msg = "ff: a functional programming language build for fun"

let show_usage () = Arg.usage speclist help_msg

let read_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  if Sys.file_exists filename then (
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    Some s)
  else None

let default_output_file = "a.out"

let get_output_file () =
  if !output_file = "" then default_output_file else !output_file

let debug_compose input (pass, dbg, post) =
  Option.bind input (fun prog ->
      let result = pass prog in
      match result with
      | Some result' ->
          if !debug then (
            let no_extension_filename =
              Filename.remove_extension (get_output_file ())
            in
            let debug_filename =
              Printf.sprintf "%s.%s" no_extension_filename post
            in
            let oc = open_out debug_filename in
            Printf.fprintf oc "%s" (dbg result');
            close_out oc);
          result
      | None -> None)

let ( |-> ) = debug_compose

let wrap_pass pass x =
  try Some (pass x) with
  | Failure msg ->
      Printf.printf "Failed with: %s\n" msg;
      None
  | _ -> None

let () =
  Arg.parse speclist store_input help_msg;
  if !show_version then (
    Printf.printf "ff: (%s %s)\n" version build_date;
    exit 0);
  let input_file =
    match !input_file with
    | None ->
        Printf.printf "No input file provided!\n";
        show_usage ();
        exit 0
    | Some f -> f
  in
  let output =
    input_file
    |> read_file
    |-> (S.Parsing.attempt2 input_file, S.Parsetree.dbg, "parsing")
    |-> ( (fun prog ->
            Option.map
              (fun (typed, _env) -> typed)
              (Typing.Report.wrap_with_error_report (fun () ->
                   Typing.Tools.type_check_program prog))),
          Typing.Render.default_dbg,
          "typed" )
    |-> (wrap_pass Lo.compile_program, Lam.Tree.dbg, "lambda")
    |-> (wrap_pass Li.lift, Clos.Closure.dbg, "closure")
    |-> (wrap_pass Back.Closure_translator.translate, (fun x -> x), "c_dbg")
  in
  match output with
  | Some prog ->
      if !output_stdout then print_string prog;
      let output_file = get_output_file () in
      let oc = open_out output_file in
      Stdlib.output_string oc prog;
      close_out oc
  | None -> Printf.eprintf "\nCompilation failed!"
