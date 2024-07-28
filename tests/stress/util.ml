(* todo: use a library to do these things *)

let call_ff_int_to_int ~(case : string) i =
  let exe = Printf.sprintf "./%s.out" case in
  let _pid =
    Unix.create_process "ff_wrapper" [| "ff_wrapper"; case |] Unix.stdin
      Unix.stdout Unix.stderr
  in
  ignore (Unix.wait ());
  let pr1, pw1 = Unix.pipe () in
  let pr2, pw2 = Unix.pipe () in
  let _pid = Unix.create_process exe [| exe |] pr1 pw2 Unix.stderr in
  let wrapped_in = Unix.out_channel_of_descr pw1 in
  output_string wrapped_in (Printf.sprintf "%d\n" i);
  Stdlib.flush wrapped_in;
  ignore (Unix.wait ());
  let wrapped_out = Unix.in_channel_of_descr pr2 in
  let line = input_line wrapped_out in
  close_in wrapped_out;
  int_of_string line

[@@@warning "-27"]

open Lwt.Syntax

let call_ff_on_prog_to_int_list ~(prog : string) : int list =
  let temp_file =
    Filename.temp_file ~temp_dir:"./"
      (* create temprary file for input source program *) "rta_" ".fun"
  in
  (* write input program to temp file *)
  let oc = open_out temp_file in
  Printf.fprintf oc "%s" prog;
  close_out oc;
  (* compiling *)
  Printf.printf "Call ff_wraper on tempfile %s\n" temp_file;
  let _pid =
    Unix.create_process "ff_wrapper"
      [| "ff_wrapper"; temp_file |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  ignore (Unix.wait ());
  let exe = Printf.sprintf "%s.out" temp_file in
  Printf.printf "Dumped exe file path: %s\n" exe;
  Lwt_main.run
    (let* out = Lwt_process.pread (exe, [| exe |]) in
     let lines = String.split_on_char '\n' out in
     let ints =
       List.(lines |> filter (fun s -> s <> "") |> map int_of_string)
     in
     Lwt.return ints)
