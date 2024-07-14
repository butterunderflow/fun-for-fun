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
  let _line (* ignore first line *) = input_line wrapped_out in
  let line = input_line wrapped_out in
  close_in wrapped_out;
  int_of_string line
