let type_check_program prog =
  Env.refresh_id ();
  Report.refresh ();
  let env = Env.init () in
  let result = Check.tc_top_levels prog env in
  match !Report.errors with
  | [] -> result
  | _ ->
      Report.report ();
      result
