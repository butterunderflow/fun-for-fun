let type_check_program prog =
  Env.refresh_id ();
  let env = Env.init () in
  Check.tc_top_levels prog env
