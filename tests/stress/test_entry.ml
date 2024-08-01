module U = Util

let sum_test =
  let rec sum x = if x = 0 then 0 else x + sum (x - 1) in
  QCheck.Test.make ~count:10 ~name:"sum.exe property" QCheck.small_nat
    (fun i ->
      Alcotest.(check int)
        "int compare" (sum i)
        (U.call_ff_int_to_int ~case:"sum.fun" i);
      true)

let fib_test =
  let rec fib x =
    match x with
    | 0 -> 1
    | 1 -> 1
    | _ -> fib (x - 1) + fib (x - 2)
  in
  QCheck.Test.make ~count:5 ~name:"fib.exe propety" (QCheck.int_bound 30)
    (fun i ->
      Alcotest.(check int)
        "int compare" (fib i)
        (U.call_ff_int_to_int ~case:"fib.fun" i);
      true)

module RTA = Rand_struct_access

let struct_acces_test =
  QCheck.Test.(
    make ~count:5 ~name:"struct acessing property"
      (QCheck.make RTA.tree_gen ~print:(fun tree ->
           fst (RTA.test_case_of_tree tree)))
      (fun tree ->
        let _int_list_tsetable = Alcotest.(list int) in
        let prog, expect = RTA.test_case_of_tree tree in
        Alcotest.(check (list int))
          "int list should equal" expect
          (U.call_ff_on_prog_to_int_list ~prog);
        true))

let () =
  let suite =
    List.map
      (QCheck_alcotest.to_alcotest ~verbose:true)
      [ sum_test; fib_test; struct_acces_test ]
  in
  Alcotest.run "Test compiled program property" [ ("calculation", suite) ]
