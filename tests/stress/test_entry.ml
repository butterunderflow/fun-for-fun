module U = Util

let sum_test =
  let rec sum x = if x = 0 then 0 else x + sum (x - 1) in
  QCheck.Test.make ~count:10 ~name:"sum.exe property" QCheck.small_nat
    (fun i -> U.call_ff_int_to_int ~case:"sum.fun" i = sum i)

let fib_test =
  let rec fib x =
    match x with
    | 0 -> 1
    | 1 -> 1
    | _ -> fib (x - 1) + fib (x - 2)
  in
  QCheck.Test.make ~count:5 ~name:"fib.exe propety" (QCheck.int_bound 30)
    (fun i -> U.call_ff_int_to_int ~case:"fib.fun" i = fib i)

module RTA = Rand_struct_access

let struct_acces_test =
  QCheck.Test.(
    make ~count:5 ~name:"struct acessing property"
      (QCheck.make RTA.tree_gen ~print:(fun tree ->
           fst (RTA.test_case_of_tree tree)))
      (fun tree ->
        let prog, expect = RTA.test_case_of_tree tree in
        U.call_ff_on_prog_to_int_list ~prog = expect))

let () =
  let suite =
    List.map
      (QCheck_alcotest.to_alcotest ~verbose:true)
      [ sum_test; fib_test; struct_acces_test ]
  in
  Alcotest.run "Test compiled program property" [ ("calculation", suite) ]
