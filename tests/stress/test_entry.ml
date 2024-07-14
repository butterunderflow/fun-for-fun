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

let () =
  let suite =
    List.map
      (QCheck_alcotest.to_alcotest ~verbose:true)
      [ sum_test; fib_test ]
  in
  Alcotest.run "Test compiled program property" [ ("calculation", suite) ]
