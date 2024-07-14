module U = Util

let sum_test =
  let rec sum x = if x = 0 then 0 else x + sum (x - 1) in
  QCheck.Test.make ~count:10 ~name:"ff sum is correct" QCheck.small_nat
    (fun i -> U.call_ff_int_to_int ~case:"sum.fun" i = sum i)

let fib_test =
  let rec fib x =
    match x with
    | 0 -> 1
    | 1 -> 1
    | _ -> fib (x - 1) + fib (x - 2)
  in
  QCheck.Test.make ~count:3 ~name:"ff fib is correct" ~long_factor:5 QCheck.small_nat
    (fun i -> U.call_ff_int_to_int ~case:"fib.fun" i = fib i)

let () = ignore (QCheck_runner.run_tests ~verbose:true [ sum_test; fib_test ])
