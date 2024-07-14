module U = Util

let rec sum x = if x = 0 then 0 else x + sum (x - 1)

let sum_test =
  QCheck.Test.make ~count:10 ~name:"ff sum is correct" QCheck.small_nat
    (fun i -> U.call_ff_int_to_int ~case:"sum.fun" i = sum i)

let () = ignore (QCheck_runner.run_tests ~verbose:true [ sum_test ])
