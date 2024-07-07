open Back.Ctree
module P = Back.Cprint1

let%expect_test "Test: Backend" =
  let bin =
    BINARY (ADD, CONSTANT (CONST_INT "1"), CONSTANT (CONST_INT "2"))
  in
  P.out := Buffer.create 50;
  P.print_expression bin 0;
  P.commit ();
  P.flush ();
  !P.out |> Buffer.contents |> print_string;
  [%expect {| 1 + 2 |}]
