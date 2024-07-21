open Syntax.Parsing

let%expect_test "Error reporting syntax error test" =
  let print_syntax_err str = ignore (attempt2 "*test*" str) in
  print_syntax_err "absb";
  [%expect
    {|
    File "*test*", line 1, characters 0-4:
    Syntax error before 'absb'.
    (Curated message for this state is not supported)!
    |}];

  print_syntax_err "let 23 = ";
  [%expect
    {|
    File "*test*", line 1, characters 4-6:
    Syntax error after 'let' and before '23'.
    (Curated message for this state is not supported)!
    |}];

  print_syntax_err
    {|
                    let x = 1

                    let k = M
                    
                    let 3 = 2
|};
  [%expect {|
    File "*test*", line 6, characters 24-25:
    Syntax error after 'let' and before '3'.
    (Curated message for this state is not supported)!
    |}]
