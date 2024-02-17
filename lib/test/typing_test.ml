open Syntax.Parsing
open Typing
module T = Typedtree

let print_sexp s =
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s |> print_string

let%expect_test "Test: expression typing" =
  let print_typed str =
    let e = parse_string_expr str in
    let typed, _ = Typing.Check.type_check e Typing.Env.empty in
    typed |> T.sexp_of_expr |> print_sexp
  in
  print_typed "1";
  [%expect {| (EConst (CInt 1) (TCons int ())) |}]
