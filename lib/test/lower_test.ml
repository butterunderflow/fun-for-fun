open Syntax.Parsing
module L = Lam.Tree
module C = Lam.Compile

[@@@warning "-26"]

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: full program lowering" =
  let print_lowered str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let typed, _env = Typing.Check.tc_program prog (Typing.Env.init ()) in
    typed |> C.compile_program |> L.sexp_of_expr |> print_sexp
  in

  print_lowered {| let x = 1 |};
  [%expect {| (EModObject ((FSimple x (EConst (CInt 1))))) |}]
