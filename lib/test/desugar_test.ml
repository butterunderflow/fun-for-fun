open Syntax.Parsing
open Sugar.Desugar
open Sugar.Ast

let print_desugared_program str =
  parse_string_program str |> desugar |> sexp_of_program
  |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)
  |> print_string

let%expect_test _ =
  print_desugared_program {|let x = 1|};
  [%expect {|
    ((Top_let x/0 (EConst (CInt 1))))
  |}];
  print_desugared_program
    {|
     let x = 1
     let y = 2
     let rec foo x = foo x
     |};
  [%expect
    {|
    ((Top_let x/0 (EConst (CInt 1))) (Top_let y/0 (EConst (CInt 2)))
      (Top_letrec ((foo/0 ((PBare x/0) (EApp (EVar foo/0) (EVar x/0))))))) |}];
  print_desugared_program {|let rec f (x:int) = 1|};
  [%expect
    {| ((Top_letrec ((f/0 ((PAnn x/0 (TCons int/0 ())) (EConst (CInt 1))))))) |}]
