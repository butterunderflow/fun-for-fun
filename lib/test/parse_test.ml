open Parsing
open Ast


let print_parsed_program str = 
  parse_string_program str
  |> sexp_of_program
  |> Sexplib.Sexp.to_string_hum ?indent:(Some 2)
  |> print_string


let%expect_test _ =
  print_parsed_program {|let x = 1|};
  [%expect{|
    ((Top_let (PVar x) (EConst (CInt 1))))
  |}];
  print_parsed_program
    {|
     let x = 1
     let y = 2
     let rec foo x = foo x
     |};
  [%expect {|
    ((Top_let (PVar x) (EConst (CInt 1))) (Top_let (PVar y) (EConst (CInt 2)))
      (Top_letrec ((foo (x) (EApp (EVar foo) (EVar x)))))) |}]

