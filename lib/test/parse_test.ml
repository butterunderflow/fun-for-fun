open Syntax.Parsing
open Syntax.Parsetree

let print_sexp s =
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s |> print_string

let print_parsed_program str =
  parse_string_program str |> sexp_of_program |> print_sexp

let print_parsed_path str =
  parse_string_path str |> sexp_of_path |> print_sexp

let%expect_test "Test: full program parsing" =
  print_parsed_program {|let x = 1|};
  [%expect {|
    ((Top_let (PVar x) (EConst (CInt 1))))
  |}];
  print_parsed_program
    {|
     let x = 1
     let y = 2
     let rec foo x = foo x
     |};
  [%expect
    {|
    ((Top_let (PVar x) (EConst (CInt 1))) (Top_let (PVar y) (EConst (CInt 2)))
      (Top_letrec ((foo ((PBare x)) (EApp (EVar foo) (EVar x)))))) |}];
  print_parsed_program {|let rec f (x:int) = 1|};
  [%expect
      {| ((Top_letrec ((f ((PAnn x (TCons int ()))) (EConst (CInt 1)))))) |}]

let%expect_test "Test: path parsing" = 
  print_parsed_path {|X|};
  [%expect {| (PName X) |}];
  print_parsed_path {|X.Y|};
  [%expect {| (PMem (PName X) Y) |}];
  print_parsed_path {|X(Y)|};
  [%expect {| (PApply (PName X) (PName Y)) |}];
  print_parsed_path {|X.Y(Z(N))(W.M.N)|};
  [%expect {|
    (PApply (PApply (PMem (PName X) Y) (PApply (PName Z) (PName N)))
      (PMem (PMem (PName W) M) N)) |}]
