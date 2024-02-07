open Syntax.Parsing
open Syntax.Parsetree

let print_sexp s =
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s |> print_string

let print_parsed_program str =
  parse_string_program str |> sexp_of_program |> print_sexp

let print_parsed_path str =
  parse_string_path str |> sexp_of_path |> print_sexp

let print_parsed_type_expr str =
  parse_string_type_expr str |> sexp_of_type_expr |> print_sexp

let%expect_test "Test: full program parsing" =
  print_parsed_program {|let x = 1|};
  [%expect {|
    ((TopLet (PVar x) (EConst (CInt 1))))
  |}];
  print_parsed_program
    {|
     let x = 1
     let y = 2
     let rec foo = fun x -> foo x
     |};
  [%expect
    {|
    ((TopLet (PVar x) (EConst (CInt 1))) (TopLet (PVar y) (EConst (CInt 2)))
      (TopLetRec ((foo ((PBare x) (EApp (EVar foo) (EVar x))))))) |}];
  print_parsed_program {|let rec f = fun (x:int) -> 1|};
  [%expect
    {| ((TopLetRec ((f ((PAnn x (TCons int ())) (EConst (CInt 1))))))) |}];
  print_parsed_program
    {|
    let rec odd = fun x -> even x
    and even = fun x -> odd x
     |};
  [%expect
    {|
    ((TopLetRec
       ((odd ((PBare x) (EApp (EVar even) (EVar x))))
         (even ((PBare x) (EApp (EVar odd) (EVar x))))))) |}]

let%expect_test "Test: path parsing" =
  print_parsed_path {|X|};
  [%expect {| (PName X) |}];
  print_parsed_path {|X.Y|};
  [%expect {| (PMem (PName X) Y) |}];
  print_parsed_path {|X(Y)|};
  [%expect {| (PApply (PName X) (PName Y)) |}];
  print_parsed_path {|X.Y(Z(N))(W.M.N)|};
  [%expect
    {|
    (PApply (PApply (PMem (PName X) Y) (PApply (PName Z) (PName N)))
      (PMem (PMem (PName W) M) N)) |}]

let%expect_test "Test: type expression parsing" =
  print_parsed_type_expr "string";
  [%expect {| (TCons string ()) |}];
  print_parsed_type_expr "(string) list";
  [%expect {| (TCons list ((TCons string ()))) |}];
  print_parsed_type_expr "string list";
  [%expect {| (TCons list ((TCons string ()))) |}];
  print_parsed_type_expr "'x";
  [%expect {| (TVar 'x) |}];
  print_parsed_type_expr "(string, 'x, 'y) list";
  [%expect {| (TCons list ((TCons string ()) (TVar 'x) (TVar 'y))) |}];
  print_parsed_type_expr "int * int";
  [%expect {| (TTuple ((TCons int ()) (TCons int ()))) |}];
  print_parsed_type_expr "int * i list * (x * y) list * (t1 * t2)";
  [%expect {|
    (TTuple
      ((TCons int ()) (TCons list ((TCons i ())))
        (TCons list ((TTuple ((TCons x ()) (TCons y ())))))
        (TTuple ((TCons t1 ()) (TCons t2 ()))))) |}]

let%expect_test "Test: top level module" =
  print_parsed_program {|
     module X = struct
     end
     |};
  [%expect {|
    ((TopMod X (MEStruct ()))) |}];
  print_parsed_program
    {|
     module X = struct
       let x = 1
       let rec y = fun x -> 3
       module Y = struct
       end
     end
     |};
  [%expect
    {|
    ((TopMod X
       (MEStruct
         ((TopLet (PVar x) (EConst (CInt 1)))
           (TopLetRec ((y ((PBare x) (EConst (CInt 3))))))
           (TopMod Y (MEStruct ())))))) |}]

let%expect_test "Test: module expression" =
  let print_parsed str =
    parse_string_mod_expr str |> sexp_of_mod_expr |> print_sexp
  in
  print_parsed
    {|
     struct 
       let x = 1
       type t = int
       type () a 
         = Cons of int
         | Nil 
       end
     end
     |};
  [%expect
    {|
    (MEStruct
      ((TopLet (PVar x) (EConst (CInt 1)))
        (TopTypeDef (TDAlias t (TCons int ())))
        (TopTypeDef (TDAdt a () ((Cons ((TCons int ()))) (Nil ())))))) |}];
  print_parsed {|functor (X: M) -> struct end|};
  [%expect {| (MEFunctor ((X (MTName M)) (MEStruct ()))) |}]

let%expect_test "Test: module type" =
  let print_parsed str =
    parse_string_mod_type str |> sexp_of_mod_type |> print_sexp
  in
  print_parsed {|M|};
  [%expect {| (MTName M) |}];
  print_parsed {|M.X(M).E|};
  [%expect {| (MTField (PApply (PMem (PName M) X) (PName M)) E) |}];
  print_parsed {|sig val x : int end|};
  [%expect {| (MTSig ((TValueSpec x (TCons int ())))) |}];
  print_parsed {|functor (M:M) -> sig val x: int end|};
  [%expect {| (MTFunctor M (MTName M) (MTSig ((TValueSpec x (TCons int ()))))) |}];
  print_parsed {|functor (M:M) -> M1|};
  [%expect {| (MTFunctor M (MTName M) (MTName M1)) |}];
  print_parsed {|functor (M:functor (X:M)->M) -> M1|};
  [%expect {| (MTFunctor M (MTFunctor X (MTName M) (MTName M)) (MTName M1)) |}];
  print_parsed {|
                sig
                  val x : int
                  type t
                  val m: t -> t
                  type () i_list = 
                    Cons of int 
                    | Nil
                  end
                end
                |};
  [%expect {|
    (MTSig
      ((TValueSpec x (TCons int ())) (TAbstTySpec t)
        (TValueSpec m (TArrow (TCons t ()) (TCons t ())))
        (TManiTySpec (TDAdt i_list () ((Cons ((TCons int ()))) (Nil ())))))) |}]

