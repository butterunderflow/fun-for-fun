open Syntax.Parsing
open Syntax.Parsetree

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let print_parsed_program str =
  parse_string_program str |> sexp_of_program |> print_sexp

let print_parsed_path str =
  parse_string_path str |> sexp_of_path |> print_sexp

let print_parsed_type_expr str =
  parse_string_type_expr str |> sexp_of_type_expr |> print_sexp

let%expect_test "Test: expression parsing" =
  let print_parsed str =
    parse_string_expr str |> sexp_of_expr |> print_sexp
  in
  print_parsed "x";
  [%expect {| (EVar x) |}];
  print_parsed "1";
  [%expect {| (EConst (CInt 1)) |}];
  print_parsed "true";
  [%expect {| (EConst (CBool true)) |}];
  print_parsed "let x = 1 in y";
  [%expect {| (ELet x (EConst (CInt 1)) (EVar y)) |}];
  print_parsed "1,3,4,(5,6),7";
  [%expect
    {|
    (ETuple
      ((EConst (CInt 1)) (EConst (CInt 3)) (EConst (CInt 4))
        (ETuple ((EConst (CInt 5)) (EConst (CInt 6)))) (EConst (CInt 7)))) |}];
  print_parsed "f 1, f true";
  [%expect
    {|
    (ETuple
      ((EApp (EVar f) (EConst (CInt 1))) (EApp (EVar f) (EConst (CBool true))))) |}];
  print_parsed
    {|
     let rec odd = fun x -> even x
     and even = fun (x:int) -> odd x
     in
     odd 1
     |};
  [%expect
    {|
    (ELetrec
      ((odd ((PBare x) (EApp (EVar even) (EVar x))))
        (even ((PAnn x (TCons int ())) (EApp (EVar odd) (EVar x)))))
      (EApp (EVar odd) (EConst (CInt 1)))) |}];
  print_parsed {|E.f y|};
  [%expect {| (EApp (EField (PName E) f) (EVar y)) |}];
  print_parsed {|Cons (x, y)|};
  [%expect {| (EApp (ECons Cons) (ETuple ((EVar x) (EVar y)))) |}];
  print_parsed {|L.Cons (x, y)|};
  [%expect
    {| (EApp (EFieldCons (PName L) Cons) (ETuple ((EVar x) (EVar y)))) |}];
  print_parsed {|fun x -> x|};
  [%expect {| (ELam ((PBare x) (EVar x))) |}];
  print_parsed {|f 1|};
  [%expect {| (EApp (EVar f) (EConst (CInt 1))) |}]

let%expect_test "Test: full program parsing" =
  print_parsed_program {|let x = 1|};
  [%expect {|
    ((TopLet x (EConst (CInt 1))))
  |}];
  print_parsed_program
    {|
     let x = 1
     let y = 2
     let rec foo = fun x -> foo x
     |};
  [%expect
    {|
    ((TopLet x (EConst (CInt 1))) (TopLet y (EConst (CInt 2)))
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
  [%expect {| (TVar 'x/0) |}];
  print_parsed_type_expr "(string, 'x, 'y) list";
  [%expect {| (TCons list ((TCons string ()) (TVar 'x/0) (TVar 'y/0))) |}];
  print_parsed_type_expr "int * int";
  [%expect {| (TTuple ((TCons int ()) (TCons int ()))) |}];
  print_parsed_type_expr "int * i list * (x * y) list * (t1 * t2)";
  [%expect
    {|
    (TTuple
      ((TCons int ()) (TCons list ((TCons i ())))
        (TCons list ((TTuple ((TCons x ()) (TCons y ())))))
        (TTuple ((TCons t1 ()) (TCons t2 ()))))) |}];
  print_parsed_type_expr "{x: int; y: float; z: int -> float }";
  [%expect
    {|
    (TRecord
      ((x (TCons int ())) (y (TCons float ()))
        (z (TArrow (TCons int ()) (TCons float ()))))) |}];
  print_parsed_type_expr "(int, float) T.t";
  [%expect {| (TField (PName T) t ((TCons int ()) (TCons float ()))) |}];
  print_parsed_type_expr "int T(M).t";
  [%expect {| (TField (PApply (PName T) (PName M)) t ((TCons int ()))) |}];
  print_parsed_type_expr "(int) T.t";
  [%expect {| (TField (PName T) t ((TCons int ()))) |}]

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
         ((TopLet x (EConst (CInt 1)))
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
       type () a 
         = Cons of int
         | Nil 
       end
     end
     |};
  [%expect
    {|
    (MEStruct
      ((TopLet x (EConst (CInt 1)))
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
  [%expect
    {| (MTFunctor M (MTName M) (MTSig ((TValueSpec x (TCons int ()))))) |}];
  print_parsed {|functor (M:M) -> M1|};
  [%expect {| (MTFunctor M (MTName M) (MTName M1)) |}];
  print_parsed {|functor (M:functor (X:M)->M) -> M1|};
  [%expect
    {| (MTFunctor M (MTFunctor X (MTName M) (MTName M)) (MTName M1)) |}];
  print_parsed
    {|
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
  [%expect
    {|
    (MTSig
      ((TValueSpec x (TCons int ())) (TAbstTySpec t)
        (TValueSpec m (TArrow (TCons t ()) (TCons t ())))
        (TManiTySpec (TDAdt i_list () ((Cons ((TCons int ()))) (Nil ())))))) |}]
