open Syntax.Parsing
open Syntax.Parsetree

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let print_parsed_program str =
  parse_string_program str |> sexp_of_program |> print_sexp

let print_parsed_mod_expr str =
  parse_string_mod_expr str |> sexp_of_mod_expr |> print_sexp

let print_parsed_type_expr str =
  parse_string_type_expr str |> sexp_of_ety |> print_sexp

let%expect_test "Test: expression parsing" =
  let print_parsed str =
    parse_string_expr str |> sexp_of_expr |> print_sexp
  in
  print_parsed "x";
  [%expect {| (EVar x) |}];
  print_parsed "1";
  [%expect {| (EConst (CInt 1)) |}];
  print_parsed {| () |};
  [%expect {| (EConst CUnit) |}];
  print_parsed {| "x \n \t"|};
  [%expect {| (EConst (CString "\"x \\n \\t\"")) |}];
  print_parsed {|a b c d|};
  print_parsed "true";
  [%expect
    {|
    (EApp (EApp (EApp (EVar a) (EVar b)) (EVar c)) (EVar d))
    (EConst (CBool true))
    |}];
  print_parsed "let x = 1 in y";
  [%expect {| (ELet x (EConst (CInt 1)) (EVar y)) |}];
  print_parsed {| Nil |};
  [%expect {|(ECons Nil)|}];
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
  print_parsed {|Cons (1)|};
  [%expect {| (EApp (ECons Cons) (EConst (CInt 1))) |}];
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
  [%expect {| (EApp (EField (MEName E) f) (EVar y)) |}];
  print_parsed {|Cons 1|};
  [%expect {| (EApp (ECons Cons) (EConst (CInt 1))) |}];
  print_parsed {|Cons (x, y)|};
  [%expect {| (EApp (ECons Cons) (ETuple ((EVar x) (EVar y)))) |}];
  print_parsed {|L.Cons (x, y)|};
  [%expect
    {| (EApp (EFieldCons (MEName L) Cons) (ETuple ((EVar x) (EVar y)))) |}];
  print_parsed {|fun x -> x|};
  [%expect {| (ELam ((PBare x) (EVar x))) |}];
  print_parsed {|f 1|};
  [%expect {| (EApp (EVar f) (EConst (CInt 1))) |}];
  print_parsed
    {|
         match c with
         | Cons x -> x
         | Nil    -> 0
                |};
  [%expect
    {|
    (ECase (EVar c)
      (((PCons Cons ((PVar x))) (EVar x)) ((PCons Nil ()) (EConst (CInt 0))))) |}];
  print_parsed {| x = y |};
  [%expect {| (ECmp Eq (EVar x) (EVar y)) |}];
  print_parsed {| x <> y |};
  [%expect {| (ECmp Neq (EVar x) (EVar y)) |}];
  print_parsed {|
   f 1 ; f 2 ; f 2 = 3
 |};
  [%expect {|
    (ESeq (EApp (EVar f) (EConst (CInt 1)))
      (ESeq (EApp (EVar f) (EConst (CInt 2)))
        (ECmp Eq (EApp (EVar f) (EConst (CInt 2))) (EConst (CInt 3)))))
    |}];
  print_parsed {|add x (minus x 1)|};
  [%expect {|
    (EApp (EApp (EVar add) (EVar x))
      (EApp (EApp (EVar minus) (EVar x)) (EConst (CInt 1))))
    |}]


let%expect_test "Test: pattern parsing" =
  let print_parsed str =
    parse_string_pattern str |> sexp_of_pattern |> print_sexp
  in
  print_parsed {| x |};
  [%expect {| (PVar x) |}];
  print_parsed {| 1 |};
  [%expect {| (PVal (CInt 1)) |}];
  print_parsed {| Nil |};
  [%expect {| (PCons Nil ()) |}];
  print_parsed {| Cons 1 |};
  [%expect {| (PCons Cons ((PVal (CInt 1)))) |}];
  print_parsed {| Cons x |};
  [%expect {| (PCons Cons ((PVar x))) |}];
  print_parsed {| Cons (x, y, z) |};
  [%expect {| (PCons Cons ((PTuple ((PVar x) (PVar y) (PVar z))))) |}]

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
         (even ((PBare x) (EApp (EVar odd) (EVar x))))))) |}];

  print_parsed_program
    {|
     module M = 
     struct 
       type () t = Nil

       let x = Nil
     end :
     sig
       type () t
       val x : t
     end 
     |};
  [%expect
    {|
    ((TopMod M
       (MERestrict
         (MEStruct ((TopTypeDef (TDAdt t () ((Nil ())))) (TopLet x (ECons Nil))))
         (MTSig ((TAbstTySpec t ()) (TValueSpec x (TCons t ()))))))) |}];

  print_parsed_program
    {|
     module type MIntf = 
     sig 
       type () t = Nil

       val x : t
     end 
     |};
  [%expect
    {|
    ((TopModSig MIntf
       (MTSig
         ((TManiTySpec (TDAdt t () ((Nil ())))) (TValueSpec x (TCons t ())))))) |}];

  print_parsed_program
    {|
module F =
functor
  (MI : I)
  ->
  (
    struct
      let x = 1

      let y = 1

      let z = 1
    end :
      J)
|};
  [%expect
    {|
    ((TopMod F
       (MEFunctor
         ((MI (MTName I))
           (MERestrict
             (MEStruct
               ((TopLet x (EConst (CInt 1))) (TopLet y (EConst (CInt 1)))
                 (TopLet z (EConst (CInt 1)))))
             (MTName J)))))) |}];

  print_parsed_program {|
    let co = Cons 1

    let f = 1
|};
  [%expect
    {|
    ((TopLet co (EApp (ECons Cons) (EConst (CInt 1))))
      (TopLet f (EConst (CInt 1)))) |}];

  print_parsed_program {|
       type t =  a list -> b
|};
  [%expect
    {| ((TopTypeDef (TDAlias t (TArrow (TCons list ((TCons a ()))) (TCons b ()))))) |}];

  print_parsed_program {|
  external x : int -> int -> int = "ff_add"
|};
  [%expect
    {|
    ((TopExternal x
       (TArrow (TCons int ()) (TArrow (TCons int ()) (TCons int ()))) ff_add))
    |}];

  print_parsed_program
    {|
let x = match a with
      | Cons -> 0

let y = 2
|};
  [%expect
    {|
    ((TopLet x (ECase (EVar a) (((PCons Cons ()) (EConst (CInt 0))))))
      (TopLet y (EConst (CInt 2))))
    |}];

  print_parsed_program {|
let x = fun x -> y

let y = 2
|};
  [%expect
    {| ((TopLet x (ELam ((PBare x) (EVar y)))) (TopLet y (EConst (CInt 2)))) |}];

  print_parsed_program
    {|
     let x = (1,2)

     let y = (1, 2)

     let z = match y with 
             | (x, y) -> x

     let n = fun y -> match y with 
       | (x, y) -> y

     let w = 1
     |};
  [%expect
    {|
    ((TopLet x (ETuple ((EConst (CInt 1)) (EConst (CInt 2)))))
      (TopLet y (ETuple ((EConst (CInt 1)) (EConst (CInt 2)))))
      (TopLet z (ECase (EVar y) (((PTuple ((PVar x) (PVar y))) (EVar x)))))
      (TopLet n
        (ELam
          ((PBare y) (ECase (EVar y) (((PTuple ((PVar x) (PVar y))) (EVar y)))))))
      (TopLet w (EConst (CInt 1))))
    |}];

print_parsed_program {|
let rec sum = fun x ->
    (if x = 0
    then 0
    else 1)

let result = print_int (sum 4)
|};
  [%expect {|
    ((TopLetRec
       ((sum
          ((PBare x)
            (EIf (ECmp Eq (EVar x) (EConst (CInt 0))) (EConst (CInt 0))
              (EConst (CInt 1)))))))
      (TopLet result (EApp (EVar print_int) (EApp (EVar sum) (EConst (CInt 4))))))
    |}]


let%expect_test "Test: path parsing" =
  print_parsed_mod_expr {|X|};
  [%expect {| (MEName X) |}];
  print_parsed_mod_expr {|X.Y|};
  [%expect {| (MEField (MEName X) Y) |}];
  print_parsed_mod_expr {|X(Y)|};
  [%expect {| (MEApply (MEName X) (MEName Y)) |}];
  print_parsed_mod_expr {|X.Y(Z(N))(W.M.N)|};
  [%expect
    {|
    (MEApply (MEApply (MEField (MEName X) Y) (MEApply (MEName Z) (MEName N)))
      (MEField (MEField (MEName W) M) N)) |}]

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
  [%expect {| (TField (MEName T) t ((TCons int ()) (TCons float ()))) |}];
  print_parsed_type_expr "int T(M).t";
  [%expect {| (TField (MEApply (MEName T) (MEName M)) t ((TCons int ()))) |}];
  print_parsed_type_expr "(int) T.t";
  [%expect {| (TField (MEName T) t ((TCons int ()))) |}]

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
       type () a = 
         | Cons of int
         | Nil 

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
    parse_string_mod_type str |> sexp_of_emod_ty |> print_sexp
  in
  print_parsed {|M|};
  [%expect {| (MTName M) |}];
  print_parsed {|M.X(M).E|};
  [%expect {| (MTField (MEApply (MEField (MEName M) X) (MEName M)) E) |}];
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
                  type () t
                  val m: t -> t
                  type () i_list = 
                    Cons of int 
                    | Nil

                end
                |};
  [%expect
    {|
    (MTSig
      ((TValueSpec x (TCons int ())) (TAbstTySpec t ())
        (TValueSpec m (TArrow (TCons t ()) (TCons t ())))
        (TManiTySpec (TDAdt i_list () ((Cons ((TCons int ()))) (Nil ())))))) |}]
