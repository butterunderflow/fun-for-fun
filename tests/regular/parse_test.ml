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
  [%expect
    {|
    ((node (EVar x))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (attrs ()))
    |}];
  print_parsed "1";
  [%expect
    {|
    ((node (EConst (CInt 1)))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (attrs ()))
    |}];
  print_parsed {| () |};
  [%expect
    {|
    ((node (EConst CUnit))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
      (attrs ()))
    |}];
  print_parsed {| "x \n \t"|};
  [%expect
    {|
    ((node (EConst (CString "\"x \\n \\t\"")))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
      (attrs ()))
    |}];
  print_parsed {|a b c d|};
  print_parsed "true";
  [%expect
    {|
    ((node
       (EApp
         ((node
            (EApp
              ((node
                 (EApp
                   ((node (EVar a))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                     (attrs ()))
                   ((node (EVar b))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                (attrs ()))
              ((node (EVar c))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
           (attrs ()))
         ((node (EVar d))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
      (attrs ()))
    ((node (EConst (CBool true)))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
      (attrs ()))
    |}];
  print_parsed "let x = 1 in y";
  [%expect
    {|
    ((node
       (ELet x
         ((node (EConst (CInt 1)))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
           (attrs ()))
         ((node (EVar y))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
      (attrs ()))
    |}];
  print_parsed {| Nil |};
  [%expect
    {|
    ((node (ECons Nil))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
      (attrs ()))
    |}];
  print_parsed "1,3,4,(5,6),7";
  [%expect
    {|
    ((node
       (ETuple
         (((node (EConst (CInt 1)))
            (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
            (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
            (attrs ()))
           ((node (EConst (CInt 3)))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
             (attrs ()))
           ((node (EConst (CInt 4)))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
             (attrs ()))
           ((node
              (ETuple
                (((node (EConst (CInt 5)))
                   (start_loc
                     ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                   (attrs ()))
                  ((node (EConst (CInt 6)))
                    (start_loc
                      ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
                    (attrs ())))))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
             (attrs ()))
           ((node (EConst (CInt 7)))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 12)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
             (attrs ())))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
      (attrs ()))
    |}];
  print_parsed "f 1, f true";
  [%expect
    {|
    ((node
       (ETuple
         (((node
             (EApp
               ((node (EVar f))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                 (attrs ()))
               ((node (EConst (CInt 1)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                 (attrs ()))))
            (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
            (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
            (attrs ()))
           ((node
              (EApp
                ((node (EVar f))
                  (start_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
                  (attrs ()))
                ((node (EConst (CBool true)))
                  (start_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
                  (attrs ()))))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
             (attrs ())))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
      (attrs ()))
    |}];
  print_parsed {|Cons (1)|};
  [%expect
    {|
    ((node
       (EApp
         ((node (ECons Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (attrs ()))
         ((node (EConst (CInt 1)))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
      (attrs ()))
    |}];
  print_parsed
    {|
     let rec odd = fun x -> even x
     and even = fun (x:int) -> odd x
     in
     odd 1
     |};
  [%expect
    {|
    ((node
       (ELetrec
         ((odd
            ((PBare x)
              ((node
                 (EApp
                   ((node (EVar even))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 29)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 33)))
                     (attrs ()))
                   ((node (EVar x))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 34)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 35)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 29)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 35)))
                (attrs ()))))
           (even
             ((PAnn x (TCons int ()))
               ((node
                  (EApp
                    ((node (EVar odd))
                      (start_loc
                        ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 67)))
                      (end_loc
                        ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 70)))
                      (attrs ()))
                    ((node (EVar x))
                      (start_loc
                        ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 71)))
                      (end_loc
                        ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 72)))
                      (attrs ()))))
                 (start_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 67)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 72)))
                 (attrs ())))))
         ((node
            (EApp
              ((node (EVar odd))
                (start_loc
                  ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 86)))
                (end_loc
                  ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 89)))
                (attrs ()))
              ((node (EConst (CInt 1)))
                (start_loc
                  ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 90)))
                (end_loc
                  ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 91)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 86)))
           (end_loc ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 91)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 6)))
      (end_loc ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 91)))
      (attrs ()))
    |}];
  print_parsed {|E.f y|};
  [%expect
    {|
    ((node
       (EApp
         ((node
            (EField
              ((node (MEName E))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                (attrs ()))
              f))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
           (attrs ()))
         ((node (EVar y))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
      (attrs ()))
    |}];
  print_parsed {|Cons 1|};
  [%expect
    {|
    ((node
       (EApp
         ((node (ECons Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (attrs ()))
         ((node (EConst (CInt 1)))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
      (attrs ()))
    |}];
  print_parsed {|Cons (x, y)|};
  [%expect
    {|
    ((node
       (EApp
         ((node (ECons Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (attrs ()))
         ((node
            (ETuple
              (((node (EVar x))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                 (attrs ()))
                ((node (EVar y))
                  (start_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
                  (attrs ())))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
      (attrs ()))
    |}];
  print_parsed {|L.Cons (x, y)|};
  [%expect
    {|
    ((node
       (EApp
         ((node
            (EFieldCons
              ((node (MEName L))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                (attrs ()))
              Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (attrs ()))
         ((node
            (ETuple
              (((node (EVar x))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                 (attrs ()))
                ((node (EVar y))
                  (start_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 12)))
                  (attrs ())))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 12)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
      (attrs ()))
    |}];
  print_parsed {|fun x -> x|};
  [%expect
    {|
    ((node
       (ELam
         ((PBare x)
           ((node (EVar x))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
             (attrs ())))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
      (attrs ()))
    |}];
  print_parsed {|f 1|};
  [%expect
    {|
    ((node
       (EApp
         ((node (EVar f))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (attrs ()))
         ((node (EConst (CInt 1)))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
      (attrs ()))
    |}];
  print_parsed
    {|
         match c with
         | Cons x -> x
         | Nil    -> 0
                |};
  [%expect
    {|
    ((node
       (ECase
         ((node (EVar c))
           (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
           (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 17)))
           (attrs ()))
         (((PCons Cons ((PVar x)))
            ((node (EVar x))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 23) (pos_cnum 44)))
              (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 23) (pos_cnum 45)))
              (attrs ())))
           ((PCons Nil ())
             ((node (EConst (CInt 0)))
               (start_loc
                 ((pos_fname "") (pos_lnum 4) (pos_bol 46) (pos_cnum 67)))
               (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 46) (pos_cnum 68)))
               (attrs ()))))))
      (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 10)))
      (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 46) (pos_cnum 68)))
      (attrs ()))
    |}];
  print_parsed {| x = y |};
  [%expect
    {|
    ((node
       (ECmp Eq
         ((node (EVar x))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
           (attrs ()))
         ((node (EVar y))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
      (attrs ()))
    |}];
  print_parsed {| x <> y |};
  [%expect
    {|
    ((node
       (ECmp Neq
         ((node (EVar x))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
           (attrs ()))
         ((node (EVar y))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
      (attrs ()))
    |}];
  print_parsed {|
   f 1 ; f 2 ; f 2 = 3
 |};
  [%expect
    {|
    ((node
       (ESeq
         ((node
            (EApp
              ((node (EVar f))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 4)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 5)))
                (attrs ()))
              ((node (EConst (CInt 1)))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 6)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 7)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 4)))
           (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 7)))
           (attrs ()))
         ((node
            (ESeq
              ((node
                 (EApp
                   ((node (EVar f))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 10)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 11)))
                     (attrs ()))
                   ((node (EConst (CInt 2)))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 12)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 13)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 10)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 13)))
                (attrs ()))
              ((node
                 (ECmp Eq
                   ((node
                      (EApp
                        ((node (EVar f))
                          (start_loc
                            ((pos_fname "") (pos_lnum 2) (pos_bol 1)
                              (pos_cnum 16)))
                          (end_loc
                            ((pos_fname "") (pos_lnum 2) (pos_bol 1)
                              (pos_cnum 17)))
                          (attrs ()))
                        ((node (EConst (CInt 2)))
                          (start_loc
                            ((pos_fname "") (pos_lnum 2) (pos_bol 1)
                              (pos_cnum 18)))
                          (end_loc
                            ((pos_fname "") (pos_lnum 2) (pos_bol 1)
                              (pos_cnum 19)))
                          (attrs ()))))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 19)))
                     (attrs ()))
                   ((node (EConst (CInt 3)))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 22)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 23)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 23)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 10)))
           (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 23)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 4)))
      (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 23)))
      (attrs ()))
    |}];
  print_parsed {|add x (minus x 1)|};
  [%expect
    {|
    ((node
       (EApp
         ((node
            (EApp
              ((node (EVar add))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                (attrs ()))
              ((node (EVar x))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
           (attrs ()))
         ((node
            (EApp
              ((node
                 (EApp
                   ((node (EVar minus))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 12)))
                     (attrs ()))
                   ((node (EVar x))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
                (attrs ()))
              ((node (EConst (CInt 1)))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
      (attrs ()))
    |}];
  print_parsed {|a b Nil|};
  [%expect
    {|
    ((node
       (EApp
         ((node
            (EApp
              ((node (EVar a))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                (attrs ()))
              ((node (EVar b))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
           (attrs ()))
         ((node (ECons Nil))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
      (attrs ()))
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
  [%expect
    {|
    ((TopLet x
       ((node (EConst (CInt 1)))
         (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
         (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
         (attrs ()))))
    |}];
  print_parsed_program
    {|
     let x = 1
     let y = 2
     let rec foo = fun x -> foo x
     |};
  [%expect
    {|
    ((TopLet x
       ((node (EConst (CInt 1)))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 14)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
         (attrs ())))
      (TopLet y
        ((node (EConst (CInt 2)))
          (start_loc ((pos_fname "") (pos_lnum 3) (pos_bol 16) (pos_cnum 29)))
          (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 16) (pos_cnum 30)))
          (attrs ())))
      (TopLetRec
        ((foo
           ((PBare x)
             ((node
                (EApp
                  ((node (EVar foo))
                    (start_loc
                      ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 59)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 62)))
                    (attrs ()))
                  ((node (EVar x))
                    (start_loc
                      ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 63)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 64)))
                    (attrs ()))))
               (start_loc
                 ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 59)))
               (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 64)))
               (attrs ())))))))
    |}];
  print_parsed_program {|let rec f = fun (x:int) -> 1|};
  [%expect
    {|
    ((TopLetRec
       ((f
          ((PAnn x (TCons int ()))
            ((node (EConst (CInt 1)))
              (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 27)))
              (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
              (attrs ())))))))
    |}];
  print_parsed_program
    {|
    let rec odd = fun x -> even x
    and even = fun x -> odd x
     |};
  [%expect
    {|
    ((TopLetRec
       ((odd
          ((PBare x)
            ((node
               (EApp
                 ((node (EVar even))
                   (start_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 28)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 32)))
                   (attrs ()))
                 ((node (EVar x))
                   (start_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 33)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 34)))
                   (attrs ()))))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 28)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 34)))
              (attrs ()))))
         (even
           ((PBare x)
             ((node
                (EApp
                  ((node (EVar odd))
                    (start_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 59)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 62)))
                    (attrs ()))
                  ((node (EVar x))
                    (start_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 63)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 64)))
                    (attrs ()))))
               (start_loc
                 ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 59)))
               (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 64)))
               (attrs ())))))))
    |}];

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
       ((node
          (MERestrict
            ((node
               (MEStruct
                 ((TopTypeDef (TDAdt t () ((Nil ()))))
                   (TopLet x
                     ((node (ECons Nil))
                       (start_loc
                         ((pos_fname "") (pos_lnum 6) (pos_bol 55) (pos_cnum 70)))
                       (end_loc
                         ((pos_fname "") (pos_lnum 6) (pos_bol 55) (pos_cnum 73)))
                       (attrs ()))))))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 18) (pos_cnum 23)))
              (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 74) (pos_cnum 82)))
              (attrs ()))
            (MTSig ((TAbstTySpec t ()) (TValueSpec x (TCons t ()))))))
         (start_loc ((pos_fname "") (pos_lnum 3) (pos_bol 18) (pos_cnum 23)))
         (end_loc ((pos_fname "") (pos_lnum 11) (pos_bol 128) (pos_cnum 136)))
         (attrs ()))))
    |}];

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
         ((TManiTySpec (TDAdt t () ((Nil ())))) (TValueSpec x (TCons t ()))))))
    |}];

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
       ((node
          (MEFunctor
            ((MI (MTName I))
              ((node
                 (MERestrict
                   ((node
                      (MEStruct
                        ((TopLet x
                           ((node (EConst (CInt 1)))
                             (start_loc
                               ((pos_fname "") (pos_lnum 8) (pos_bol 51)
                                 (pos_cnum 65)))
                             (end_loc
                               ((pos_fname "") (pos_lnum 8) (pos_bol 51)
                                 (pos_cnum 66)))
                             (attrs ())))
                          (TopLet y
                            ((node (EConst (CInt 1)))
                              (start_loc
                                ((pos_fname "") (pos_lnum 10) (pos_bol 68)
                                  (pos_cnum 82)))
                              (end_loc
                                ((pos_fname "") (pos_lnum 10) (pos_bol 68)
                                  (pos_cnum 83)))
                              (attrs ())))
                          (TopLet z
                            ((node (EConst (CInt 1)))
                              (start_loc
                                ((pos_fname "") (pos_lnum 12) (pos_bol 85)
                                  (pos_cnum 99)))
                              (end_loc
                                ((pos_fname "") (pos_lnum 12) (pos_bol 85)
                                  (pos_cnum 100)))
                              (attrs ()))))))
                     (start_loc
                       ((pos_fname "") (pos_lnum 7) (pos_bol 40) (pos_cnum 44)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 13) (pos_bol 101)
                         (pos_cnum 108)))
                     (attrs ()))
                   (MTName J)))
                (start_loc
                  ((pos_fname "") (pos_lnum 7) (pos_bol 40) (pos_cnum 44)))
                (end_loc
                  ((pos_fname "") (pos_lnum 14) (pos_bol 111) (pos_cnum 118)))
                (attrs ())))))
         (start_loc ((pos_fname "") (pos_lnum 3) (pos_bol 12) (pos_cnum 12)))
         (end_loc ((pos_fname "") (pos_lnum 14) (pos_bol 111) (pos_cnum 119)))
         (attrs ()))))
    |}];

  print_parsed_program {|
    let co = Cons 1

    let f = 1
|};
  [%expect
    {|
    ((TopLet co
       ((node
          (EApp
            ((node (ECons Cons))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 14)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
              (attrs ()))
            ((node (EConst (CInt 1)))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 19)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 20)))
              (attrs ()))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 14)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 20)))
         (attrs ())))
      (TopLet f
        ((node (EConst (CInt 1)))
          (start_loc ((pos_fname "") (pos_lnum 4) (pos_bol 22) (pos_cnum 34)))
          (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 22) (pos_cnum 35)))
          (attrs ()))))
    |}];

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
    ((TopLet x
       ((node
          (ECase
            ((node (EVar a))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
              (attrs ()))
            (((PCons Cons ())
               ((node (EConst (CInt 0)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 22) (pos_cnum 38)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 22) (pos_cnum 39)))
                 (attrs ()))))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 9)))
         (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 22) (pos_cnum 39)))
         (attrs ())))
      (TopLet y
        ((node (EConst (CInt 2)))
          (start_loc ((pos_fname "") (pos_lnum 5) (pos_bol 41) (pos_cnum 49)))
          (end_loc ((pos_fname "") (pos_lnum 5) (pos_bol 41) (pos_cnum 50)))
          (attrs ()))))
    |}];

  print_parsed_program {|
let x = fun x -> y

let y = 2
|};
  [%expect
    {|
    ((TopLet x
       ((node
          (ELam
            ((PBare x)
              ((node (EVar y))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 19)))
                (attrs ())))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 9)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 19)))
         (attrs ())))
      (TopLet y
        ((node (EConst (CInt 2)))
          (start_loc ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 29)))
          (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 30)))
          (attrs ()))))
    |}];

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
    ((TopLet x
       ((node
          (ETuple
            (((node (EConst (CInt 1)))
               (start_loc
                 ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
               (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
               (attrs ()))
              ((node (EConst (CInt 2)))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 17)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
                (attrs ())))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
         (attrs ())))
      (TopLet y
        ((node
           (ETuple
             (((node (EConst (CInt 1)))
                (start_loc
                  ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 35)))
                (end_loc
                  ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 36)))
                (attrs ()))
               ((node (EConst (CInt 2)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 38)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 39)))
                 (attrs ())))))
          (start_loc ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 35)))
          (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 39)))
          (attrs ())))
      (TopLet z
        ((node
           (ECase
             ((node (EVar y))
               (start_loc
                 ((pos_fname "") (pos_lnum 6) (pos_bol 42) (pos_cnum 61)))
               (end_loc ((pos_fname "") (pos_lnum 6) (pos_bol 42) (pos_cnum 62)))
               (attrs ()))
             (((PTuple ((PVar x) (PVar y)))
                ((node (EVar x))
                  (start_loc
                    ((pos_fname "") (pos_lnum 7) (pos_bol 69) (pos_cnum 94)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 7) (pos_bol 69) (pos_cnum 95)))
                  (attrs ()))))))
          (start_loc ((pos_fname "") (pos_lnum 6) (pos_bol 42) (pos_cnum 55)))
          (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 69) (pos_cnum 95)))
          (attrs ())))
      (TopLet n
        ((node
           (ELam
             ((PBare y)
               ((node
                  (ECase
                    ((node (EVar y))
                      (start_loc
                        ((pos_fname "") (pos_lnum 9) (pos_bol 97) (pos_cnum 125)))
                      (end_loc
                        ((pos_fname "") (pos_lnum 9) (pos_bol 97) (pos_cnum 126)))
                      (attrs ()))
                    (((PTuple ((PVar x) (PVar y)))
                       ((node (EVar y))
                         (start_loc
                           ((pos_fname "") (pos_lnum 10) (pos_bol 133)
                             (pos_cnum 152)))
                         (end_loc
                           ((pos_fname "") (pos_lnum 10) (pos_bol 133)
                             (pos_cnum 153)))
                         (attrs ()))))))
                 (start_loc
                   ((pos_fname "") (pos_lnum 9) (pos_bol 97) (pos_cnum 119)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 10) (pos_bol 133) (pos_cnum 153)))
                 (attrs ())))))
          (start_loc ((pos_fname "") (pos_lnum 9) (pos_bol 97) (pos_cnum 110)))
          (end_loc ((pos_fname "") (pos_lnum 10) (pos_bol 133) (pos_cnum 153)))
          (attrs ())))
      (TopLet w
        ((node (EConst (CInt 1)))
          (start_loc ((pos_fname "") (pos_lnum 12) (pos_bol 155) (pos_cnum 168)))
          (end_loc ((pos_fname "") (pos_lnum 12) (pos_bol 155) (pos_cnum 169)))
          (attrs ()))))
    |}];

  print_parsed_program
    {|
let rec sum = fun x ->
    (if x = 0
    then 0
    else 1)

let result = print_int (sum 4)
|};
  [%expect
    {|
    ((TopLetRec
       ((sum
          ((PBare x)
            ((node
               (EIf
                 ((node
                    (ECmp Eq
                      ((node (EVar x))
                        (start_loc
                          ((pos_fname "") (pos_lnum 3) (pos_bol 24)
                            (pos_cnum 32)))
                        (end_loc
                          ((pos_fname "") (pos_lnum 3) (pos_bol 24)
                            (pos_cnum 33)))
                        (attrs ()))
                      ((node (EConst (CInt 0)))
                        (start_loc
                          ((pos_fname "") (pos_lnum 3) (pos_bol 24)
                            (pos_cnum 36)))
                        (end_loc
                          ((pos_fname "") (pos_lnum 3) (pos_bol 24)
                            (pos_cnum 37)))
                        (attrs ()))))
                   (start_loc
                     ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 32)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 37)))
                   (attrs ()))
                 ((node (EConst (CInt 0)))
                   (start_loc
                     ((pos_fname "") (pos_lnum 4) (pos_bol 38) (pos_cnum 47)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 4) (pos_bol 38) (pos_cnum 48)))
                   (attrs ()))
                 ((node (EConst (CInt 1)))
                   (start_loc
                     ((pos_fname "") (pos_lnum 5) (pos_bol 49) (pos_cnum 58)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 5) (pos_bol 49) (pos_cnum 59)))
                   (attrs ()))))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 29)))
              (end_loc ((pos_fname "") (pos_lnum 5) (pos_bol 49) (pos_cnum 59)))
              (attrs ()))))))
      (TopLet result
        ((node
           (EApp
             ((node (EVar print_int))
               (start_loc
                 ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 75)))
               (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 84)))
               (attrs ()))
             ((node
                (EApp
                  ((node (EVar sum))
                    (start_loc
                      ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 86)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 89)))
                    (attrs ()))
                  ((node (EConst (CInt 4)))
                    (start_loc
                      ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 90)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 91)))
                    (attrs ()))))
               (start_loc
                 ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 86)))
               (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 91)))
               (attrs ()))))
          (start_loc ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 75)))
          (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 92)))
          (attrs ()))))
    |}];
  print_parsed_program {|
                type 'a t = Nil
                        |};
  [%expect {| ((TopTypeDef (TDAdt t ('a/0) ((Nil ()))))) |}];
  print_parsed_program {|
                type t = | Nil
                        |};
  [%expect {| ((TopTypeDef (TDAdt t () ((Nil ()))))) |}]


let%expect_test "Test: path parsing" =
  print_parsed_mod_expr {|X|};
  [%expect
    {|
    ((node (MEName X))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (attrs ()))
    |}];
  print_parsed_mod_expr {|X.Y|};
  [%expect
    {|
    ((node
       (MEField
         ((node (MEName X))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (attrs ()))
         Y))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
      (attrs ()))
    |}];
  print_parsed_mod_expr {|X(Y)|};
  [%expect
    {|
    ((node
       (MEApply
         ((node (MEName X))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (attrs ()))
         ((node (MEName Y))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
      (attrs ()))
    |}];
  print_parsed_mod_expr {|X.Y(Z(N))(W.M.N)|};
  [%expect
    {|
    ((node
       (MEApply
         ((node
            (MEApply
              ((node
                 (MEField
                   ((node (MEName X))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                     (attrs ()))
                   Y))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                (attrs ()))
              ((node
                 (MEApply
                   ((node (MEName Z))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                     (attrs ()))
                   ((node (MEName N))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
           (attrs ()))
         ((node
            (MEField
              ((node
                 (MEField
                   ((node (MEName W))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
                     (attrs ()))
                   M))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
                (attrs ()))
              N))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
      (attrs ()))
    |}]

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
  [%expect
    {|
    (TField
      ((node (MEName T))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
        (attrs ()))
      t ((TCons int ()) (TCons float ())))
    |}];
  print_parsed_type_expr "int T(M).t";
  [%expect
    {|
    (TField
      ((node
         (MEApply
           ((node (MEName T))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
             (attrs ()))
           ((node (MEName M))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
             (attrs ()))))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
        (attrs ()))
      t ((TCons int ())))
    |}];
  print_parsed_type_expr "(int) T.t";
  [%expect
    {|
    (TField
      ((node (MEName T))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
        (attrs ()))
      t ((TCons int ())))
    |}]

let%expect_test "Test: top level module" =
  print_parsed_program {|
     module X = struct
     end
     |};
  [%expect
    {|
    ((TopMod X
       ((node (MEStruct ()))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 17)))
         (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 32)))
         (attrs ()))))
    |}];

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
       ((node
          (MEStruct
            ((TopLet x
               ((node (EConst (CInt 1)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 39)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 40)))
                 (attrs ())))
              (TopLetRec
                ((y
                   ((PBare x)
                     ((node (EConst (CInt 3)))
                       (start_loc
                         ((pos_fname "") (pos_lnum 4) (pos_bol 41) (pos_cnum 69)))
                       (end_loc
                         ((pos_fname "") (pos_lnum 4) (pos_bol 41) (pos_cnum 70)))
                       (attrs ()))))))
              (TopMod Y
                ((node (MEStruct ()))
                  (start_loc
                    ((pos_fname "") (pos_lnum 5) (pos_bol 71) (pos_cnum 89)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 6) (pos_bol 96) (pos_cnum 106)))
                  (attrs ()))))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 17)))
         (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 107) (pos_cnum 115)))
         (attrs ()))))
    |}]

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
    ((node
       (MEStruct
         ((TopLet x
            ((node (EConst (CInt 1)))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 14) (pos_cnum 29)))
              (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 14) (pos_cnum 30)))
              (attrs ())))
           (TopTypeDef (TDAdt a () ((Cons ((TCons int ()))) (Nil ())))))))
      (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 6)))
      (end_loc ((pos_fname "") (pos_lnum 8) (pos_bol 91) (pos_cnum 99)))
      (attrs ()))
    |}];
  print_parsed {|functor (X: M) -> struct end|};
  [%expect
    {|
    ((node
       (MEFunctor
         ((X (MTName M))
           ((node (MEStruct ()))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
             (attrs ())))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
      (attrs ()))
    |}]

let%expect_test "Test: module type" =
  let print_parsed str =
    parse_string_mod_type str |> sexp_of_emod_ty |> print_sexp
  in
  print_parsed {|M|};
  [%expect {| (MTName M) |}];
  print_parsed {|M.X(M).E|};
  [%expect
    {|
    (MTField
      ((node
         (MEApply
           ((node
              (MEField
                ((node (MEName M))
                  (start_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                  (attrs ()))
                X))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
             (attrs ()))
           ((node (MEName M))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
             (attrs ()))))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
        (attrs ()))
      E)
    |}];
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
