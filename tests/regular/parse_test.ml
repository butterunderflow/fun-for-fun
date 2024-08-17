open Syntax.Parsing
open Syntax.Parsetree

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let print_parsed_program str =
  parse_string_program str |> sexp_of_program |> print_sexp

let print_parsed_mod_expr str =
  parse_string_mod_expr str |> sexp_of_mod_expr |> print_sexp

let print_parsed_type_expr str =
  parse_string_type_expr str |> sexp_of_surface_ty |> print_sexp

let%expect_test "Test: expression parsing" =
  let print_parsed str =
    parse_string_expr str |> sexp_of_expr |> print_sexp
  in
  print_parsed "x";
  [%expect
    {|
    ((desc (Exp_var x))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (attrs ()))
    |}];
  print_parsed "1";
  [%expect
    {|
    ((desc (Exp_const (Const_int 1)))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (attrs ()))
    |}];
  print_parsed {| () |};
  [%expect
    {|
    ((desc (Exp_const Const_unit))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
      (attrs ()))
    |}];
  print_parsed {| "x \n \t,()*@/"|};
  [%expect
    {|
    ((desc (Exp_const (Const_string "\"x \\n \\t,()*@/\"")))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
      (attrs ()))
    |}];
  print_parsed {|a b c d|};
  print_parsed "true";
  [%expect
    {|
    ((desc
       (Exp_app
         ((desc
            (Exp_app
              ((desc
                 (Exp_app
                   ((desc (Exp_var a))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                     (attrs ()))
                   ((desc (Exp_var b))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                (attrs ()))
              ((desc (Exp_var c))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
           (attrs ()))
         ((desc (Exp_var d))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
           (attrs ()))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
      (attrs ()))
    ((desc (Exp_const (Const_bool true)))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
      (attrs ()))
    |}];
  print_parsed "let x = 1 in y";
  [%expect
    {|
    ((desc
       (Exp_let x
         ((desc (Exp_const (Const_int 1)))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
           (attrs ()))
         ((desc (Exp_var y))
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
    ((desc (Exp_constr Nil))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
      (attrs ()))
    |}];
  print_parsed "1,3,4,(5,6),7";
  [%expect
    {|
    ((desc
       (Exp_tuple
         (((desc (Exp_const (Const_int 1)))
            (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
            (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
            (attrs ()))
           ((desc (Exp_const (Const_int 3)))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
             (attrs ()))
           ((desc (Exp_const (Const_int 4)))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
             (attrs ()))
           ((desc
              (Exp_tuple
                (((desc (Exp_const (Const_int 5)))
                   (start_loc
                     ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                   (attrs ()))
                  ((desc (Exp_const (Const_int 6)))
                    (start_loc
                      ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
                    (attrs ())))))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
             (attrs ()))
           ((desc (Exp_const (Const_int 7)))
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
    ((desc
       (Exp_tuple
         (((desc
             (Exp_app
               ((desc (Exp_var f))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                 (attrs ()))
               ((desc (Exp_const (Const_int 1)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                 (attrs ()))))
            (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
            (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
            (attrs ()))
           ((desc
              (Exp_app
                ((desc (Exp_var f))
                  (start_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
                  (attrs ()))
                ((desc (Exp_const (Const_bool true)))
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
    ((desc
       (Exp_app
         ((desc (Exp_constr Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (attrs ()))
         ((desc (Exp_const (Const_int 1)))
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
    ((desc
       (Exp_letrec
         ((odd
            ((Para_bare x)
              ((desc
                 (Exp_app
                   ((desc (Exp_var even))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 29)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 33)))
                     (attrs ()))
                   ((desc (Exp_var x))
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
             ((Para_ann x (Ty_cons int ()))
               ((desc
                  (Exp_app
                    ((desc (Exp_var odd))
                      (start_loc
                        ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 67)))
                      (end_loc
                        ((pos_fname "") (pos_lnum 3) (pos_bol 36) (pos_cnum 70)))
                      (attrs ()))
                    ((desc (Exp_var x))
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
         ((desc
            (Exp_app
              ((desc (Exp_var odd))
                (start_loc
                  ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 86)))
                (end_loc
                  ((pos_fname "") (pos_lnum 5) (pos_bol 81) (pos_cnum 89)))
                (attrs ()))
              ((desc (Exp_const (Const_int 1)))
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
    ((desc
       (Exp_app
         ((desc
            (Exp_field
              ((desc (Mod_name E))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                (attrs ()))
              f))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
           (attrs ()))
         ((desc (Exp_var y))
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
    ((desc
       (Exp_app
         ((desc (Exp_constr Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (attrs ()))
         ((desc (Exp_const (Const_int 1)))
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
    ((desc
       (Exp_app
         ((desc (Exp_constr Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
           (attrs ()))
         ((desc
            (Exp_tuple
              (((desc (Exp_var x))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                 (attrs ()))
                ((desc (Exp_var y))
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
    ((desc
       (Exp_app
         ((desc
            (Exp_field_constr
              ((desc (Mod_name L))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                (attrs ()))
              Cons))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
           (attrs ()))
         ((desc
            (Exp_tuple
              (((desc (Exp_var x))
                 (start_loc
                   ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                 (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                 (attrs ()))
                ((desc (Exp_var y))
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
    ((desc
       (Exp_lam
         ((Para_bare x)
           ((desc (Exp_var x))
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
    ((desc
       (Exp_app
         ((desc (Exp_var f))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (attrs ()))
         ((desc (Exp_const (Const_int 1)))
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
    ((desc
       (Exp_case
         ((desc (Exp_var c))
           (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
           (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 17)))
           (attrs ()))
         (((Pat_constr Cons ((Pat_var x)))
            ((desc (Exp_var x))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 23) (pos_cnum 44)))
              (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 23) (pos_cnum 45)))
              (attrs ())))
           ((Pat_constr Nil ())
             ((desc (Exp_const (Const_int 0)))
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
    ((desc
       (Exp_cmp Eq
         ((desc (Exp_var x))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
           (attrs ()))
         ((desc (Exp_var y))
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
    ((desc
       (Exp_cmp Neq
         ((desc (Exp_var x))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
           (attrs ()))
         ((desc (Exp_var y))
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
    ((desc
       (Exp_seq
         ((desc
            (Exp_app
              ((desc (Exp_var f))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 4)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 5)))
                (attrs ()))
              ((desc (Exp_const (Const_int 1)))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 6)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 7)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 4)))
           (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 7)))
           (attrs ()))
         ((desc
            (Exp_seq
              ((desc
                 (Exp_app
                   ((desc (Exp_var f))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 10)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 11)))
                     (attrs ()))
                   ((desc (Exp_const (Const_int 2)))
                     (start_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 12)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 13)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 10)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 13)))
                (attrs ()))
              ((desc
                 (Exp_cmp Eq
                   ((desc
                      (Exp_app
                        ((desc (Exp_var f))
                          (start_loc
                            ((pos_fname "") (pos_lnum 2) (pos_bol 1)
                              (pos_cnum 16)))
                          (end_loc
                            ((pos_fname "") (pos_lnum 2) (pos_bol 1)
                              (pos_cnum 17)))
                          (attrs ()))
                        ((desc (Exp_const (Const_int 2)))
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
                   ((desc (Exp_const (Const_int 3)))
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
    ((desc
       (Exp_app
         ((desc
            (Exp_app
              ((desc (Exp_var add))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                (attrs ()))
              ((desc (Exp_var x))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
           (attrs ()))
         ((desc
            (Exp_app
              ((desc
                 (Exp_app
                   ((desc (Exp_var minus))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 12)))
                     (attrs ()))
                   ((desc (Exp_var x))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
                     (attrs ()))))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
                (attrs ()))
              ((desc (Exp_const (Const_int 1)))
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
    ((desc
       (Exp_app
         ((desc
            (Exp_app
              ((desc (Exp_var a))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                (attrs ()))
              ((desc (Exp_var b))
                (start_loc
                  ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 2)))
                (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
                (attrs ()))))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
           (attrs ()))
         ((desc (Exp_constr Nil))
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
  [%expect {| (Pat_var x) |}];
  print_parsed {| 1 |};
  [%expect {| (Pat_val (Const_int 1)) |}];
  print_parsed {| Nil |};
  [%expect {| (Pat_constr Nil ()) |}];
  print_parsed {| Cons 1 |};
  [%expect {| (Pat_constr Cons ((Pat_val (Const_int 1)))) |}];
  print_parsed {| Cons x |};
  [%expect {| (Pat_constr Cons ((Pat_var x))) |}];
  print_parsed {| Cons (x, y, z) |};
  [%expect
    {| (Pat_constr Cons ((Pat_tuple ((Pat_var x) (Pat_var y) (Pat_var z))))) |}]

let%expect_test "Test: full program parsing" =
  print_parsed_program
    {|
     (* xyz"abcde\n\\\de" in*) let (* xyz *) x (* xyz *) = (* xyz *) 1 (* xyz *)
     |};
  print_parsed_program {|let x = 1|};
  [%expect
    {|
    ((Top_let x
       ((desc (Exp_const (Const_int 1)))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 70)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 71)))
         (attrs ()))))
    ((Top_let x
       ((desc (Exp_const (Const_int 1)))
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
    ((Top_let x
       ((desc (Exp_const (Const_int 1)))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 14)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
         (attrs ())))
      (Top_let y
        ((desc (Exp_const (Const_int 2)))
          (start_loc ((pos_fname "") (pos_lnum 3) (pos_bol 16) (pos_cnum 29)))
          (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 16) (pos_cnum 30)))
          (attrs ())))
      (Top_letrec
        ((foo
           ((Para_bare x)
             ((desc
                (Exp_app
                  ((desc (Exp_var foo))
                    (start_loc
                      ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 59)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 4) (pos_bol 31) (pos_cnum 62)))
                    (attrs ()))
                  ((desc (Exp_var x))
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
    ((Top_letrec
       ((f
          ((Para_ann x (Ty_cons int ()))
            ((desc (Exp_const (Const_int 1)))
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
    ((Top_letrec
       ((odd
          ((Para_bare x)
            ((desc
               (Exp_app
                 ((desc (Exp_var even))
                   (start_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 28)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 32)))
                   (attrs ()))
                 ((desc (Exp_var x))
                   (start_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 33)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 34)))
                   (attrs ()))))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 28)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 34)))
              (attrs ()))))
         (even
           ((Para_bare x)
             ((desc
                (Exp_app
                  ((desc (Exp_var odd))
                    (start_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 59)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 35) (pos_cnum 62)))
                    (attrs ()))
                  ((desc (Exp_var x))
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
    ((Top_mod M
       ((desc
          (Mod_restrict
            ((desc
               (Mod_struct
                 ((Top_type_def (Ty_def_adt t () ((Nil ()))))
                   (Top_let x
                     ((desc (Exp_constr Nil))
                       (start_loc
                         ((pos_fname "") (pos_lnum 6) (pos_bol 55) (pos_cnum 70)))
                       (end_loc
                         ((pos_fname "") (pos_lnum 6) (pos_bol 55) (pos_cnum 73)))
                       (attrs ()))))))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 18) (pos_cnum 23)))
              (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 74) (pos_cnum 82)))
              (attrs ()))
            (Mod_ty_sig ((Spec_abstr t ()) (Spec_value x (Ty_cons t ()))))))
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
    ((Top_mod_sig MIntf
       (Mod_ty_sig
         ((Spec_mani_ty (Ty_def_adt t () ((Nil ()))))
           (Spec_value x (Ty_cons t ()))))))
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
    ((Top_mod F
       ((desc
          (Mod_functor
            ((MI (Mod_ty_name I))
              ((desc
                 (Mod_restrict
                   ((desc
                      (Mod_struct
                        ((Top_let x
                           ((desc (Exp_const (Const_int 1)))
                             (start_loc
                               ((pos_fname "") (pos_lnum 8) (pos_bol 51)
                                 (pos_cnum 65)))
                             (end_loc
                               ((pos_fname "") (pos_lnum 8) (pos_bol 51)
                                 (pos_cnum 66)))
                             (attrs ())))
                          (Top_let y
                            ((desc (Exp_const (Const_int 1)))
                              (start_loc
                                ((pos_fname "") (pos_lnum 10) (pos_bol 68)
                                  (pos_cnum 82)))
                              (end_loc
                                ((pos_fname "") (pos_lnum 10) (pos_bol 68)
                                  (pos_cnum 83)))
                              (attrs ())))
                          (Top_let z
                            ((desc (Exp_const (Const_int 1)))
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
                   (Mod_ty_name J)))
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
    ((Top_let co
       ((desc
          (Exp_app
            ((desc (Exp_constr Cons))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 14)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
              (attrs ()))
            ((desc (Exp_const (Const_int 1)))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 19)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 20)))
              (attrs ()))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 14)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 20)))
         (attrs ())))
      (Top_let f
        ((desc (Exp_const (Const_int 1)))
          (start_loc ((pos_fname "") (pos_lnum 4) (pos_bol 22) (pos_cnum 34)))
          (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 22) (pos_cnum 35)))
          (attrs ()))))
    |}];

  print_parsed_program {|
       type t =  a list -> b
|};
  [%expect
    {|
    ((Top_type_def
       (Ty_def_alias t (Ty_arrow (Ty_cons list ((Ty_cons a ()))) (Ty_cons b ())))))
    |}];

  print_parsed_program {|
  external x : int -> int -> int = "ff_add"
|};
  [%expect
    {|
    ((Top_external x
       (Ty_arrow (Ty_cons int ()) (Ty_arrow (Ty_cons int ()) (Ty_cons int ())))
       ff_add))
    |}];

  print_parsed_program
    {|
let x = match a with
      | Cons -> 0

let y = 2
|};
  [%expect
    {|
    ((Top_let x
       ((desc
          (Exp_case
            ((desc (Exp_var a))
              (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
              (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
              (attrs ()))
            (((Pat_constr Cons ())
               ((desc (Exp_const (Const_int 0)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 22) (pos_cnum 38)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 22) (pos_cnum 39)))
                 (attrs ()))))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 9)))
         (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 22) (pos_cnum 39)))
         (attrs ())))
      (Top_let y
        ((desc (Exp_const (Const_int 2)))
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
    ((Top_let x
       ((desc
          (Exp_lam
            ((Para_bare x)
              ((desc (Exp_var y))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 19)))
                (attrs ())))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 9)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 19)))
         (attrs ())))
      (Top_let y
        ((desc (Exp_const (Const_int 2)))
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
    ((Top_let x
       ((desc
          (Exp_tuple
            (((desc (Exp_const (Const_int 1)))
               (start_loc
                 ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
               (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 16)))
               (attrs ()))
              ((desc (Exp_const (Const_int 2)))
                (start_loc
                  ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 17)))
                (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
                (attrs ())))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 15)))
         (end_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 18)))
         (attrs ())))
      (Top_let y
        ((desc
           (Exp_tuple
             (((desc (Exp_const (Const_int 1)))
                (start_loc
                  ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 35)))
                (end_loc
                  ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 36)))
                (attrs ()))
               ((desc (Exp_const (Const_int 2)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 38)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 39)))
                 (attrs ())))))
          (start_loc ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 35)))
          (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 21) (pos_cnum 39)))
          (attrs ())))
      (Top_let z
        ((desc
           (Exp_case
             ((desc (Exp_var y))
               (start_loc
                 ((pos_fname "") (pos_lnum 6) (pos_bol 42) (pos_cnum 61)))
               (end_loc ((pos_fname "") (pos_lnum 6) (pos_bol 42) (pos_cnum 62)))
               (attrs ()))
             (((Pat_tuple ((Pat_var x) (Pat_var y)))
                ((desc (Exp_var x))
                  (start_loc
                    ((pos_fname "") (pos_lnum 7) (pos_bol 69) (pos_cnum 94)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 7) (pos_bol 69) (pos_cnum 95)))
                  (attrs ()))))))
          (start_loc ((pos_fname "") (pos_lnum 6) (pos_bol 42) (pos_cnum 55)))
          (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 69) (pos_cnum 95)))
          (attrs ())))
      (Top_let n
        ((desc
           (Exp_lam
             ((Para_bare y)
               ((desc
                  (Exp_case
                    ((desc (Exp_var y))
                      (start_loc
                        ((pos_fname "") (pos_lnum 9) (pos_bol 97) (pos_cnum 125)))
                      (end_loc
                        ((pos_fname "") (pos_lnum 9) (pos_bol 97) (pos_cnum 126)))
                      (attrs ()))
                    (((Pat_tuple ((Pat_var x) (Pat_var y)))
                       ((desc (Exp_var y))
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
      (Top_let w
        ((desc (Exp_const (Const_int 1)))
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
    ((Top_letrec
       ((sum
          ((Para_bare x)
            ((desc
               (Exp_if
                 ((desc
                    (Exp_cmp Eq
                      ((desc (Exp_var x))
                        (start_loc
                          ((pos_fname "") (pos_lnum 3) (pos_bol 24)
                            (pos_cnum 32)))
                        (end_loc
                          ((pos_fname "") (pos_lnum 3) (pos_bol 24)
                            (pos_cnum 33)))
                        (attrs ()))
                      ((desc (Exp_const (Const_int 0)))
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
                 ((desc (Exp_const (Const_int 0)))
                   (start_loc
                     ((pos_fname "") (pos_lnum 4) (pos_bol 38) (pos_cnum 47)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 4) (pos_bol 38) (pos_cnum 48)))
                   (attrs ()))
                 ((desc (Exp_const (Const_int 1)))
                   (start_loc
                     ((pos_fname "") (pos_lnum 5) (pos_bol 49) (pos_cnum 58)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 5) (pos_bol 49) (pos_cnum 59)))
                   (attrs ()))))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 29)))
              (end_loc ((pos_fname "") (pos_lnum 5) (pos_bol 49) (pos_cnum 59)))
              (attrs ()))))))
      (Top_let result
        ((desc
           (Exp_app
             ((desc (Exp_var print_int))
               (start_loc
                 ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 75)))
               (end_loc ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 84)))
               (attrs ()))
             ((desc
                (Exp_app
                  ((desc (Exp_var sum))
                    (start_loc
                      ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 86)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 7) (pos_bol 62) (pos_cnum 89)))
                    (attrs ()))
                  ((desc (Exp_const (Const_int 4)))
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
  print_parsed_program
    {|
                type 'a t = Nil
                        |};
  [%expect {| ((Top_type_def (Ty_def_adt t ('a/0) ((Nil ()))))) |}];
  print_parsed_program
    {|
                type t = | Nil
                        |};
  [%expect {| ((Top_type_def (Ty_def_adt t () ((Nil ()))))) |}];
  print_parsed_program
    {|
     let x = let rec f = fun x -> 1
             and g = fun y -> 2 in
             f
     let _ = 1

     |};
  [%expect
    {|
    ((Top_let x
       ((desc
          (Exp_letrec
            ((f
               ((Para_bare x)
                 ((desc (Exp_const (Const_int 1)))
                   (start_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 35)))
                   (end_loc
                     ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 36)))
                   (attrs ()))))
              (g
                ((Para_bare y)
                  ((desc (Exp_const (Const_int 2)))
                    (start_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 37) (pos_cnum 67)))
                    (end_loc
                      ((pos_fname "") (pos_lnum 3) (pos_bol 37) (pos_cnum 68)))
                    (attrs ())))))
            ((desc (Exp_var f))
              (start_loc
                ((pos_fname "") (pos_lnum 4) (pos_bol 72) (pos_cnum 85)))
              (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 72) (pos_cnum 86)))
              (attrs ()))))
         (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 14)))
         (end_loc ((pos_fname "") (pos_lnum 4) (pos_bol 72) (pos_cnum 86)))
         (attrs ())))
      (Top_let _
        ((desc (Exp_const (Const_int 1)))
          (start_loc ((pos_fname "") (pos_lnum 5) (pos_bol 87) (pos_cnum 100)))
          (end_loc ((pos_fname "") (pos_lnum 5) (pos_bol 87) (pos_cnum 101)))
          (attrs ()))))
    |}]

let%expect_test "Test: path parsing" =
  print_parsed_mod_expr {|X|};
  [%expect
    {|
    ((desc (Mod_name X))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
      (attrs ()))
    |}];
  print_parsed_mod_expr {|X.Y|};
  [%expect
    {|
    ((desc
       (Mod_field
         ((desc (Mod_name X))
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
    ((desc
       (Mod_apply
         ((desc (Mod_name X))
           (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
           (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
           (attrs ()))
         ((desc (Mod_name Y))
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
    ((desc
       (Mod_apply
         ((desc
            (Mod_apply
              ((desc
                 (Mod_field
                   ((desc (Mod_name X))
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
              ((desc
                 (Mod_apply
                   ((desc (Mod_name Z))
                     (start_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                     (end_loc
                       ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                     (attrs ()))
                   ((desc (Mod_name N))
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
         ((desc
            (Mod_field
              ((desc
                 (Mod_field
                   ((desc (Mod_name W))
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
  [%expect {| (Ty_cons string ()) |}];
  print_parsed_type_expr "(string) list";
  [%expect {| (Ty_cons list ((Ty_cons string ()))) |}];
  print_parsed_type_expr "string list";
  [%expect {| (Ty_cons list ((Ty_cons string ()))) |}];
  print_parsed_type_expr "'x";
  [%expect {| (Ty_var 'x/0) |}];
  print_parsed_type_expr "(string, 'x, 'y) list";
  [%expect
    {| (Ty_cons list ((Ty_cons string ()) (Ty_var 'x/0) (Ty_var 'y/0))) |}];
  print_parsed_type_expr "int * int";
  [%expect {| (Ty_tuple ((Ty_cons int ()) (Ty_cons int ()))) |}];
  print_parsed_type_expr "int * i list * (x * y) list * (t1 * t2)";
  [%expect
    {|
    (Ty_tuple
      ((Ty_cons int ()) (Ty_cons list ((Ty_cons i ())))
        (Ty_cons list ((Ty_tuple ((Ty_cons x ()) (Ty_cons y ())))))
        (Ty_tuple ((Ty_cons t1 ()) (Ty_cons t2 ())))))
    |}];
  print_parsed_type_expr "{x: int; y: float; z: int -> float }";
  [%expect
    {|
    (Ty_record
      ((x (Ty_cons int ())) (y (Ty_cons float ()))
        (z (Ty_arrow (Ty_cons int ()) (Ty_cons float ())))))
    |}];
  print_parsed_type_expr "(int, float) T.t";
  [%expect
    {|
    (Ty_field
      ((desc (Mod_name T))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
        (attrs ()))
      t ((Ty_cons int ()) (Ty_cons float ())))
    |}];
  print_parsed_type_expr "int T(M).t";
  [%expect
    {|
    (Ty_field
      ((desc
         (Mod_apply
           ((desc (Mod_name T))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
             (attrs ()))
           ((desc (Mod_name M))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
             (attrs ()))))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
        (attrs ()))
      t ((Ty_cons int ())))
    |}];
  print_parsed_type_expr "(int) T.t";
  [%expect
    {|
    (Ty_field
      ((desc (Mod_name T))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 7)))
        (attrs ()))
      t ((Ty_cons int ())))
    |}]

let%expect_test "Test: top level module" =
  print_parsed_program {|
     module X = struct
     end
     |};
  [%expect
    {|
    ((Top_mod X
       ((desc (Mod_struct ()))
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
    ((Top_mod X
       ((desc
          (Mod_struct
            ((Top_let x
               ((desc (Exp_const (Const_int 1)))
                 (start_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 39)))
                 (end_loc
                   ((pos_fname "") (pos_lnum 3) (pos_bol 24) (pos_cnum 40)))
                 (attrs ())))
              (Top_letrec
                ((y
                   ((Para_bare x)
                     ((desc (Exp_const (Const_int 3)))
                       (start_loc
                         ((pos_fname "") (pos_lnum 4) (pos_bol 41) (pos_cnum 69)))
                       (end_loc
                         ((pos_fname "") (pos_lnum 4) (pos_bol 41) (pos_cnum 70)))
                       (attrs ()))))))
              (Top_mod Y
                ((desc (Mod_struct ()))
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
    ((desc
       (Mod_struct
         ((Top_let x
            ((desc (Exp_const (Const_int 1)))
              (start_loc
                ((pos_fname "") (pos_lnum 3) (pos_bol 14) (pos_cnum 29)))
              (end_loc ((pos_fname "") (pos_lnum 3) (pos_bol 14) (pos_cnum 30)))
              (attrs ())))
           (Top_type_def (Ty_def_adt a () ((Cons ((Ty_cons int ()))) (Nil ())))))))
      (start_loc ((pos_fname "") (pos_lnum 2) (pos_bol 1) (pos_cnum 6)))
      (end_loc ((pos_fname "") (pos_lnum 8) (pos_bol 91) (pos_cnum 99)))
      (attrs ()))
    |}];
  print_parsed {|functor (X: M) -> struct end|};
  [%expect
    {|
    ((desc
       (Mod_functor
         ((X (Mod_ty_name M))
           ((desc (Mod_struct ()))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
             (attrs ())))))
      (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
      (attrs ()))
    |}]

let%expect_test "Test: module type" =
  let print_parsed str =
    parse_string_mod_type str |> sexp_of_surface_mod_ty |> print_sexp
  in
  print_parsed {|M|};
  [%expect {| (Mod_ty_name M) |}];
  print_parsed {|M.X(M).E|};
  [%expect
    {|
    (Mod_ty_field
      ((desc
         (Mod_apply
           ((desc
              (Mod_field
                ((desc (Mod_name M))
                  (start_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
                  (end_loc
                    ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 1)))
                  (attrs ()))
                X))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 3)))
             (attrs ()))
           ((desc (Mod_name M))
             (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
             (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
             (attrs ()))))
        (start_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
        (end_loc ((pos_fname "") (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
        (attrs ()))
      E)
    |}];
  print_parsed {|sig val x : int end|};
  [%expect {| (Mod_ty_sig ((Spec_value x (Ty_cons int ())))) |}];
  print_parsed {|functor (M:M) -> sig val x: int end|};
  [%expect
    {|
    (Mod_ty_functor M (Mod_ty_name M)
      (Mod_ty_sig ((Spec_value x (Ty_cons int ())))))
    |}];
  print_parsed {|functor (M:M) -> M1|};
  [%expect {| (Mod_ty_functor M (Mod_ty_name M) (Mod_ty_name M1)) |}];
  print_parsed {|functor (M:functor (X:M)->M) -> M1|};
  [%expect
    {|
    (Mod_ty_functor M (Mod_ty_functor X (Mod_ty_name M) (Mod_ty_name M))
      (Mod_ty_name M1))
    |}];
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
    (Mod_ty_sig
      ((Spec_value x (Ty_cons int ())) (Spec_abstr t ())
        (Spec_value m (Ty_arrow (Ty_cons t ()) (Ty_cons t ())))
        (Spec_mani_ty
          (Ty_def_adt i_list () ((Cons ((Ty_cons int ()))) (Nil ()))))))
    |}]
