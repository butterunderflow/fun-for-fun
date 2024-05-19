open Syntax.Parsing
open Typing
module T = Typedtree

[@@@warning "-26"]

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: expression typing" =
  let print_typed str =
    Ident.refresh ();
    let e = parse_string_expr str in
    let typed, _ = Typing.Check.tc_expr e Typing.Env.init in
    typed |> T.sexp_of_expr |> print_sexp
  in
  let print_type str =
    Ident.refresh ();
    let e = parse_string_expr str in
    let typed, _ = Typing.Check.tc_expr e Typing.Env.init in
    typed |> T.get_ty |> Syntax.Types_in.sexp_of_ty |> print_sexp
  in
  print_typed "1";
  [%expect {| (EConst (CInt 1) (TConsI (0 int) ())) |}];

  print_type "1";
  [%expect {| (TConsI (0 int) ()) |}];

  print_typed "let x = 1 in x";
  [%expect
    {|
    (ELet x (EConst (CInt 1) (TConsI (0 int) ())) (EVar x (TConsI (0 int) ()))
      (TConsI (0 int) ())) |}];

  print_type "let x = 1 in x";
  [%expect {| (TConsI (0 int) ()) |}];

  print_typed "let f = (fun x -> x) in f";
  [%expect
    {|
    (ELet f
      (ELam (x (EVar x (TVarI '_t/1)) (TArrowI (TVarI '_t/1) (TVarI '_t/1))))
      (EVar f (TArrowI (TVarI '_t/2) (TVarI '_t/2)))
      (TArrowI (TVarI '_t/2) (TVarI '_t/2))) |}];

  print_typed "let f = (fun x -> x) in f 1";
  [%expect
    {|
    (ELet f
      (ELam (x (EVar x (TVarI '_t/1)) (TArrowI (TVarI '_t/1) (TVarI '_t/1))))
      (EApp (EVar f (TArrowI (TConsI (0 int) ()) (TConsI (0 int) ())))
        (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ()))
      (TConsI (0 int) ()))
     |}];

  print_type "let f = (fun x -> x) in f 1";
  [%expect {|
    (TConsI (0 int) ()) |}];

  print_typed "1, true";
  [%expect
    {|
    (ETuple
      ((EConst (CInt 1) (TConsI (0 int) ()))
        (EConst (CBool true) (TConsI (0 bool) ())))
      (TTupleI ((TConsI (0 int) ()) (TConsI (0 bool) ())))) |}];

  print_typed "let f = (fun x -> x) in (f 1, f true)";
  [%expect
    {|
    (ELet f
      (ELam (x (EVar x (TVarI '_t/1)) (TArrowI (TVarI '_t/1) (TVarI '_t/1))))
      (ETuple
        ((EApp (EVar f (TArrowI (TConsI (0 int) ()) (TConsI (0 int) ())))
           (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ()))
          (EApp (EVar f (TArrowI (TConsI (0 bool) ()) (TConsI (0 bool) ())))
            (EConst (CBool true) (TConsI (0 bool) ())) (TConsI (0 bool) ())))
        (TTupleI ((TConsI (0 int) ()) (TConsI (0 bool) ()))))
      (TTupleI ((TConsI (0 int) ()) (TConsI (0 bool) ())))) |}];
  print_typed
    {|
     let rec f = fun x -> g x
     and 
     g = fun x -> f x
     in
     1
     |};
  [%expect
    {|
    (ELetrec
      ((f
         (x
           (EApp (EVar g (TArrowI (TVarI '_t/5) (TVarI ret/6)))
             (EVar x (TVarI '_t/5)) (TVarI ret/6))
           (TArrowI (TVarI '_t/5) (TVarI ret/6))))
        (g
          (x
            (EApp (EVar f (TArrowI (TVarI '_t/5) (TVarI ret/6)))
              (EVar x (TVarI '_t/5)) (TVarI ret/6))
            (TArrowI (TVarI '_t/5) (TVarI ret/6)))))
      (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ())) |}];

  print_typed
    {|
     let rec f = fun x -> x
     and 
     g = fun x -> f 1
     in
     1
     |};
  [%expect
    {|
    (ELetrec
      ((f
         (x (EVar x (TConsI (0 int) ()))
           (TArrowI (TConsI (0 int) ()) (TConsI (0 int) ()))))
        (g
          (x
            (EApp (EVar f (TArrowI (TConsI (0 int) ()) (TConsI (0 int) ())))
              (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ()))
            (TArrowI (TVarI '_t/4) (TConsI (0 int) ())))))
      (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ())) |}]
(* todo: test pattern matching *)

let%expect_test "Test: program toplevel typing" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    try
      let typed, _u, _env = Typing.Check.tc_program prog Typing.Env.init in
      typed |> T.sexp_of_program |> print_sexp
    with
    | Unify.UnificationError (t0, t1) ->
        Printf.printf "Can't unify %s with %s" t0 t1
  in
  let print_effect str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let _typed, _u, env = Typing.Check.tc_program prog Typing.Env.init in
    Printf.printf "%s\n" (Env.dbg env)
  in
  print_typed {|
     let x = 1
     |};
  [%expect
    {| ((TopLet x (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ()))) |}];
  print_typed
    {|
     let rec f = fun x -> x
     and 
     g = fun x -> f 1
     |};
  [%expect
    {|
    ((TopLetRec
       ((f
          (x (EVar x (TConsI (0 int) ()))
            (TArrowI (TConsI (0 int) ()) (TConsI (0 int) ()))))
         (g
           (x
             (EApp (EVar f (TArrowI (TConsI (0 int) ()) (TConsI (0 int) ())))
               (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ()))
             (TArrowI (TVarI '_t/4) (TConsI (0 int) ()))))))) |}];
  print_effect
    {|
     type () a 
     = Cons of int
     | Nil 
     end
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------
    Value Bindings:
      Cons |-> forall  . (TArrowI (TConsI (0 int) ()) (TConsI (0 a) ()));
      Nil |-> forall  . (TConsI (0 a) ())
    Type Definitions:
      a |-> (TDAdtI a () ((Cons ((TConsI (0 int) ()))) (Nil ())))
    Current Module Index:
      0
    ------------------Envirment Debug Info End-------------------------- |}];
  print_typed
    {|
     type () int_l 
     = Cons of int
     | Nil 
     end
     let c = Nil
     let co = Cons 1
     |};
  [%expect
    {|
    ((TopTypeDef (TDAdtI int_l () ((Cons ((TConsI (0 int) ()))) (Nil ()))))
      (TopLet c (ECons Nil (TConsI (0 int_l) ())) (TConsI (0 int_l) ()))
      (TopLet co
        (EApp (ECons Cons (TArrowI (TConsI (0 int) ()) (TConsI (0 int_l) ())))
          (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int_l) ()))
        (TConsI (0 int_l) ()))) |}];
  print_effect
    {|
     type () int_l 
     = Cons of int
     | Nil 
     end
     let c = Nil
     let co = Cons 1
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------
    Value Bindings:
      co |-> forall  . (TConsI (0 int_l) ());
      c |-> forall  . (TConsI (0 int_l) ());
      Cons |-> forall  . (TArrowI (TConsI (0 int) ()) (TConsI (0 int_l) ()));
      Nil |-> forall  . (TConsI (0 int_l) ())
    Type Definitions:
      int_l |-> (TDAdtI int_l () ((Cons ((TConsI (0 int) ()))) (Nil ())))
    Current Module Index:
      0
    ------------------Envirment Debug Info End-------------------------- |}];
  print_typed
    {|
     type () int_l
     = Cons of int
     | Nil
     end
     let x = Nil
     let f =
         match x with
        | Cons x -> x
        | Nil    -> 0
|};
  [%expect
    {|
    ((TopTypeDef (TDAdtI int_l () ((Cons ((TConsI (0 int) ()))) (Nil ()))))
      (TopLet x (ECons Nil (TConsI (0 int_l) ())) (TConsI (0 int_l) ()))
      (TopLet f
        (ECase (EVar x (TConsI (0 int_l) ()))
          (((PCons Cons ((PVar x))) (EVar x (TConsI (0 int) ())))
            ((PCons Nil ()) (EConst (CInt 0) (TConsI (0 int) ()))))
          (TConsI (0 int) ()))
        (TConsI (0 int) ()))) |}];
  print_typed
    {|
     type ('a, 'b) int_l
     = Cons of ('a * 'b)
     | Nil
     end
     let x = Nil
     let f =
         match x with
        | Cons (a, b) -> (b, a)
     |};
  [%expect
    {|
    ((TopTypeDef
       (TDAdtI int_l ('a/0 'b/0)
         ((Cons ((TTupleI ((TVarI 'a/0) (TVarI 'b/0))))) (Nil ()))))
      (TopLet x (ECons Nil (TConsI (0 int_l) ((TVarI 'a/5) (TVarI 'b/6))))
        (TConsI (0 int_l) ((TVarI 'a/5) (TVarI 'b/6))))
      (TopLet f
        (ECase (EVar x (TConsI (0 int_l) ((TVarI '_t/12) (TVarI '_t/13))))
          (((PCons Cons ((PTuple ((PVar a) (PVar b)))))
             (ETuple ((EVar b (TVarI '_t/13)) (EVar a (TVarI '_t/12)))
               (TTupleI ((TVarI '_t/13) (TVarI '_t/12))))))
          (TTupleI ((TVarI '_t/13) (TVarI '_t/12))))
        (TTupleI ((TVarI '_t/13) (TVarI '_t/12))))) |}];
  print_effect
    {|
     type ('a, 'b) int_l
     = Cons of ('a * 'b)
     | Nil
     end
     let x = Nil
     let f =
         match x with
        | Cons (a, b) -> (b, a)
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------
    Value Bindings:
      f |-> forall '_t/13;'_t/12 . (TTupleI ((TVarI '_t/13) (TVarI '_t/12)));
      x |-> forall 'b/6;'a/5 . (TConsI (0 int_l) ((TVarI 'a/5) (TVarI 'b/6)));
      Cons |-> forall 'a/1;'b/2 . (TArrowI (TTupleI ((TVarI 'a/1) (TVarI 'b/2)))
      (TConsI (0 int_l) ((TVarI 'a/1) (TVarI 'b/2))));
      Nil |-> forall 'a/3;'b/4 . (TConsI (0 int_l) ((TVarI 'a/3) (TVarI 'b/4)))
    Type Definitions:
      int_l |-> (TDAdtI int_l ('a/0 'b/0)
      ((Cons ((TTupleI ((TVarI 'a/0) (TVarI 'b/0))))) (Nil ())))
    Current Module Index:
      0
    ------------------Envirment Debug Info End-------------------------- |}]

let%expect_test "Test: type check pattern" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    try
      let typed, _u, _env = Typing.Check.tc_program prog Typing.Env.init in
      typed |> T.sexp_of_program |> print_sexp
    with
    | Unify.UnificationError (t0, t1) ->
        Printf.printf "Can't unify %s with %s" t0 t1
  in
  ()
