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
    let typed, _ = Typing.Check.tc_expr e Typing.Env.empty in
    typed |> T.sexp_of_expr |> print_sexp
  in
  let print_type str =
    Ident.refresh ();
    let e = parse_string_expr str in
    let typed, _ = Typing.Check.tc_expr e Typing.Env.empty in
    typed |> T.get_ty |> Syntax.Parsetree.sexp_of_type_expr |> print_sexp
  in
  print_typed "1";
  [%expect {| (EConst (CInt 1) (TCons int ())) |}];

  print_type "1";
  [%expect {| (TCons int ()) |}];

  print_typed "let x = 1 in x";
  [%expect
    {|
    (ELet x (EConst (CInt 1) (TCons int ())) (EVar x (TCons int ()))
      (TCons int ())) |}];

  print_type "let x = 1 in x";
  [%expect {| (TCons int ()) |}];

  print_typed "let f = (fun x -> x) in f";
  [%expect
    {|
    (ELet f (ELam (x (EVar x (TVar '_t/1)) (TArrow (TVar '_t/1) (TVar '_t/1))))
      (EVar f (TArrow (TVar '_t/2) (TVar '_t/2)))
      (TArrow (TVar '_t/2) (TVar '_t/2))) |}];

  print_typed "let f = (fun x -> x) in f 1";
  [%expect
    {|
    (ELet f (ELam (x (EVar x (TVar '_t/1)) (TArrow (TVar '_t/1) (TVar '_t/1))))
      (EApp (EVar f (TArrow (TCons int ()) (TCons int ())))
        (EConst (CInt 1) (TCons int ())) (TCons int ()))
      (TCons int ()))
     |}];

  print_type "let f = (fun x -> x) in f 1";
  [%expect {|
    (TCons int ()) |}];

  print_typed "1, true";
  [%expect
    {|
    (ETuple
      ((EConst (CInt 1) (TCons int ())) (EConst (CBool true) (TCons bool ())))
      (TTuple ((TCons int ()) (TCons bool ())))) |}];

  print_typed "let f = (fun x -> x) in (f 1, f true)";
  [%expect
    {|
    (ELet f (ELam (x (EVar x (TVar '_t/1)) (TArrow (TVar '_t/1) (TVar '_t/1))))
      (ETuple
        ((EApp (EVar f (TArrow (TCons int ()) (TCons int ())))
           (EConst (CInt 1) (TCons int ())) (TCons int ()))
          (EApp (EVar f (TArrow (TCons bool ()) (TCons bool ())))
            (EConst (CBool true) (TCons bool ())) (TCons bool ())))
        (TTuple ((TCons int ()) (TCons bool ()))))
      (TTuple ((TCons int ()) (TCons bool ())))) |}];
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
           (EApp (EVar g (TArrow (TVar '_t/5) (TVar ret/6)))
             (EVar x (TVar '_t/5)) (TVar ret/6))
           (TArrow (TVar '_t/5) (TVar ret/6))))
        (g
          (x
            (EApp (EVar f (TArrow (TVar '_t/5) (TVar ret/6)))
              (EVar x (TVar '_t/5)) (TVar ret/6))
            (TArrow (TVar '_t/5) (TVar ret/6)))))
      (EConst (CInt 1) (TCons int ())) (TCons int ())) |}];

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
      ((f (x (EVar x (TCons int ())) (TArrow (TCons int ()) (TCons int ()))))
        (g
          (x
            (EApp (EVar f (TArrow (TCons int ()) (TCons int ())))
              (EConst (CInt 1) (TCons int ())) (TCons int ()))
            (TArrow (TVar '_t/4) (TCons int ())))))
      (EConst (CInt 1) (TCons int ())) (TCons int ())) |}]
(* todo: test pattern matching *)

let%expect_test "Test: program toplevel typing" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    try
      let typed, _u, _env = Typing.Check.tc_program prog Typing.Env.empty in
      typed |> T.sexp_of_program |> print_sexp
    with
    | Unify.UnificationError (t0, t1) ->
        Printf.printf "Can't unify %s with %s" t0 t1
  in
  let print_effect str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let _typed, _u, env = Typing.Check.tc_program prog Typing.Env.empty in
    Printf.printf "%s\n" (Env.dbg env)
  in
  print_typed {|
     let x = 1
     |};
  [%expect
    {| ((TopLet x (EConst (CInt 1) (TCons int ())) (TCons int ()))) |}];
  print_typed
    {|
     let rec f = fun x -> x
     and 
     g = fun x -> f 1
     |};
  [%expect
    {|
    ((TopLetRec
       ((f (x (EVar x (TCons int ())) (TArrow (TCons int ()) (TCons int ()))))
         (g
           (x
             (EApp (EVar f (TArrow (TCons int ()) (TCons int ())))
               (EConst (CInt 1) (TCons int ())) (TCons int ()))
             (TArrow (TVar '_t/4) (TCons int ()))))))) |}];
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
      Cons |-> forall  . (TArrow (TCons int ()) (TCons a ()));
      Nil |-> forall  . (TCons a ())
    Type Definitions:
      a |-> (TDAdt a () ((Cons ((TCons int ()))) (Nil ())))
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
    ((TopTypeDef (TDAdt int_l () ((Cons ((TCons int ()))) (Nil ()))))
      (TopLet c (ECons Nil (TCons int_l ())) (TCons int_l ()))
      (TopLet co
        (EApp (ECons Cons (TArrow (TCons int ()) (TCons int_l ())))
          (EConst (CInt 1) (TCons int ())) (TCons int_l ()))
        (TCons int_l ()))) |}];
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
      co |-> forall  . (TCons int_l ());
      c |-> forall  . (TCons int_l ());
      Cons |-> forall  . (TArrow (TCons int ()) (TCons int_l ()));
      Nil |-> forall  . (TCons int_l ())
    Type Definitions:
      int_l |-> (TDAdt int_l () ((Cons ((TCons int ()))) (Nil ())))
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
    ((TopTypeDef (TDAdt int_l () ((Cons ((TCons int ()))) (Nil ()))))
      (TopLet x (ECons Nil (TCons int_l ())) (TCons int_l ()))
      (TopLet f
        (ECase (EVar x (TCons int_l ()))
          (((PCons Cons ((PVar x))) (EVar x (TCons int ())))
            ((PCons Nil ()) (EConst (CInt 0) (TCons int ()))))
          (TCons int ()))
        (TCons int ()))) |}];
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
  [%expect {|
    ((TopTypeDef
       (TDAdt int_l ('a/0 'b/0)
         ((Cons ((TTuple ((TVar 'a/0) (TVar 'b/0))))) (Nil ()))))
      (TopLet x (ECons Nil (TCons int_l ((TVar 'a/5) (TVar 'b/6))))
        (TCons int_l ((TVar 'a/5) (TVar 'b/6))))
      (TopLet f
        (ECase (EVar x (TCons int_l ((TVar '_t/12) (TVar '_t/13))))
          (((PCons Cons ((PTuple ((PVar a) (PVar b)))))
             (ETuple ((EVar b (TVar '_t/13)) (EVar a (TVar '_t/12)))
               (TTuple ((TVar '_t/13) (TVar '_t/12))))))
          (TTuple ((TVar '_t/13) (TVar '_t/12))))
        (TTuple ((TVar '_t/13) (TVar '_t/12))))) |}];
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
  [%expect {|
    ------------------Envirment Debug Info Begin------------------------
    Value Bindings:
      f |-> forall '_t/13;'_t/12 . (TTuple ((TVar '_t/13) (TVar '_t/12)));
      x |-> forall 'b/6;'a/5 . (TCons int_l ((TVar 'a/5) (TVar 'b/6)));
      Cons |-> forall 'a/1;'b/2 . (TArrow (TTuple ((TVar 'a/1) (TVar 'b/2)))
      (TCons int_l ((TVar 'a/1) (TVar 'b/2))));
      Nil |-> forall 'a/3;'b/4 . (TCons int_l ((TVar 'a/3) (TVar 'b/4)))
    Type Definitions:
      int_l |-> (TDAdt int_l ('a/0 'b/0)
      ((Cons ((TTuple ((TVar 'a/0) (TVar 'b/0))))) (Nil ())))
    ------------------Envirment Debug Info End-------------------------- |}]



let%expect_test "Test: type check pattern" = 
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    try
      let typed, _u, _env = Typing.Check.tc_program prog Typing.Env.empty in
      typed |> T.sexp_of_program |> print_sexp
    with
    | Unify.UnificationError (t0, t1) ->
        Printf.printf "Can't unify %s with %s" t0 t1
  in
  ()
  

  
