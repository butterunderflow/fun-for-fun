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
    let typed = Typing.Check.tc_expr e Typing.Env.init in
    typed |> T.sexp_of_expr |> print_sexp
  in
  let print_type str =
    Ident.refresh ();
    let e = parse_string_expr str in
    let typed = Typing.Check.tc_expr e Typing.Env.init in
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
      (ELam
        (x (EVar x (TVarI (Unbound '_t/1)))
          (TArrowI (TVarI (Unbound '_t/1)) (TVarI (Unbound '_t/1)))))
      (EVar f (TArrowI (TVarI (Unbound '_t/2)) (TVarI (Unbound '_t/2))))
      (TArrowI (TVarI (Unbound '_t/2)) (TVarI (Unbound '_t/2)))) |}];

  print_typed "let f = (fun x -> x) in f 1";
  [%expect
    {|
    (ELet f
      (ELam
        (x (EVar x (TVarI (Unbound '_t/1)))
          (TArrowI (TVarI (Unbound '_t/1)) (TVarI (Unbound '_t/1)))))
      (EApp
        (EVar f
          (TArrowI (TVarI (Link (TConsI (0 int) ())))
            (TVarI (Link (TConsI (0 int) ())))))
        (EConst (CInt 1) (TConsI (0 int) ())) (TVarI (Link (TConsI (0 int) ()))))
      (TVarI (Link (TConsI (0 int) ()))))
     |}];

  print_type "let f = (fun x -> x) in f 1";
  [%expect {|
    (TVarI (Link (TConsI (0 int) ()))) |}];

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
      (ELam
        (x (EVar x (TVarI (Unbound '_t/1)))
          (TArrowI (TVarI (Unbound '_t/1)) (TVarI (Unbound '_t/1)))))
      (ETuple
        ((EApp
           (EVar f
             (TArrowI (TVarI (Link (TConsI (0 int) ())))
               (TVarI (Link (TConsI (0 int) ())))))
           (EConst (CInt 1) (TConsI (0 int) ()))
           (TVarI (Link (TConsI (0 int) ()))))
          (EApp
            (EVar f
              (TArrowI (TVarI (Link (TConsI (0 bool) ())))
                (TVarI (Link (TConsI (0 bool) ())))))
            (EConst (CBool true) (TConsI (0 bool) ()))
            (TVarI (Link (TConsI (0 bool) ())))))
        (TTupleI
          ((TVarI (Link (TConsI (0 int) ())))
            (TVarI (Link (TConsI (0 bool) ()))))))
      (TTupleI
        ((TVarI (Link (TConsI (0 int) ()))) (TVarI (Link (TConsI (0 bool) ())))))) |}];
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
           (EApp
             (EVar g
               (TVarI
                 (Link (TArrowI (TVarI (Unbound '_t/3)) (TVarI (Unbound ret/4))))))
             (EVar x (TVarI (Unbound '_t/3))) (TVarI (Unbound ret/4)))
           (TArrowI (TVarI (Unbound '_t/3)) (TVarI (Unbound ret/4)))))
        (g
          (x
            (EApp
              (EVar f
                (TVarI
                  (Link
                    (TArrowI (TVarI (Unbound '_t/5)) (TVarI (Unbound ret/6))))))
              (EVar x (TVarI (Unbound '_t/5))) (TVarI (Unbound ret/6)))
            (TArrowI (TVarI (Unbound '_t/5)) (TVarI (Unbound ret/6))))))
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
         (x (EVar x (TVarI (Unbound '_t/3)))
           (TArrowI (TVarI (Unbound '_t/3)) (TVarI (Unbound '_t/3)))))
        (g
          (x
            (EApp
              (EVar f
                (TVarI
                  (Link (TArrowI (TConsI (0 int) ()) (TVarI (Unbound ret/5))))))
              (EConst (CInt 1) (TConsI (0 int) ())) (TVarI (Unbound ret/5)))
            (TArrowI (TVarI (Unbound '_t/4)) (TVarI (Unbound ret/5))))))
      (EConst (CInt 1) (TConsI (0 int) ())) (TConsI (0 int) ())) |}]
(* todo: test pattern matching *)

let%expect_test "Test: program toplevel typing" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    try
      let typed, _env = Typing.Check.tc_program prog Typing.Env.init in
      typed |> T.sexp_of_program |> print_sexp
    with
    | Unify.UnificationError (t0, t1) ->
        Printf.printf "Can't unify %s with %s" t0 t1
  in
  let print_effect str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let _typed, env = Typing.Check.tc_program prog Typing.Env.init in
    Printf.printf "%s\n" (Env.dbg env)
  in
  print_typed {|
     let x = 1
     |};
  [%expect
    {| ((TopLet x (EConst (CInt 1) (TConsI (0 int) ())))) |}];
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
          (x (EVar x (TVarI (Unbound '_t/3)))
            (TArrowI (TVarI (Unbound '_t/3)) (TVarI (Unbound '_t/3)))))
         (g
           (x
             (EApp
               (EVar f
                 (TVarI
                   (Link (TArrowI (TConsI (0 int) ()) (TVarI (Unbound ret/5))))))
               (EConst (CInt 1) (TConsI (0 int) ())) (TVarI (Unbound ret/5)))
             (TArrowI (TVarI (Unbound '_t/4)) (TVarI (Unbound ret/5)))))))) |}];
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
      (TopLet c (ECons Nil (TConsI (0 int_l) ())))
      (TopLet co
        (EApp (ECons Cons (TArrowI (TConsI (0 int) ()) (TConsI (0 int_l) ())))
          (EConst (CInt 1) (TConsI (0 int) ()))
          (TVarI (Link (TConsI (0 int_l) ())))))) |}];
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
      (TopLet x (ECons Nil (TConsI (0 int_l) ())))
      (TopLet f
        (ECase (EVar x (TConsI (0 int_l) ()))
          (((PCons Cons ((PVar x))) (EVar x (TConsI (0 int) ())))
            ((PCons Nil ()) (EConst (CInt 0) (TConsI (0 int) ()))))
          (TVarI (Link (TConsI (0 int) ())))))) |}];

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
         ((Cons ((TTupleI ((TQVarI 'a/0) (TQVarI 'b/0))))) (Nil ()))))
      (TopLet x
        (ECons Nil
          (TConsI (0 int_l) ((TVarI (Unbound 'a/1)) (TVarI (Unbound 'b/2))))))
      (TopLet f
        (ECase
          (EVar x
            (TConsI (0 int_l)
              ((TVarI (Link (TVarI (Unbound '_t/6))))
                (TVarI (Link (TVarI (Unbound '_t/7)))))))
          (((PCons Cons ((PTuple ((PVar a) (PVar b)))))
             (ETuple
               ((EVar b (TVarI (Link (TVarI (Link (TVarI (Unbound '_t/7)))))))
                 (EVar a (TVarI (Link (TVarI (Link (TVarI (Unbound '_t/6))))))))
               (TTupleI
                 ((TVarI (Link (TVarI (Link (TVarI (Unbound '_t/7))))))
                   (TVarI (Link (TVarI (Link (TVarI (Unbound '_t/6)))))))))))
          (TVarI
            (Link
              (TTupleI
                ((TVarI (Link (TVarI (Link (TVarI (Unbound '_t/7))))))
                  (TVarI (Link (TVarI (Link (TVarI (Unbound '_t/6))))))))))))) |}];
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
      f |-> forall  . (TTupleI ((TVarI (Unbound '_t/7)) (TVarI (Unbound '_t/6))));
      x |-> forall  . (TConsI (0 int_l) ((TVarI (Unbound 'a/1)) (TVarI (Unbound 'b/2))));
      Cons |-> forall 'a/0;'b/0 . (TArrowI (TTupleI ((TQVarI 'a/0) (TQVarI 'b/0)))
      (TConsI (0 int_l) ((TQVarI 'a/0) (TQVarI 'b/0))));
      Nil |-> forall 'a/0;'b/0 . (TConsI (0 int_l) ((TQVarI 'a/0) (TQVarI 'b/0)))
    Type Definitions:
      int_l |-> (TDAdtI int_l ('a/0 'b/0)
      ((Cons ((TTupleI ((TQVarI 'a/0) (TQVarI 'b/0))))) (Nil ())))
    Current Module Index:
      0
    ------------------Envirment Debug Info End-------------------------- |}]

let%expect_test "Test: full program typing" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    try
      let typed, _env = Typing.Check.tc_program prog Typing.Env.init in
      typed |> T.sexp_of_program |> print_sexp
    with
    | Unify.UnificationError (t0, t1) ->
        Printf.printf "Can't unify %s with %s" t0 t1
  in
  print_typed {| let x = 1 |};
  [%expect
    {| ((TopLet x (EConst (CInt 1) (TConsI (0 int) ())))) |}];
  print_typed {| module M = struct let x = 1 end |};
  [%expect
    {|
    ((TopMod M
       (MEStruct ((TopLet x (EConst (CInt 1) (TConsI (0 int) ()))))
         (MTMod (id 1) (val_defs ((x (() (TConsI (0 int) ()))))) (ty_defs ())
           (mod_defs ()))))) |}];
  print_typed
    {|
     module M = struct let x = 1 end
     let c = M.x
     |};
  [%expect {|
    ((TopMod M
       (MEStruct ((TopLet x (EConst (CInt 1) (TConsI (0 int) ()))))
         (MTMod (id 1) (val_defs ((x (() (TConsI (0 int) ()))))) (ty_defs ())
           (mod_defs ()))))
      (TopLet c
        (EField
          (MEName M
            (MTMod (id 1) (val_defs ((x (() (TConsI (0 int) ()))))) (ty_defs ())
              (mod_defs ())))
          x (TConsI (0 int) ())))) |}];
  print_typed
    {|
     module M =
       struct
         type () t = Nil
         end

         let x = Nil
       end
     let c = M.x
     |};
  [%expect {|
    ((TopMod M
       (MEStruct
         ((TopTypeDef (TDAdtI t () ((Nil ()))))
           (TopLet x (ECons Nil (TConsI (1 t) ()))))
         (MTMod (id 1)
           (val_defs ((x (() (TConsI (1 t) ()))) (Nil (() (TConsI (1 t) ())))))
           (ty_defs ((TDAdtI t () ((Nil ()))))) (mod_defs ()))))
      (TopLet c
        (EField
          (MEName M
            (MTMod (id 1)
              (val_defs
                ((x (() (TConsI (1 t) ()))) (Nil (() (TConsI (1 t) ())))))
              (ty_defs ((TDAdtI t () ((Nil ()))))) (mod_defs ())))
          x (TConsI (1 t) ())))) |}]
