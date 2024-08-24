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
    try
      let typed = Typing.Check.check_expr e (Typing.Env.init ()) in
      typed |> T.sexp_of_expr |> print_sexp
    with
    | Unify.OccurError (tvn, te) ->
        print_sexp (Types_in.sexp_of_tv !tvn);
        Printf.printf "occured in ";
        print_sexp (Types_in.sexp_of_ty te)
  in

  let print_type str =
    Ident.refresh ();
    let e = parse_string_expr str in
    let typed = Typing.Check.check_expr e (Typing.Env.init ()) in
    typed |> T.get_ty |> Typing.Types_in.sexp_of_ty |> print_sexp
  in
  print_typed "1";
  [%expect {| (EConst (CInt 1) (TCons (0 int) ())) |}];

  print_type "1";
  [%expect {| (TCons (0 int) ()) |}];

  print_typed "let x = 1 in x";
  [%expect
    {|
    (ELet x (EConst (CInt 1) (TCons (0 int) ())) (EVar x (TCons (0 int) ()))
      (TCons (0 int) ())) |}];

  print_type "let x = 1 in x";
  [%expect {| (TCons (0 int) ()) |}];

  print_typed "let f = (fun x -> x) in f";
  [%expect
    {|
    (ELet f
      (ELam
        (x (EVar x (TVar (Unbound '_t/1 1)))
          (TArrow (TVar (Unbound '_t/1 1)) (TVar (Unbound '_t/1 1)))))
      (EVar f (TArrow (TVar (Unbound '_t/2 0)) (TVar (Unbound '_t/2 0))))
      (TArrow (TVar (Unbound '_t/2 0)) (TVar (Unbound '_t/2 0))))
    |}];

  print_typed "let f = (fun x -> x) in f 1";
  [%expect
    {|
    (ELet f
      (ELam
        (x (EVar x (TVar (Unbound '_t/1 1)))
          (TArrow (TVar (Unbound '_t/1 1)) (TVar (Unbound '_t/1 1)))))
      (EApp
        (EVar f
          (TArrow (TVar (Link (TCons (0 int) ())))
            (TVar (Link (TCons (0 int) ())))))
        (EConst (CInt 1) (TCons (0 int) ())) (TVar (Link (TCons (0 int) ()))))
      (TVar (Link (TCons (0 int) ()))))
    |}];

  print_type "let f = (fun x -> x) in f 1";
  [%expect {|
    (TVar (Link (TCons (0 int) ()))) |}];

  print_typed "1, true";
  [%expect
    {|
    (ETuple
      ((EConst (CInt 1) (TCons (0 int) ()))
        (EConst (CBool true) (TCons (0 bool) ())))
      (TTuple ((TCons (0 int) ()) (TCons (0 bool) ())))) |}];

  print_typed "let f = (fun x -> x) in (f 1, f true)";
  [%expect
    {|
    (ELet f
      (ELam
        (x (EVar x (TVar (Unbound '_t/1 1)))
          (TArrow (TVar (Unbound '_t/1 1)) (TVar (Unbound '_t/1 1)))))
      (ETuple
        ((EApp
           (EVar f
             (TArrow (TVar (Link (TCons (0 int) ())))
               (TVar (Link (TCons (0 int) ())))))
           (EConst (CInt 1) (TCons (0 int) ())) (TVar (Link (TCons (0 int) ()))))
          (EApp
            (EVar f
              (TArrow (TVar (Link (TCons (0 bool) ())))
                (TVar (Link (TCons (0 bool) ())))))
            (EConst (CBool true) (TCons (0 bool) ()))
            (TVar (Link (TCons (0 bool) ())))))
        (TTuple
          ((TVar (Link (TCons (0 int) ()))) (TVar (Link (TCons (0 bool) ()))))))
      (TTuple
        ((TVar (Link (TCons (0 int) ()))) (TVar (Link (TCons (0 bool) ()))))))
    |}];
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
               (TVar
                 (Link
                   (TArrow (TVar (Link (TVar (Unbound '_t/5 1))))
                     (TVar (Link (TVar (Unbound 'ret/6 1))))))))
             (EVar x (TVar (Link (TVar (Unbound '_t/5 1)))))
             (TVar (Link (TVar (Unbound 'ret/6 1)))))
           (TArrow (TVar (Link (TVar (Unbound '_t/5 1))))
             (TVar (Link (TVar (Unbound 'ret/6 1)))))))
        (g
          (x
            (EApp
              (EVar f
                (TArrow (TVar (Link (TVar (Unbound '_t/5 1))))
                  (TVar (Link (TVar (Unbound 'ret/6 1))))))
              (EVar x (TVar (Unbound '_t/5 1))) (TVar (Unbound 'ret/6 1)))
            (TArrow (TVar (Unbound '_t/5 1)) (TVar (Unbound 'ret/6 1))))))
      (EConst (CInt 1) (TCons (0 int) ())) (TCons (0 int) ()))
    |}];

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
         (x (EVar x (TVar (Link (TCons (0 int) ()))))
           (TArrow (TVar (Link (TCons (0 int) ())))
             (TVar (Link (TCons (0 int) ()))))))
        (g
          (x
            (EApp
              (EVar f
                (TArrow (TVar (Link (TCons (0 int) ())))
                  (TVar (Link (TCons (0 int) ())))))
              (EConst (CInt 1) (TCons (0 int) ()))
              (TVar (Link (TCons (0 int) ()))))
            (TArrow (TVar (Unbound '_t/4 1)) (TVar (Link (TCons (0 int) ())))))))
      (EConst (CInt 1) (TCons (0 int) ())) (TCons (0 int) ()))
    |}]
(* todo: test pattern matching *)

let%expect_test "Test: program toplevel typing" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let typed, _env = Typing.Tools.type_check_program prog in
    typed |> T.sexp_of_program |> print_sexp
  in
  let print_effect str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let _typed, env = Typing.Tools.type_check_program prog in
    Printf.printf "%s\n" (Env.dbg env)
  in
  print_typed {|
     let x = 1
     |};
  [%expect {| ((TopLet x (EConst (CInt 1) (TCons (0 int) ())))) |}];
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
          (x (EVar x (TVar (Link (TCons (0 int) ()))))
            (TArrow (TVar (Link (TCons (0 int) ())))
              (TVar (Link (TCons (0 int) ()))))))
         (g
           (x
             (EApp
               (EVar f
                 (TArrow (TVar (Link (TCons (0 int) ())))
                   (TVar (Link (TCons (0 int) ())))))
               (EConst (CInt 1) (TCons (0 int) ()))
               (TVar (Link (TCons (0 int) ()))))
             (TArrow (TVar (Unbound '_t/4 1)) (TVar (Link (TCons (0 int) ())))))))))
    |}];
  print_effect {|
     type () a 
     = Cons of int
     | Nil 
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------

    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
    Value Bindings:

    Type Definitions:
      a |-> (TDAdt a () ((Cons ((TCons (0 int) ()))) (Nil ())))
    Module Definitions:

    Module Types:

    Module Creation History:

    Current Module Index:
      0
    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++

    ------------------Envirment Debug Info End-------------------------- |}];
  print_typed
    {|
     type () int_l 
     = Cons of int
     | Nil 
     let c = Nil
     let co = Cons 1
     |};
  [%expect
    {|
    ((TopTypeDef (TDAdt int_l () ((Cons ((TCons (0 int) ()))) (Nil ()))))
      (TopLet c (ECons Nil 1 (TCons (0 int_l) ())))
      (TopLet co
        (EApp (ECons Cons 0 (TArrow (TCons (0 int) ()) (TCons (0 int_l) ())))
          (EConst (CInt 1) (TCons (0 int) ()))
          (TVar (Link (TCons (0 int_l) ())))))) |}];
  print_effect
    {|
     type () int_l 
     = Cons of int
     | Nil 

     let c = Nil
     let co = Cons 1
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------

    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
    Value Bindings:
      co |-> forall  . (TCons (0 int_l) ());
      c |-> forall  . (TCons (0 int_l) ())
    Type Definitions:
      int_l |-> (TDAdt int_l () ((Cons ((TCons (0 int) ()))) (Nil ())))
    Module Definitions:

    Module Types:

    Module Creation History:

    Current Module Index:
      0
    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++

    ------------------Envirment Debug Info End-------------------------- |}];
  print_typed
    {|
     type () int_l
     = Cons of int
     | Nil

     let x = Nil
     let f =
         match x with
        | Cons x -> x
        | Nil    -> 0
|};
  [%expect
    {|
    ((TopTypeDef (TDAdt int_l () ((Cons ((TCons (0 int) ()))) (Nil ()))))
      (TopLet x (ECons Nil 1 (TCons (0 int_l) ())))
      (TopLet f
        (ECase (EVar x (TCons (0 int_l) ()))
          (((PCons Cons 0 ((PVar x (TCons (0 int) ()))))
             (EVar x (TCons (0 int) ())))
            ((PCons Nil 1 ()) (EConst (CInt 0) (TCons (0 int) ()))))
          (TVar (Link (TCons (0 int) ())))))) |}];

  print_typed
    {|
     type ('a, 'b) int_l
     = Cons of ('a * 'b)
     | Nil

     let x = Nil
     let f =
         match x with
        | Cons (a, b) -> (b, a)
     |};
  [%expect
    {|
    ((TopTypeDef
       (TDAdt int_l ('a/0 'b/0)
         ((Cons ((TTuple ((TQVar 'a/0) (TQVar 'b/0))))) (Nil ()))))
      (TopLet x
        (ECons Nil 1
          (TCons (0 int_l) ((TVar (Unbound 'a/1 1)) (TVar (Unbound 'b/2 1))))))
      (TopLet f
        (ECase
          (EVar x
            (TCons (0 int_l)
              ((TVar (Link (TVar (Unbound '_t/8 1))))
                (TVar (Link (TVar (Unbound '_t/9 1)))))))
          (((PCons Cons 0
              ((PTuple
                 ((PVar a (TVar (Unbound '_t/8 1)))
                   (PVar b (TVar (Unbound '_t/9 1)))))))
             (ETuple
               ((EVar b (TVar (Unbound '_t/9 1)))
                 (EVar a (TVar (Unbound '_t/8 1))))
               (TTuple ((TVar (Unbound '_t/9 1)) (TVar (Unbound '_t/8 1)))))))
          (TVar
            (Link (TTuple ((TVar (Unbound '_t/9 1)) (TVar (Unbound '_t/8 1)))))))))
    |}];
  print_effect
    {|
     type ('a, 'b) int_l
     = Cons of ('a * 'b)
     | Nil

     let x = Nil
     let f =
         match x with
        | Cons (a, b) -> (b, a)
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------

    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
    Value Bindings:
      f |-> forall '_t/8;'_t/9 . (TTuple ((TQVar '_t/9) (TQVar '_t/8)));
      x |-> forall 'b/2;'a/1 . (TCons (0 int_l) ((TQVar 'a/1) (TQVar 'b/2)))
    Type Definitions:
      int_l |-> (TDAdt int_l ('a/0 'b/0)
      ((Cons ((TTuple ((TQVar 'a/0) (TQVar 'b/0))))) (Nil ())))
    Module Definitions:

    Module Types:

    Module Creation History:

    Current Module Index:
      0
    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++

    ------------------Envirment Debug Info End--------------------------
    |}]

let%expect_test "Test: full program typing" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let typed, _env = Typing.Tools.type_check_program prog in
    typed |> T.sexp_of_program |> print_sexp
  in
  let print_effect str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let _typed, env = Typing.Tools.type_check_program prog in
    Printf.printf "%s\n" (Env.dbg env)
  in

  print_typed {| let x = 1 |};
  [%expect {| ((TopLet x (EConst (CInt 1) (TCons (0 int) ())))) |}];
  print_typed {| module M = struct let x = 1 end |};
  [%expect
    {|
    ((TopMod M
       (MEStruct ((TopLet x (EConst (CInt 1) (TCons (0 int) ()))))
         (MTMod
           ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
             (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
    |}];
  print_typed
    {|
     module M = struct let x = 1 end
     let c = M.x
     |};
  [%expect
    {|
    ((TopMod M
       (MEStruct ((TopLet x (EConst (CInt 1) (TCons (0 int) ()))))
         (MTMod
           ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
             (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
      (TopLet c
        (EField
          (MEName M
            (MTMod
              ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
                (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
          x (TCons (0 int) ()))))
    |}];
  print_typed
    {|
     module M =
       struct
         type () t = Nil


         let x = Nil
       end
     let c = M.x
     |};
  [%expect
    {|
    ((TopMod M
       (MEStruct
         ((TopTypeDef (TDAdt t () ((Nil ()))))
           (TopLet x (ECons Nil 0 (TCons (1 t) ()))))
         (MTMod
           ((id 1) (val_defs ((x (() (TCons (1 t) ())))))
             (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
             (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
             (owned_mods ())))))
      (TopLet c
        (EField
          (MEName M
            (MTMod
              ((id 1) (val_defs ((x (() (TCons (1 t) ())))))
                (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
                (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
                (owned_mods ()))))
          x (TCons (1 t) ()))))
    |}];

  print_typed
    {|
     module M =
       struct
         type () t = Nil

         let x = Nil
         module N =
           struct
             type () t = Nil

           end

         let z = N.Nil
       end
     let c = M.x
     let x = M.N.Nil
     let y = M.z
     |};
  [%expect
    {|
    ((TopMod M
       (MEStruct
         ((TopTypeDef (TDAdt t () ((Nil ()))))
           (TopLet x (ECons Nil 0 (TCons (1 t) ())))
           (TopMod N
             (MEStruct ((TopTypeDef (TDAdt t () ((Nil ())))))
               (MTMod
                 ((id 2) (val_defs ())
                   (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                   (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                   (mod_defs ()) (owned_mods ())))))
           (TopLet z
             (EFieldCons
               (MEName N
                 (MTMod
                   ((id 2) (val_defs ())
                     (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                     (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                     (mod_defs ()) (owned_mods ()))))
               Nil 0 (TCons (2 t) ()))))
         (MTMod
           ((id 1)
             (val_defs ((z (() (TCons (2 t) ()))) (x (() (TCons (1 t) ())))))
             (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
             (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
             (mod_defs
               ((N
                  (MTMod
                    ((id 2) (val_defs ())
                      (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                      (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                      (mod_defs ()) (owned_mods ()))))))
             (owned_mods (2))))))
      (TopLet c
        (EField
          (MEName M
            (MTMod
              ((id 1)
                (val_defs ((z (() (TCons (2 t) ()))) (x (() (TCons (1 t) ())))))
                (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
                (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                (mod_defs
                  ((N
                     (MTMod
                       ((id 2) (val_defs ())
                         (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                         (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                         (mod_defs ()) (owned_mods ()))))))
                (owned_mods (2)))))
          x (TCons (1 t) ())))
      (TopLet x
        (EFieldCons
          (MEField
            (MEName M
              (MTMod
                ((id 1)
                  (val_defs
                    ((z (() (TCons (2 t) ()))) (x (() (TCons (1 t) ())))))
                  (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
                  (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                  (mod_defs
                    ((N
                       (MTMod
                         ((id 2) (val_defs ())
                           (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                           (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                           (mod_defs ()) (owned_mods ()))))))
                  (owned_mods (2)))))
            N
            (MTMod
              ((id 2) (val_defs ())
                (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
                (owned_mods ()))))
          Nil 0 (TCons (2 t) ())))
      (TopLet y
        (EField
          (MEName M
            (MTMod
              ((id 1)
                (val_defs ((z (() (TCons (2 t) ()))) (x (() (TCons (1 t) ())))))
                (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
                (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                (mod_defs
                  ((N
                     (MTMod
                       ((id 2) (val_defs ())
                         (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                         (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ())
                         (mod_defs ()) (owned_mods ()))))))
                (owned_mods (2)))))
          z (TCons (2 t) ()))))
    |}];

  print_typed
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
         (MEStruct
           ((TopTypeDef (TDAdt t () ((Nil ()))))
             (TopLet x (ECons Nil 0 (TCons (1 t) ()))))
           (MTMod
             ((id 1) (val_defs ((x (() (TCons (1 t) ())))))
               (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
               (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
               (owned_mods ()))))
         (MTMod
           ((id 2) (val_defs ((x (() (TCons (2 t) ()))))) (constr_defs ())
             (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
             (owned_mods ())))
         (MTMod
           ((id 3) (val_defs ((x (() (TCons (3 t) ()))))) (constr_defs ())
             (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
             (owned_mods ()))))))
    |}];

  print_typed
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
       (MTMod
         ((id 1) (val_defs ((x (() (TCons (1 t) ())))))
           (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
           (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
           (owned_mods ())))))
    |}];

  print_effect
    {|
     module type MIntf = 
     sig 
       type () t = Nil

       val x : t
     end 
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------

    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
    Value Bindings:

    Type Definitions:

    Module Definitions:

    Module Types:
      MIntf |-> (MTMod
      ((id 1) (val_defs ((x (() (TCons (1 t) ())))))
        (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
        (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
        (owned_mods ())))
    Module Creation History:
      1
    Current Module Index:
      0
    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++

    ------------------Envirment Debug Info End--------------------------
    |}];

  print_typed
    {|
     module type MIntf = 
     sig 
       type () t

       val x : t
     end 
     |};
  [%expect
    {|
    ((TopModSig MIntf
       (MTMod
         ((id 1) (val_defs ((x (() (TCons (1 t) ()))))) (constr_defs ())
           (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
           (owned_mods ())))))
    |}];

  print_typed
    {|
     module type MIntf = 
     sig 
       type () t

       val x : t
     end 

     module MImpl = 
     struct 
       type () t = Nil

       let z = 1       

       let x = Nil
     end : MIntf

     |};
  [%expect
    {|
    ((TopModSig MIntf
       (MTMod
         ((id 1) (val_defs ((x (() (TCons (1 t) ()))))) (constr_defs ())
           (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
           (owned_mods ()))))
      (TopMod MImpl
        (MERestrict
          (MEStruct
            ((TopTypeDef (TDAdt t () ((Nil ()))))
              (TopLet z (EConst (CInt 1) (TCons (0 int) ())))
              (TopLet x (ECons Nil 0 (TCons (2 t) ()))))
            (MTMod
              ((id 2)
                (val_defs
                  ((x (() (TCons (2 t) ()))) (z (() (TCons (0 int) ())))))
                (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
                (owned_mods ()))))
          (MTMod
            ((id 1) (val_defs ((x (() (TCons (1 t) ()))))) (constr_defs ())
              (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (MTMod
            ((id 3) (val_defs ((x (() (TCons (3 t) ()))))) (constr_defs ())
              (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
              (owned_mods ()))))))
    |}];

  print_typed
    {|
     module type I = sig
        val x : int

        val y : int
     end

     module type J = sig
        val x : int

        val y : int

        val z : int
     end

module MJ = struct
  let x = 1

  let y = 1

  let z = 1
end

module Simple = struct
  let x = 1

  let y = 2
end

module M =
functor
  (MI : functor (MI : I) -> I)
  ->
  struct
    module K = MI (Simple)
  end

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

module MMM = (M(F).K : I)
     |};
  [%expect
    {|
    ((TopModSig I
       (MTMod
         ((id 1)
           (val_defs ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
           (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
           (owned_mods ()))))
      (TopModSig J
        (MTMod
          ((id 2)
            (val_defs
              ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
                (x (() (TCons (0 int) ())))))
            (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
            (owned_mods ()))))
      (TopMod MJ
        (MEStruct
          ((TopLet x (EConst (CInt 1) (TCons (0 int) ())))
            (TopLet y (EConst (CInt 1) (TCons (0 int) ())))
            (TopLet z (EConst (CInt 1) (TCons (0 int) ()))))
          (MTMod
            ((id 3)
              (val_defs
                ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
                  (x (() (TCons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))))
      (TopMod Simple
        (MEStruct
          ((TopLet x (EConst (CInt 1) (TCons (0 int) ())))
            (TopLet y (EConst (CInt 2) (TCons (0 int) ()))))
          (MTMod
            ((id 4)
              (val_defs
                ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))))
      (TopMod M
        (MEFunctor
          (MI
            (MTFun
              ((MTMod
                 ((id 1)
                   (val_defs
                     ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
                   (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                   (owned_mods ())))
                (MTMod
                  ((id 1)
                    (val_defs
                      ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
                    (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                    (owned_mods ()))))))
          (MEStruct
            ((TopMod K
               (MEApply
                 (MEName MI
                   (MTFun
                     ((MTMod
                        ((id 1)
                          (val_defs
                            ((y (() (TCons (0 int) ())))
                              (x (() (TCons (0 int) ())))))
                          (constr_defs ()) (ty_defs ()) (mod_sigs ())
                          (mod_defs ()) (owned_mods ())))
                       (MTMod
                         ((id 1)
                           (val_defs
                             ((y (() (TCons (0 int) ())))
                               (x (() (TCons (0 int) ())))))
                           (constr_defs ()) (ty_defs ()) (mod_sigs ())
                           (mod_defs ()) (owned_mods ()))))))
                 (MEName Simple
                   (MTMod
                     ((id 4)
                       (val_defs
                         ((y (() (TCons (0 int) ())))
                           (x (() (TCons (0 int) ())))))
                       (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                       (owned_mods ()))))
                 (MTMod
                   ((id 6)
                     (val_defs
                       ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
                     (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                     (owned_mods ()))))))
            (MTMod
              ((id 5) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
                (mod_defs
                  ((K
                     (MTMod
                       ((id 6)
                         (val_defs
                           ((y (() (TCons (0 int) ())))
                             (x (() (TCons (0 int) ())))))
                         (constr_defs ()) (ty_defs ()) (mod_sigs ())
                         (mod_defs ()) (owned_mods ()))))))
                (owned_mods (6)))))))
      (TopMod F
        (MEFunctor
          (MI
            (MTMod
              ((id 1)
                (val_defs
                  ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
                (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                (owned_mods ()))))
          (MERestrict
            (MEStruct
              ((TopLet x (EConst (CInt 1) (TCons (0 int) ())))
                (TopLet y (EConst (CInt 1) (TCons (0 int) ())))
                (TopLet z (EConst (CInt 1) (TCons (0 int) ()))))
              (MTMod
                ((id 7)
                  (val_defs
                    ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
                      (x (() (TCons (0 int) ())))))
                  (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                  (owned_mods ()))))
            (MTMod
              ((id 2)
                (val_defs
                  ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
                    (x (() (TCons (0 int) ())))))
                (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                (owned_mods ())))
            (MTMod
              ((id 8)
                (val_defs
                  ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
                    (x (() (TCons (0 int) ())))))
                (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                (owned_mods ()))))))
      (TopMod MMM
        (MERestrict
          (MEField
            (MEApply
              (MEName M
                (MTFun
                  ((MTFun
                     ((MTMod
                        ((id 1)
                          (val_defs
                            ((y (() (TCons (0 int) ())))
                              (x (() (TCons (0 int) ())))))
                          (constr_defs ()) (ty_defs ()) (mod_sigs ())
                          (mod_defs ()) (owned_mods ())))
                       (MTMod
                         ((id 1)
                           (val_defs
                             ((y (() (TCons (0 int) ())))
                               (x (() (TCons (0 int) ())))))
                           (constr_defs ()) (ty_defs ()) (mod_sigs ())
                           (mod_defs ()) (owned_mods ())))))
                    (MTMod
                      ((id 5) (val_defs ()) (constr_defs ()) (ty_defs ())
                        (mod_sigs ())
                        (mod_defs
                          ((K
                             (MTMod
                               ((id 6)
                                 (val_defs
                                   ((y (() (TCons (0 int) ())))
                                     (x (() (TCons (0 int) ())))))
                                 (constr_defs ()) (ty_defs ()) (mod_sigs ())
                                 (mod_defs ()) (owned_mods ()))))))
                        (owned_mods (6)))))))
              (MEName F
                (MTFun
                  ((MTMod
                     ((id 1)
                       (val_defs
                         ((y (() (TCons (0 int) ())))
                           (x (() (TCons (0 int) ())))))
                       (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                       (owned_mods ())))
                    (MTMod
                      ((id 8)
                        (val_defs
                          ((z (() (TCons (0 int) ())))
                            (y (() (TCons (0 int) ())))
                            (x (() (TCons (0 int) ())))))
                        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                        (owned_mods ()))))))
              (MTMod
                ((id 9) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
                  (mod_defs
                    ((K
                       (MTMod
                         ((id 10)
                           (val_defs
                             ((y (() (TCons (0 int) ())))
                               (x (() (TCons (0 int) ())))))
                           (constr_defs ()) (ty_defs ()) (mod_sigs ())
                           (mod_defs ()) (owned_mods ()))))))
                  (owned_mods (10)))))
            K
            (MTMod
              ((id 10)
                (val_defs
                  ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
                (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                (owned_mods ()))))
          (MTMod
            ((id 1)
              (val_defs
                ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (MTMod
            ((id 11)
              (val_defs
                ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ()))))))
    |}];

  print_effect
    {|
     module type I = sig
        val x : int

        val y : int
     end

     module type J = sig
        val x : int

        val y : int

        val z : int
     end

module MJ = struct
  let x = 1

  let y = 1

  let z = 1
end

module Simple = struct
  let x = 1

  let y = 2
end

module M =
functor
  (MI : functor (MI : I) -> I)
  ->
  struct
    module K = MI (Simple)
    module K2 = MI (MJ)
  end

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

module MMM = (M(F).K : I)
     |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------

    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
    Value Bindings:

    Type Definitions:

    Module Definitions:
      MMM |-> (MTMod
      ((id 13)
        (val_defs ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
        (owned_mods ())));
     F |-> (MTFun
      ((MTMod
         ((id 1)
           (val_defs ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
           (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
           (owned_mods ())))
        (MTMod
          ((id 9)
            (val_defs
              ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
                (x (() (TCons (0 int) ())))))
            (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
            (owned_mods ())))));
     M |-> (MTFun
      ((MTFun
         ((MTMod
            ((id 1)
              (val_defs
                ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
           (MTMod
             ((id 1)
               (val_defs
                 ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
               (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
               (owned_mods ())))))
        (MTMod
          ((id 5) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
            (mod_defs
              ((K2
                 (MTMod
                   ((id 7)
                     (val_defs
                       ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
                     (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                     (owned_mods ()))))
                (K
                  (MTMod
                    ((id 6)
                      (val_defs
                        ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
                      (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                      (owned_mods ()))))))
            (owned_mods (7 6))))));
     Simple |-> (MTMod
      ((id 4)
        (val_defs ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
        (owned_mods ())));
     MJ |-> (MTMod
      ((id 3)
        (val_defs
          ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
            (x (() (TCons (0 int) ())))))
        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
        (owned_mods ())))
    Module Types:
      J |-> (MTMod
      ((id 2)
        (val_defs
          ((z (() (TCons (0 int) ()))) (y (() (TCons (0 int) ())))
            (x (() (TCons (0 int) ())))))
        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
        (owned_mods ())));
     I |-> (MTMod
      ((id 1)
        (val_defs ((y (() (TCons (0 int) ()))) (x (() (TCons (0 int) ())))))
        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
        (owned_mods ())))
    Module Creation History:
      13;
      12;
      11;
      10;
      9;
      8;
      7;
      6;
      5;
      4;
      3;
      2;
      1
    Current Module Index:
      0
    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++

    ------------------Envirment Debug Info End--------------------------
    |}];

  print_typed
    {|
external add : int -> int -> int = "ff_add"

external print_int : int ->  int = "ff_builtin_print_int"
               |};
  [%expect
    {|
    ((TopExternal add
       (TArrow (TCons (0 int) ()) (TArrow (TCons (0 int) ()) (TCons (0 int) ())))
       ff_add)
      (TopExternal print_int (TArrow (TCons (0 int) ()) (TCons (0 int) ()))
        ff_builtin_print_int))
    |}];

  print_typed {|
     let x = 1
     let n = x = 1
|};
  [%expect
    {|
    ((TopLet x (EConst (CInt 1) (TCons (0 int) ())))
      (TopLet n
        (ECmp Eq (EVar x (TCons (0 int) ())) (EConst (CInt 1) (TCons (0 int) ()))
          (TCons (0 bool) ()))))
    |}];

  print_effect {|
               let rec id = fun x -> x
               |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------

    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
    Value Bindings:
      id |-> forall '_t/2 . (TArrow (TQVar '_t/2) (TQVar '_t/2))
    Type Definitions:

    Module Definitions:

    Module Types:

    Module Creation History:

    Current Module Index:
      0
    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++

    ------------------Envirment Debug Info End--------------------------
     |}];
  print_typed
    {|
     module M = struct
       module type N = sig
       end
     end

     module F = functor (X:M.N) -> struct end
     |};
  [%expect
    {|
    ((TopMod M
       (MEStruct
         ((TopModSig N
            (MTMod
              ((id 2) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
                (mod_defs ()) (owned_mods ())))))
         (MTMod
           ((id 1) (val_defs ()) (constr_defs ()) (ty_defs ())
             (mod_sigs
               ((N
                  (MTMod
                    ((id 2) (val_defs ()) (constr_defs ()) (ty_defs ())
                      (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
             (mod_defs ()) (owned_mods (2))))))
      (TopMod F
        (MEFunctor
          (X
            (MTMod
              ((id 2) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
                (mod_defs ()) (owned_mods ()))))
          (MEStruct ()
            (MTMod
              ((id 3) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
                (mod_defs ()) (owned_mods ())))))))
    |}];
  print_typed
    {|
     module F = ((struct
       type t = int
     end : sig
       type () t
     end) : sig
       type () t
     end)
     |};
  [%expect
    {|
    ((TopMod F
       (MERestrict
         (MERestrict
           (MEStruct ((TopTypeDef (TDAlias t (TCons (0 int) ()))))
             (MTMod
               ((id 1) (val_defs ()) (constr_defs ())
                 (ty_defs ((TDAlias t (TCons (0 int) ())))) (mod_sigs ())
                 (mod_defs ()) (owned_mods ()))))
           (MTMod
             ((id 2) (val_defs ()) (constr_defs ()) (ty_defs ((TDOpaque t ())))
               (mod_sigs ()) (mod_defs ()) (owned_mods ())))
           (MTMod
             ((id 3) (val_defs ()) (constr_defs ()) (ty_defs ((TDOpaque t ())))
               (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
         (MTMod
           ((id 4) (val_defs ()) (constr_defs ()) (ty_defs ((TDOpaque t ())))
             (mod_sigs ()) (mod_defs ()) (owned_mods ())))
         (MTMod
           ((id 5) (val_defs ()) (constr_defs ()) (ty_defs ((TDOpaque t ())))
             (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
    |}];

  print_typed
    {|
               type additive =
               | Add of (atom * atom)
               and () multiple = 
               | Mul of (additive * additive)
               | Div of (additive * additive)
               and atom =
               | Int of int
               |};
  [%expect
    {|
    ((TopTypeDef (TDAdt atom () ((Int ((TCons (0 int) ()))))))
      (TopTypeDef
        (TDAdt multiple ()
          ((Mul ((TTuple ((TCons (0 additive) ()) (TCons (0 additive) ())))))
            (Div ((TTuple ((TCons (0 additive) ()) (TCons (0 additive) ()))))))))
      (TopTypeDef
        (TDAdt additive ()
          ((Add ((TTuple ((TCons (0 atom) ()) (TCons (0 atom) ())))))))))
    |}];
  print_typed
    {|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)

type env = int list

and t =
  | Closure of (string * env)

let x = Closure ("x", Nil)

               |};
  [%expect
    {|
    ((TopTypeDef
       (TDAdt list ('a/0)
         ((Nil ())
           (Cons ((TTuple ((TQVar 'a/0) (TCons (0 list) ((TQVar 'a/0))))))))))
      (TopTypeDef
        (TDAdt t ()
          ((Closure
             ((TTuple
                ((TCons (0 string) ()) (TCons (0 list) ((TCons (0 int) ()))))))))))
      (TopTypeDef (TDAlias env (TCons (0 list) ((TCons (0 int) ())))))
      (TopLet x
        (EApp
          (ECons Closure 0
            (TArrow
              (TTuple
                ((TCons (0 string) ()) (TCons (0 list) ((TCons (0 int) ())))))
              (TCons (0 t) ())))
          (ETuple
            ((EConst (CString "\"x\"") (TCons (0 string) ()))
              (ECons Nil 0 (TCons (0 list) ((TVar (Link (TCons (0 int) ())))))))
            (TTuple
              ((TCons (0 string) ())
                (TCons (0 list) ((TVar (Link (TCons (0 int) ()))))))))
          (TVar (Link (TCons (0 t) ()))))))
    |}];

  print_typed {|
let x = assert true
|};
  [%expect
    {|
    ((TopLet x
       (EAssert (EConst (CBool true) (TCons (0 bool) ())) (TCons (0 unit) ()))))
    |}]

let%expect_test "Error reporting test" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let result =
      Report.wrap_with_error_report (fun () ->
          Typing.Tools.type_check_program prog)
    in
    match result with
    | Some (typed, _env) -> typed |> T.sexp_of_program |> print_sexp
    | None -> ()
  in
  print_typed {|
     let x = 1
     let y = true
     let z = x = y
     |};
  [%expect {| 4:13-4:13 Can't unify `() int` with `() bool` |}];

  print_typed
    {|
             module type I = sig
               val x : int
             end
             
             module F =  (struct
               type t = int

               let y = 1
             end : sig
               type () t

               val y : () t
             end)
     |};

  print_typed
    {|
             module type I = sig
               val x : int
             end
             
             module F = functor (X:I) -> (struct
               type t = int

               let y = X.x
             end : sig
               type () t

               val y : () t
             end)

     |};
  [%expect
    {|
    ((TopModSig I
       (MTMod
         ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
           (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
      (TopMod F
        (MERestrict
          (MEStruct
            ((TopTypeDef (TDAlias t (TCons (0 int) ())))
              (TopLet y (EConst (CInt 1) (TCons (0 int) ()))))
            (MTMod
              ((id 2) (val_defs ((y (() (TCons (0 int) ()))))) (constr_defs ())
                (ty_defs ((TDAlias t (TCons (0 int) ())))) (mod_sigs ())
                (mod_defs ()) (owned_mods ()))))
          (MTMod
            ((id 3) (val_defs ((y (() (TCons (3 t) ()))))) (constr_defs ())
              (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (MTMod
            ((id 4) (val_defs ((y (() (TCons (4 t) ()))))) (constr_defs ())
              (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
              (owned_mods ()))))))
    ((TopModSig I
       (MTMod
         ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
           (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
      (TopMod F
        (MEFunctor
          (X
            (MTMod
              ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
                (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
          (MERestrict
            (MEStruct
              ((TopTypeDef (TDAlias t (TCons (0 int) ())))
                (TopLet y
                  (EField
                    (MEName X
                      (MTMod
                        ((id 1) (val_defs ((x (() (TCons (0 int) ())))))
                          (constr_defs ()) (ty_defs ()) (mod_sigs ())
                          (mod_defs ()) (owned_mods ()))))
                    x (TCons (0 int) ()))))
              (MTMod
                ((id 2) (val_defs ((y (() (TCons (0 int) ()))))) (constr_defs ())
                  (ty_defs ((TDAlias t (TCons (0 int) ())))) (mod_sigs ())
                  (mod_defs ()) (owned_mods ()))))
            (MTMod
              ((id 3) (val_defs ((y (() (TCons (3 t) ()))))) (constr_defs ())
                (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
                (owned_mods ())))
            (MTMod
              ((id 4) (val_defs ((y (() (TCons (4 t) ()))))) (constr_defs ())
                (ty_defs ((TDOpaque t ()))) (mod_sigs ()) (mod_defs ())
                (owned_mods ())))))))
    |}];

  print_typed
    {|
             module type I = sig
               val x : int
             end
             
             module F = functor (X:I) -> (struct
               type t = int

               let y = X.x
             end : sig
               type () t

               val y : () t
             end)


             module X = struct
               let x = 1
             end

             module Y1 = F (X)

             module Y2 = F (X)

             let z = Y1.y = Y2.y
     |};
  [%expect {| 25:21-25:21 Can't unify `() Y1.t` with `() Y2.t` |}];
  print_typed
    {|
             module type I = sig
               val x : int
             end
             
             module F = functor (X:I) -> (struct
               type t = int

               type n = t

               let y = 3
             end : sig
               type () n 

               type () t

               val y : () t
             end)
     |};
  [%expect
    {|
    ((TopModSig I
       (MTMod
         ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
           (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
      (TopMod F
        (MEFunctor
          (X
            (MTMod
              ((id 1) (val_defs ((x (() (TCons (0 int) ()))))) (constr_defs ())
                (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
          (MERestrict
            (MEStruct
              ((TopTypeDef (TDAlias t (TCons (0 int) ())))
                (TopTypeDef (TDAlias n (TCons (0 int) ())))
                (TopLet y (EConst (CInt 3) (TCons (0 int) ()))))
              (MTMod
                ((id 2) (val_defs ((y (() (TCons (0 int) ()))))) (constr_defs ())
                  (ty_defs
                    ((TDAlias n (TCons (0 int) ()))
                      (TDAlias t (TCons (0 int) ()))))
                  (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
            (MTMod
              ((id 3) (val_defs ((y (() (TCons (3 t) ()))))) (constr_defs ())
                (ty_defs ((TDOpaque t ()) (TDOpaque n ()))) (mod_sigs ())
                (mod_defs ()) (owned_mods ())))
            (MTMod
              ((id 4) (val_defs ((y (() (TCons (4 t) ()))))) (constr_defs ())
                (ty_defs ((TDOpaque t ()) (TDOpaque n ()))) (mod_sigs ())
                (mod_defs ()) (owned_mods ())))))))
    |}];

  print_typed
    {|
             let x = 
                let y  = (1: 'a , 1) in
                let z = (2, 1: 'a) in
                (y, z)
             |};
  [%expect
    {|
    ((TopLet x
       (ELet y
         (ETuple
           ((EConst (CInt 1) (TCons (0 int) ()))
             (EConst (CInt 1) (TCons (0 int) ())))
           (TTuple ((TCons (0 int) ()) (TCons (0 int) ()))))
         (ELet z
           (ETuple
             ((EConst (CInt 2) (TCons (0 int) ()))
               (EConst (CInt 1) (TCons (0 int) ())))
             (TTuple ((TCons (0 int) ()) (TCons (0 int) ()))))
           (ETuple
             ((EVar y (TTuple ((TCons (0 int) ()) (TCons (0 int) ()))))
               (EVar z (TTuple ((TCons (0 int) ()) (TCons (0 int) ())))))
             (TTuple
               ((TTuple ((TCons (0 int) ()) (TCons (0 int) ())))
                 (TTuple ((TCons (0 int) ()) (TCons (0 int) ()))))))
           (TTuple
             ((TTuple ((TCons (0 int) ()) (TCons (0 int) ())))
               (TTuple ((TCons (0 int) ()) (TCons (0 int) ()))))))
         (TTuple
           ((TTuple ((TCons (0 int) ()) (TCons (0 int) ())))
             (TTuple ((TCons (0 int) ()) (TCons (0 int) ()))))))))
    |}];

  print_typed
    {|
module type M = sig

             module N : sig

               type () t 

               type () s

             end
             
end


module K = struct

             module N = struct 

               type t = int

               type s = int
             end
end

module L1 = (K: M)
module L2 = (K: M)

             |};
  [%expect
    {|
    ((TopModSig M
       (MTMod
         ((id 1) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
           (mod_defs
             ((N
                (MTMod
                  ((id 2) (val_defs ()) (constr_defs ())
                    (ty_defs ((TDOpaque s ()) (TDOpaque t ()))) (mod_sigs ())
                    (mod_defs ()) (owned_mods ()))))))
           (owned_mods (2)))))
      (TopMod K
        (MEStruct
          ((TopMod N
             (MEStruct
               ((TopTypeDef (TDAlias t (TCons (0 int) ())))
                 (TopTypeDef (TDAlias s (TCons (0 int) ()))))
               (MTMod
                 ((id 4) (val_defs ()) (constr_defs ())
                   (ty_defs
                     ((TDAlias s (TCons (0 int) ()))
                       (TDAlias t (TCons (0 int) ()))))
                   (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
          (MTMod
            ((id 3) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
              (mod_defs
                ((N
                   (MTMod
                     ((id 4) (val_defs ()) (constr_defs ())
                       (ty_defs
                         ((TDAlias s (TCons (0 int) ()))
                           (TDAlias t (TCons (0 int) ()))))
                       (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
              (owned_mods (4))))))
      (TopMod L1
        (MERestrict
          (MEName K
            (MTMod
              ((id 3) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
                (mod_defs
                  ((N
                     (MTMod
                       ((id 4) (val_defs ()) (constr_defs ())
                         (ty_defs
                           ((TDAlias s (TCons (0 int) ()))
                             (TDAlias t (TCons (0 int) ()))))
                         (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
                (owned_mods (4)))))
          (MTMod
            ((id 1) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
              (mod_defs
                ((N
                   (MTMod
                     ((id 2) (val_defs ()) (constr_defs ())
                       (ty_defs ((TDOpaque s ()) (TDOpaque t ()))) (mod_sigs ())
                       (mod_defs ()) (owned_mods ()))))))
              (owned_mods (2))))
          (MTMod
            ((id 5) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
              (mod_defs
                ((N
                   (MTMod
                     ((id 6) (val_defs ()) (constr_defs ())
                       (ty_defs ((TDOpaque s ()) (TDOpaque t ()))) (mod_sigs ())
                       (mod_defs ()) (owned_mods ()))))))
              (owned_mods (6))))))
      (TopMod L2
        (MERestrict
          (MEName K
            (MTMod
              ((id 3) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
                (mod_defs
                  ((N
                     (MTMod
                       ((id 4) (val_defs ()) (constr_defs ())
                         (ty_defs
                           ((TDAlias s (TCons (0 int) ()))
                             (TDAlias t (TCons (0 int) ()))))
                         (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
                (owned_mods (4)))))
          (MTMod
            ((id 1) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
              (mod_defs
                ((N
                   (MTMod
                     ((id 2) (val_defs ()) (constr_defs ())
                       (ty_defs ((TDOpaque s ()) (TDOpaque t ()))) (mod_sigs ())
                       (mod_defs ()) (owned_mods ()))))))
              (owned_mods (2))))
          (MTMod
            ((id 7) (val_defs ()) (constr_defs ()) (ty_defs ()) (mod_sigs ())
              (mod_defs
                ((N
                   (MTMod
                     ((id 8) (val_defs ()) (constr_defs ())
                       (ty_defs ((TDOpaque s ()) (TDOpaque t ()))) (mod_sigs ())
                       (mod_defs ()) (owned_mods ()))))))
              (owned_mods (8)))))))
    |}];

  print_typed
    {|
               let result =
                 let id = fun x -> x in
                 (id 1, id "xx")
               |};
  [%expect
    {|
    ((TopLet result
       (ELet id
         (ELam
           (x (EVar x (TVar (Unbound '_t/1 4)))
             (TArrow (TVar (Unbound '_t/1 4)) (TVar (Unbound '_t/1 4)))))
         (ETuple
           ((EApp
              (EVar id
                (TArrow (TVar (Link (TCons (0 int) ())))
                  (TVar (Link (TCons (0 int) ())))))
              (EConst (CInt 1) (TCons (0 int) ()))
              (TVar (Link (TCons (0 int) ()))))
             (EApp
               (EVar id
                 (TArrow (TVar (Link (TCons (0 string) ())))
                   (TVar (Link (TCons (0 string) ())))))
               (EConst (CString "\"xx\"") (TCons (0 string) ()))
               (TVar (Link (TCons (0 string) ())))))
           (TTuple
             ((TVar (Link (TCons (0 int) ())))
               (TVar (Link (TCons (0 string) ()))))))
         (TTuple
           ((TVar (Link (TCons (0 int) ()))) (TVar (Link (TCons (0 string) ()))))))))
    |}];

  print_typed {|
             let _ = 1

             let result = _
|};
  [%expect {| 4:26-4:26 name `_` not found |}];
  print_typed
    {|
     module M = struct end
     module X = M
     module Bad = M(M)
     |};
  [%expect {| 4:18-4:18 try apply a structure |}];
  print_typed
    {|
     module type M = sig end
     module F = functor (X:M) -> struct end
     let x = F.x
     |};
  [%expect {| 4:13-4:13 try get field from functor |}];
  print_typed
    {|
     module M = functor(X:sig end) -> struct
     end
 
     module F = M.N
     |};
  [%expect {| 5:16-5:16 try get field from functor |}];
  print_typed
    {|
               module type MT = sig
                 type t = | Nil
               end
               
               module M = (struct
                 type t = | Nil
               end : MT)

               |};
  [%expect
    {|
    ((TopModSig MT
       (MTMod
         ((id 1) (val_defs ()) (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
           (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
           (owned_mods ()))))
      (TopMod M
        (MERestrict
          (MEStruct ((TopTypeDef (TDAdt t () ((Nil ())))))
            (MTMod
              ((id 2) (val_defs ())
                (constr_defs ((Nil ((() (TCons (2 t) ())) 0))))
                (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
                (owned_mods ()))))
          (MTMod
            ((id 1) (val_defs ()) (constr_defs ((Nil ((() (TCons (1 t) ())) 0))))
              (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (MTMod
            ((id 3) (val_defs ()) (constr_defs ((Nil ((() (TCons (3 t) ())) 0))))
              (ty_defs ((TDAdt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
              (owned_mods ()))))))
    |}];

  print_typed
    {|
     module type S = sig
       type () t
     end
     
     module X = (struct end : S)
               |};
  [%expect
    {|
    6:17-6:17 Type definition component t not exists in module struct

    end
    |}];
  print_typed
    {|
     module type S = sig
       val x : int
     end
     
     module X = (struct end : S)
               |};
  [%expect
    {|
    6:17-6:17 Value component x not exists in module struct

    end
    |}];
  print_typed
    {|
     module type S = sig
       module M : sig end
     end
     
     module X = (struct end : S)
     |};
  [%expect
    {|
    6:17-6:17 Module component M not exists in module struct

    end
    |}];

  print_typed
    {|
     module type S = sig
       val x : int
     end
     
     module X = (struct let x = "" end : S)
     |};
  [%expect
    {| 6:17-6:17 Value component x has type `() string`, which is not compatible with `() int` |}]
