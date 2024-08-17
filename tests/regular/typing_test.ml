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
  [%expect {| (Exp_const (Const_int 1) (Ty_cons (0 int) ())) |}];

  print_type "1";
  [%expect {| (Ty_cons (0 int) ()) |}];

  print_typed "let x = 1 in x";
  [%expect
    {|
    (Exp_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ()))
      (Exp_var x (Ty_cons (0 int) ())) (Ty_cons (0 int) ()))
    |}];

  print_type "let x = 1 in x";
  [%expect {| (Ty_cons (0 int) ()) |}];

  print_typed "let f = (fun x -> x) in f";
  [%expect
    {|
    (Exp_let f
      (Exp_lam
        (x (Exp_var x (Ty_var (Unbound '_t/1 1)))
          (Ty_arrow (Ty_var (Unbound '_t/1 1)) (Ty_var (Unbound '_t/1 1)))))
      (Exp_var f
        (Ty_arrow (Ty_var (Unbound '_t/2 0)) (Ty_var (Unbound '_t/2 0))))
      (Ty_arrow (Ty_var (Unbound '_t/2 0)) (Ty_var (Unbound '_t/2 0))))
    |}];

  print_typed "let f = (fun x -> x) in f 1";
  [%expect
    {|
    (Exp_let f
      (Exp_lam
        (x (Exp_var x (Ty_var (Unbound '_t/1 1)))
          (Ty_arrow (Ty_var (Unbound '_t/1 1)) (Ty_var (Unbound '_t/1 1)))))
      (Exp_app
        (Exp_var f
          (Ty_arrow (Ty_var (Link (Ty_cons (0 int) ())))
            (Ty_var (Link (Ty_cons (0 int) ())))))
        (Exp_const (Const_int 1) (Ty_cons (0 int) ()))
        (Ty_var (Link (Ty_cons (0 int) ()))))
      (Ty_var (Link (Ty_cons (0 int) ()))))
    |}];

  print_type "let f = (fun x -> x) in f 1";
  [%expect {|
    (Ty_var (Link (Ty_cons (0 int) ()))) |}];

  print_typed "1, true";
  [%expect
    {|
    (Exp_tuple
      ((Exp_const (Const_int 1) (Ty_cons (0 int) ()))
        (Exp_const (Const_bool true) (Ty_cons (0 bool) ())))
      (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 bool) ())))) |}];

  print_typed "let f = (fun x -> x) in (f 1, f true)";
  [%expect
    {|
    (Exp_let f
      (Exp_lam
        (x (Exp_var x (Ty_var (Unbound '_t/1 1)))
          (Ty_arrow (Ty_var (Unbound '_t/1 1)) (Ty_var (Unbound '_t/1 1)))))
      (Exp_tuple
        ((Exp_app
           (Exp_var f
             (Ty_arrow (Ty_var (Link (Ty_cons (0 int) ())))
               (Ty_var (Link (Ty_cons (0 int) ())))))
           (Exp_const (Const_int 1) (Ty_cons (0 int) ()))
           (Ty_var (Link (Ty_cons (0 int) ()))))
          (Exp_app
            (Exp_var f
              (Ty_arrow (Ty_var (Link (Ty_cons (0 bool) ())))
                (Ty_var (Link (Ty_cons (0 bool) ())))))
            (Exp_const (Const_bool true) (Ty_cons (0 bool) ()))
            (Ty_var (Link (Ty_cons (0 bool) ())))))
        (Ty_tuple
          ((Ty_var (Link (Ty_cons (0 int) ())))
            (Ty_var (Link (Ty_cons (0 bool) ()))))))
      (Ty_tuple
        ((Ty_var (Link (Ty_cons (0 int) ())))
          (Ty_var (Link (Ty_cons (0 bool) ()))))))
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
    (Exp_letrec
      ((f
         (x
           (Exp_app
             (Exp_var g
               (Ty_var
                 (Link
                   (Ty_arrow (Ty_var (Link (Ty_var (Unbound '_t/5 1))))
                     (Ty_var (Link (Ty_var (Unbound 'ret/6 1))))))))
             (Exp_var x (Ty_var (Link (Ty_var (Unbound '_t/5 1)))))
             (Ty_var (Link (Ty_var (Unbound 'ret/6 1)))))
           (Ty_arrow (Ty_var (Link (Ty_var (Unbound '_t/5 1))))
             (Ty_var (Link (Ty_var (Unbound 'ret/6 1)))))))
        (g
          (x
            (Exp_app
              (Exp_var f
                (Ty_arrow (Ty_var (Link (Ty_var (Unbound '_t/5 1))))
                  (Ty_var (Link (Ty_var (Unbound 'ret/6 1))))))
              (Exp_var x (Ty_var (Unbound '_t/5 1))) (Ty_var (Unbound 'ret/6 1)))
            (Ty_arrow (Ty_var (Unbound '_t/5 1)) (Ty_var (Unbound 'ret/6 1))))))
      (Exp_const (Const_int 1) (Ty_cons (0 int) ())) (Ty_cons (0 int) ()))
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
    (Exp_letrec
      ((f
         (x (Exp_var x (Ty_var (Link (Ty_cons (0 int) ()))))
           (Ty_arrow (Ty_var (Link (Ty_cons (0 int) ())))
             (Ty_var (Link (Ty_cons (0 int) ()))))))
        (g
          (x
            (Exp_app
              (Exp_var f
                (Ty_arrow (Ty_var (Link (Ty_cons (0 int) ())))
                  (Ty_var (Link (Ty_cons (0 int) ())))))
              (Exp_const (Const_int 1) (Ty_cons (0 int) ()))
              (Ty_var (Link (Ty_cons (0 int) ()))))
            (Ty_arrow (Ty_var (Unbound '_t/4 1))
              (Ty_var (Link (Ty_cons (0 int) ())))))))
      (Exp_const (Const_int 1) (Ty_cons (0 int) ())) (Ty_cons (0 int) ()))
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
  [%expect
    {| ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ())))) |}];
  print_typed
    {|
     let rec f = fun x -> x
     and 
     g = fun x -> f 1
     |};
  [%expect
    {|
    ((Top_letrec
       ((f
          (x (Exp_var x (Ty_var (Link (Ty_cons (0 int) ()))))
            (Ty_arrow (Ty_var (Link (Ty_cons (0 int) ())))
              (Ty_var (Link (Ty_cons (0 int) ()))))))
         (g
           (x
             (Exp_app
               (Exp_var f
                 (Ty_arrow (Ty_var (Link (Ty_cons (0 int) ())))
                   (Ty_var (Link (Ty_cons (0 int) ())))))
               (Exp_const (Const_int 1) (Ty_cons (0 int) ()))
               (Ty_var (Link (Ty_cons (0 int) ()))))
             (Ty_arrow (Ty_var (Unbound '_t/4 1))
               (Ty_var (Link (Ty_cons (0 int) ())))))))))
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
      a |-> (Ty_def_adt a () ((Cons ((Ty_cons (0 int) ()))) (Nil ())))
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
    ((Top_type_def
       (Ty_def_adt int_l () ((Cons ((Ty_cons (0 int) ()))) (Nil ()))))
      (Top_let c (Exp_constr Nil 1 (Ty_cons (0 int_l) ())))
      (Top_let co
        (Exp_app
          (Exp_constr Cons 0
            (Ty_arrow (Ty_cons (0 int) ()) (Ty_cons (0 int_l) ())))
          (Exp_const (Const_int 1) (Ty_cons (0 int) ()))
          (Ty_var (Link (Ty_cons (0 int_l) ()))))))
    |}];
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
      co |-> forall  . (Ty_cons (0 int_l) ());
      c |-> forall  . (Ty_cons (0 int_l) ())
    Type Definitions:
      int_l |-> (Ty_def_adt int_l () ((Cons ((Ty_cons (0 int) ()))) (Nil ())))
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
    ((Top_type_def
       (Ty_def_adt int_l () ((Cons ((Ty_cons (0 int) ()))) (Nil ()))))
      (Top_let x (Exp_constr Nil 1 (Ty_cons (0 int_l) ())))
      (Top_let f
        (Exp_case (Exp_var x (Ty_cons (0 int_l) ()))
          (((Pat_constr Cons 0 ((Pat_var x (Ty_cons (0 int) ()))))
             (Exp_var x (Ty_cons (0 int) ())))
            ((Pat_constr Nil 1 ())
              (Exp_const (Const_int 0) (Ty_cons (0 int) ()))))
          (Ty_var (Link (Ty_cons (0 int) ()))))))
    |}];

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
    ((Top_type_def
       (Ty_def_adt int_l ('a/0 'b/0)
         ((Cons ((Ty_tuple ((Ty_qvar 'a/0) (Ty_qvar 'b/0))))) (Nil ()))))
      (Top_let x
        (Exp_constr Nil 1
          (Ty_cons (0 int_l)
            ((Ty_var (Unbound 'a/1 1)) (Ty_var (Unbound 'b/2 1))))))
      (Top_let f
        (Exp_case
          (Exp_var x
            (Ty_cons (0 int_l)
              ((Ty_var (Link (Ty_var (Unbound '_t/8 1))))
                (Ty_var (Link (Ty_var (Unbound '_t/9 1)))))))
          (((Pat_constr Cons 0
              ((Pat_tuple
                 ((Pat_var a (Ty_var (Unbound '_t/8 1)))
                   (Pat_var b (Ty_var (Unbound '_t/9 1)))))))
             (Exp_tuple
               ((Exp_var b (Ty_var (Unbound '_t/9 1)))
                 (Exp_var a (Ty_var (Unbound '_t/8 1))))
               (Ty_tuple ((Ty_var (Unbound '_t/9 1)) (Ty_var (Unbound '_t/8 1)))))))
          (Ty_var
            (Link
              (Ty_tuple ((Ty_var (Unbound '_t/9 1)) (Ty_var (Unbound '_t/8 1)))))))))
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
      f |-> forall '_t/8;'_t/9 . (Ty_tuple ((Ty_qvar '_t/9) (Ty_qvar '_t/8)));
      x |-> forall 'b/2;'a/1 . (Ty_cons (0 int_l) ((Ty_qvar 'a/1) (Ty_qvar 'b/2)))
    Type Definitions:
      int_l |-> (Ty_def_adt int_l ('a/0 'b/0)
      ((Cons ((Ty_tuple ((Ty_qvar 'a/0) (Ty_qvar 'b/0))))) (Nil ())))
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
  [%expect
    {| ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ())))) |}];
  print_typed {| module M = struct let x = 1 end |};
  [%expect
    {|
    ((Top_mod M
       (Mod_struct ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ()))))
         (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
           (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
           (owned_mods ())))))
    |}];
  print_typed
    {|
     module M = struct let x = 1 end
     let c = M.x
     |};
  [%expect
    {|
    ((Top_mod M
       (Mod_struct ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ()))))
         (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
           (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
           (owned_mods ()))))
      (Top_let c
        (Exp_field
          (Mod_name M
            (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          x (Ty_cons (0 int) ()))))
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
    ((Top_mod M
       (Mod_struct
         ((Top_type_def (Ty_def_adt t () ((Nil ()))))
           (Top_let x (Exp_constr Nil 0 (Ty_cons (1 t) ()))))
         (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
           (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
           (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
           (owned_mods ()))))
      (Top_let c
        (Exp_field
          (Mod_name M
            (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
              (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
              (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
              (mod_defs ()) (owned_mods ())))
          x (Ty_cons (1 t) ()))))
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
    ((Top_mod M
       (Mod_struct
         ((Top_type_def (Ty_def_adt t () ((Nil ()))))
           (Top_let x (Exp_constr Nil 0 (Ty_cons (1 t) ())))
           (Top_mod N
             (Mod_struct ((Top_type_def (Ty_def_adt t () ((Nil ())))))
               (Mod_ty_struct (id 2) (val_defs ())
                 (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
                 (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
                 (mod_defs ()) (owned_mods ()))))
           (Top_let z
             (Exp_field_constr
               (Mod_name N
                 (Mod_ty_struct (id 2) (val_defs ())
                   (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
                   (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
                   (mod_defs ()) (owned_mods ())))
               Nil 0 (Ty_cons (2 t) ()))))
         (Mod_ty_struct (id 1)
           (val_defs ((z (() (Ty_cons (2 t) ()))) (x (() (Ty_cons (1 t) ())))))
           (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
           (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
           (mod_defs
             ((N
                (Mod_ty_struct (id 2) (val_defs ())
                  (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
                  (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
                  (mod_defs ()) (owned_mods ())))))
           (owned_mods (2)))))
      (Top_let c
        (Exp_field
          (Mod_name M
            (Mod_ty_struct (id 1)
              (val_defs
                ((z (() (Ty_cons (2 t) ()))) (x (() (Ty_cons (1 t) ())))))
              (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
              (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
              (mod_defs
                ((N
                   (Mod_ty_struct (id 2) (val_defs ())
                     (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
                     (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
                     (mod_defs ()) (owned_mods ())))))
              (owned_mods (2))))
          x (Ty_cons (1 t) ())))
      (Top_let x
        (Exp_field_constr
          (Mod_field
            (Mod_name M
              (Mod_ty_struct (id 1)
                (val_defs
                  ((z (() (Ty_cons (2 t) ()))) (x (() (Ty_cons (1 t) ())))))
                (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
                (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
                (mod_defs
                  ((N
                     (Mod_ty_struct (id 2) (val_defs ())
                       (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
                       (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
                       (mod_defs ()) (owned_mods ())))))
                (owned_mods (2))))
            N
            (Mod_ty_struct (id 2) (val_defs ())
              (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
              (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
              (mod_defs ()) (owned_mods ())))
          Nil 0 (Ty_cons (2 t) ())))
      (Top_let y
        (Exp_field
          (Mod_name M
            (Mod_ty_struct (id 1)
              (val_defs
                ((z (() (Ty_cons (2 t) ()))) (x (() (Ty_cons (1 t) ())))))
              (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
              (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
              (mod_defs
                ((N
                   (Mod_ty_struct (id 2) (val_defs ())
                     (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
                     (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
                     (mod_defs ()) (owned_mods ())))))
              (owned_mods (2))))
          z (Ty_cons (2 t) ()))))
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
    ((Top_mod M
       (Mod_restrict
         (Mod_struct
           ((Top_type_def (Ty_def_adt t () ((Nil ()))))
             (Top_let x (Exp_constr Nil 0 (Ty_cons (1 t) ()))))
           (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
             (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
             (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
             (owned_mods ())))
         (Mod_ty_struct (id 2) (val_defs ((x (() (Ty_cons (2 t) ())))))
           (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
           (mod_defs ()) (owned_mods ()))
         (Mod_ty_struct (id 3) (val_defs ((x (() (Ty_cons (3 t) ())))))
           (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
           (mod_defs ()) (owned_mods ())))))
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
    ((Top_mod_sig MIntf
       (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
         (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
         (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
         (owned_mods ()))))
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
      MIntf |-> (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
      (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
      (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
      (owned_mods ()))
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
    ((Top_mod_sig MIntf
       (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
         (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
         (mod_defs ()) (owned_mods ()))))
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
    ((Top_mod_sig MIntf
       (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
         (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
         (mod_defs ()) (owned_mods ())))
      (Top_mod MImpl
        (Mod_restrict
          (Mod_struct
            ((Top_type_def (Ty_def_adt t () ((Nil ()))))
              (Top_let z (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
              (Top_let x (Exp_constr Nil 0 (Ty_cons (2 t) ()))))
            (Mod_ty_struct (id 2)
              (val_defs
                ((x (() (Ty_cons (2 t) ()))) (z (() (Ty_cons (0 int) ())))))
              (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
              (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
              (mod_defs ()) (owned_mods ())))
          (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (1 t) ())))))
            (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
            (mod_defs ()) (owned_mods ()))
          (Mod_ty_struct (id 3) (val_defs ((x (() (Ty_cons (3 t) ())))))
            (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
            (mod_defs ()) (owned_mods ())))))
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
    ((Top_mod_sig I
       (Mod_ty_struct (id 1)
         (val_defs ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
         (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
         (owned_mods ())))
      (Top_mod_sig J
        (Mod_ty_struct (id 2)
          (val_defs
            ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
              (x (() (Ty_cons (0 int) ())))))
          (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
          (owned_mods ())))
      (Top_mod MJ
        (Mod_struct
          ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
            (Top_let y (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
            (Top_let z (Exp_const (Const_int 1) (Ty_cons (0 int) ()))))
          (Mod_ty_struct (id 3)
            (val_defs
              ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
                (x (() (Ty_cons (0 int) ())))))
            (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
            (owned_mods ()))))
      (Top_mod Simple
        (Mod_struct
          ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
            (Top_let y (Exp_const (Const_int 2) (Ty_cons (0 int) ()))))
          (Mod_ty_struct (id 4)
            (val_defs
              ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
            (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
            (owned_mods ()))))
      (Top_mod M
        (Mod_functor
          (MI
            (Mod_ty_functor
              ((Mod_ty_struct (id 1)
                 (val_defs
                   ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
                 (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                 (owned_mods ()))
                (Mod_ty_struct (id 1)
                  (val_defs
                    ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
                  (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                  (owned_mods ())))))
          (Mod_struct
            ((Top_mod K
               (Mod_apply
                 (Mod_name MI
                   (Mod_ty_functor
                     ((Mod_ty_struct (id 1)
                        (val_defs
                          ((y (() (Ty_cons (0 int) ())))
                            (x (() (Ty_cons (0 int) ())))))
                        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                        (owned_mods ()))
                       (Mod_ty_struct (id 1)
                         (val_defs
                           ((y (() (Ty_cons (0 int) ())))
                             (x (() (Ty_cons (0 int) ())))))
                         (constr_defs ()) (ty_defs ()) (mod_sigs ())
                         (mod_defs ()) (owned_mods ())))))
                 (Mod_name Simple
                   (Mod_ty_struct (id 4)
                     (val_defs
                       ((y (() (Ty_cons (0 int) ())))
                         (x (() (Ty_cons (0 int) ())))))
                     (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                     (owned_mods ())))
                 (Mod_ty_struct (id 6)
                   (val_defs
                     ((y (() (Ty_cons (0 int) ())))
                       (x (() (Ty_cons (0 int) ())))))
                   (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                   (owned_mods ())))))
            (Mod_ty_struct (id 5) (val_defs ()) (constr_defs ()) (ty_defs ())
              (mod_sigs ())
              (mod_defs
                ((K
                   (Mod_ty_struct (id 6)
                     (val_defs
                       ((y (() (Ty_cons (0 int) ())))
                         (x (() (Ty_cons (0 int) ())))))
                     (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                     (owned_mods ())))))
              (owned_mods (6))))))
      (Top_mod F
        (Mod_functor
          (MI
            (Mod_ty_struct (id 1)
              (val_defs
                ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (Mod_restrict
            (Mod_struct
              ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
                (Top_let y (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
                (Top_let z (Exp_const (Const_int 1) (Ty_cons (0 int) ()))))
              (Mod_ty_struct (id 7)
                (val_defs
                  ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
                    (x (() (Ty_cons (0 int) ())))))
                (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                (owned_mods ())))
            (Mod_ty_struct (id 2)
              (val_defs
                ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
                  (x (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ()))
            (Mod_ty_struct (id 8)
              (val_defs
                ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
                  (x (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))))
      (Top_mod MMM
        (Mod_restrict
          (Mod_field
            (Mod_apply
              (Mod_name M
                (Mod_ty_functor
                  ((Mod_ty_functor
                     ((Mod_ty_struct (id 1)
                        (val_defs
                          ((y (() (Ty_cons (0 int) ())))
                            (x (() (Ty_cons (0 int) ())))))
                        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                        (owned_mods ()))
                       (Mod_ty_struct (id 1)
                         (val_defs
                           ((y (() (Ty_cons (0 int) ())))
                             (x (() (Ty_cons (0 int) ())))))
                         (constr_defs ()) (ty_defs ()) (mod_sigs ())
                         (mod_defs ()) (owned_mods ()))))
                    (Mod_ty_struct (id 5) (val_defs ()) (constr_defs ())
                      (ty_defs ()) (mod_sigs ())
                      (mod_defs
                        ((K
                           (Mod_ty_struct (id 6)
                             (val_defs
                               ((y (() (Ty_cons (0 int) ())))
                                 (x (() (Ty_cons (0 int) ())))))
                             (constr_defs ()) (ty_defs ()) (mod_sigs ())
                             (mod_defs ()) (owned_mods ())))))
                      (owned_mods (6))))))
              (Mod_name F
                (Mod_ty_functor
                  ((Mod_ty_struct (id 1)
                     (val_defs
                       ((y (() (Ty_cons (0 int) ())))
                         (x (() (Ty_cons (0 int) ())))))
                     (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                     (owned_mods ()))
                    (Mod_ty_struct (id 8)
                      (val_defs
                        ((z (() (Ty_cons (0 int) ())))
                          (y (() (Ty_cons (0 int) ())))
                          (x (() (Ty_cons (0 int) ())))))
                      (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                      (owned_mods ())))))
              (Mod_ty_struct (id 9) (val_defs ()) (constr_defs ()) (ty_defs ())
                (mod_sigs ())
                (mod_defs
                  ((K
                     (Mod_ty_struct (id 10)
                       (val_defs
                         ((y (() (Ty_cons (0 int) ())))
                           (x (() (Ty_cons (0 int) ())))))
                       (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                       (owned_mods ())))))
                (owned_mods (10))))
            K
            (Mod_ty_struct (id 10)
              (val_defs
                ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (Mod_ty_struct (id 1)
            (val_defs
              ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
            (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
            (owned_mods ()))
          (Mod_ty_struct (id 11)
            (val_defs
              ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
            (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
            (owned_mods ())))))
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
      MMM |-> (Mod_ty_struct (id 13)
      (val_defs ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
      (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()));
     F |-> (Mod_ty_functor
      ((Mod_ty_struct (id 1)
         (val_defs ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
         (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
         (owned_mods ()))
        (Mod_ty_struct (id 9)
          (val_defs
            ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
              (x (() (Ty_cons (0 int) ())))))
          (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
          (owned_mods ()))));
     M |-> (Mod_ty_functor
      ((Mod_ty_functor
         ((Mod_ty_struct (id 1)
            (val_defs
              ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
            (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
            (owned_mods ()))
           (Mod_ty_struct (id 1)
             (val_defs
               ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
             (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
             (owned_mods ()))))
        (Mod_ty_struct (id 5) (val_defs ()) (constr_defs ()) (ty_defs ())
          (mod_sigs ())
          (mod_defs
            ((K2
               (Mod_ty_struct (id 7)
                 (val_defs
                   ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
                 (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                 (owned_mods ())))
              (K
                (Mod_ty_struct (id 6)
                  (val_defs
                    ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
                  (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                  (owned_mods ())))))
          (owned_mods (7 6)))));
     Simple |-> (Mod_ty_struct (id 4)
      (val_defs ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
      (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()));
     MJ |-> (Mod_ty_struct (id 3)
      (val_defs
        ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
          (x (() (Ty_cons (0 int) ())))))
      (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))
    Module Types:
      J |-> (Mod_ty_struct (id 2)
      (val_defs
        ((z (() (Ty_cons (0 int) ()))) (y (() (Ty_cons (0 int) ())))
          (x (() (Ty_cons (0 int) ())))))
      (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()));
     I |-> (Mod_ty_struct (id 1)
      (val_defs ((y (() (Ty_cons (0 int) ()))) (x (() (Ty_cons (0 int) ())))))
      (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ()) (owned_mods ()))
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
    ((Top_external add
       (Ty_arrow (Ty_cons (0 int) ())
         (Ty_arrow (Ty_cons (0 int) ()) (Ty_cons (0 int) ())))
       ff_add)
      (Top_external print_int
        (Ty_arrow (Ty_cons (0 int) ()) (Ty_cons (0 int) ()))
        ff_builtin_print_int))
    |}];

  print_typed {|
     let x = 1
     let n = x = 1
|};
  [%expect
    {|
    ((Top_let x (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
      (Top_let n
        (Exp_cmp Eq (Exp_var x (Ty_cons (0 int) ()))
          (Exp_const (Const_int 1) (Ty_cons (0 int) ())) (Ty_cons (0 bool) ()))))
    |}];

  print_effect {|
               let rec id = fun x -> x
               |};
  [%expect
    {|
    ------------------Envirment Debug Info Begin------------------------

    ++++++++++++++++++Scope Debug Info Begin++++++++++++++++++
    Value Bindings:
      id |-> forall '_t/2 . (Ty_arrow (Ty_qvar '_t/2) (Ty_qvar '_t/2))
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
    ((Top_mod M
       (Mod_struct
         ((Top_mod_sig N
            (Mod_ty_struct (id 2) (val_defs ()) (constr_defs ()) (ty_defs ())
              (mod_sigs ()) (mod_defs ()) (owned_mods ()))))
         (Mod_ty_struct (id 1) (val_defs ()) (constr_defs ()) (ty_defs ())
           (mod_sigs
             ((N
                (Mod_ty_struct (id 2) (val_defs ()) (constr_defs ()) (ty_defs ())
                  (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
           (mod_defs ()) (owned_mods (2)))))
      (Top_mod F
        (Mod_functor
          (X
            (Mod_ty_struct (id 2) (val_defs ()) (constr_defs ()) (ty_defs ())
              (mod_sigs ()) (mod_defs ()) (owned_mods ())))
          (Mod_struct ()
            (Mod_ty_struct (id 3) (val_defs ()) (constr_defs ()) (ty_defs ())
              (mod_sigs ()) (mod_defs ()) (owned_mods ()))))))
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
    ((Top_mod F
       (Mod_restrict
         (Mod_restrict
           (Mod_struct ((Top_type_def (Ty_def_alias t (Ty_cons (0 int) ()))))
             (Mod_ty_struct (id 1) (val_defs ()) (constr_defs ())
               (ty_defs ((Ty_def_alias t (Ty_cons (0 int) ())))) (mod_sigs ())
               (mod_defs ()) (owned_mods ())))
           (Mod_ty_struct (id 2) (val_defs ()) (constr_defs ())
             (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ()) (mod_defs ())
             (owned_mods ()))
           (Mod_ty_struct (id 3) (val_defs ()) (constr_defs ())
             (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ()) (mod_defs ())
             (owned_mods ())))
         (Mod_ty_struct (id 4) (val_defs ()) (constr_defs ())
           (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ()) (mod_defs ())
           (owned_mods ()))
         (Mod_ty_struct (id 5) (val_defs ()) (constr_defs ())
           (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ()) (mod_defs ())
           (owned_mods ())))))
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
    ((Top_mod_sig I
       (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
         (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
         (owned_mods ())))
      (Top_mod F
        (Mod_restrict
          (Mod_struct
            ((Top_type_def (Ty_def_alias t (Ty_cons (0 int) ())))
              (Top_let y (Exp_const (Const_int 1) (Ty_cons (0 int) ()))))
            (Mod_ty_struct (id 2) (val_defs ((y (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ((Ty_def_alias t (Ty_cons (0 int) ()))))
              (mod_sigs ()) (mod_defs ()) (owned_mods ())))
          (Mod_ty_struct (id 3) (val_defs ((y (() (Ty_cons (3 t) ())))))
            (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
            (mod_defs ()) (owned_mods ()))
          (Mod_ty_struct (id 4) (val_defs ((y (() (Ty_cons (4 t) ())))))
            (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
            (mod_defs ()) (owned_mods ())))))
    ((Top_mod_sig I
       (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
         (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
         (owned_mods ())))
      (Top_mod F
        (Mod_functor
          (X
            (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (Mod_restrict
            (Mod_struct
              ((Top_type_def (Ty_def_alias t (Ty_cons (0 int) ())))
                (Top_let y
                  (Exp_field
                    (Mod_name X
                      (Mod_ty_struct (id 1)
                        (val_defs ((x (() (Ty_cons (0 int) ())))))
                        (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
                        (owned_mods ())))
                    x (Ty_cons (0 int) ()))))
              (Mod_ty_struct (id 2) (val_defs ((y (() (Ty_cons (0 int) ())))))
                (constr_defs ())
                (ty_defs ((Ty_def_alias t (Ty_cons (0 int) ())))) (mod_sigs ())
                (mod_defs ()) (owned_mods ())))
            (Mod_ty_struct (id 3) (val_defs ((y (() (Ty_cons (3 t) ())))))
              (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
              (mod_defs ()) (owned_mods ()))
            (Mod_ty_struct (id 4) (val_defs ((y (() (Ty_cons (4 t) ())))))
              (constr_defs ()) (ty_defs ((Ty_def_opaque t ()))) (mod_sigs ())
              (mod_defs ()) (owned_mods ()))))))
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
    ((Top_mod_sig I
       (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
         (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
         (owned_mods ())))
      (Top_mod F
        (Mod_functor
          (X
            (Mod_ty_struct (id 1) (val_defs ((x (() (Ty_cons (0 int) ())))))
              (constr_defs ()) (ty_defs ()) (mod_sigs ()) (mod_defs ())
              (owned_mods ())))
          (Mod_restrict
            (Mod_struct
              ((Top_type_def (Ty_def_alias t (Ty_cons (0 int) ())))
                (Top_type_def (Ty_def_alias n (Ty_cons (0 int) ())))
                (Top_let y (Exp_const (Const_int 3) (Ty_cons (0 int) ()))))
              (Mod_ty_struct (id 2) (val_defs ((y (() (Ty_cons (0 int) ())))))
                (constr_defs ())
                (ty_defs
                  ((Ty_def_alias n (Ty_cons (0 int) ()))
                    (Ty_def_alias t (Ty_cons (0 int) ()))))
                (mod_sigs ()) (mod_defs ()) (owned_mods ())))
            (Mod_ty_struct (id 3) (val_defs ((y (() (Ty_cons (3 t) ())))))
              (constr_defs ())
              (ty_defs ((Ty_def_opaque t ()) (Ty_def_opaque n ()))) (mod_sigs ())
              (mod_defs ()) (owned_mods ()))
            (Mod_ty_struct (id 4) (val_defs ((y (() (Ty_cons (4 t) ())))))
              (constr_defs ())
              (ty_defs ((Ty_def_opaque t ()) (Ty_def_opaque n ()))) (mod_sigs ())
              (mod_defs ()) (owned_mods ()))))))
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
    ((Top_let x
       (Exp_let y
         (Exp_tuple
           ((Exp_const (Const_int 1) (Ty_cons (0 int) ()))
             (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
           (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ()))))
         (Exp_let z
           (Exp_tuple
             ((Exp_const (Const_int 2) (Ty_cons (0 int) ()))
               (Exp_const (Const_int 1) (Ty_cons (0 int) ())))
             (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ()))))
           (Exp_tuple
             ((Exp_var y (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ()))))
               (Exp_var z (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ())))))
             (Ty_tuple
               ((Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ())))
                 (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ()))))))
           (Ty_tuple
             ((Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ())))
               (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ()))))))
         (Ty_tuple
           ((Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ())))
             (Ty_tuple ((Ty_cons (0 int) ()) (Ty_cons (0 int) ()))))))))
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
    ((Top_mod_sig M
       (Mod_ty_struct (id 1) (val_defs ()) (constr_defs ()) (ty_defs ())
         (mod_sigs ())
         (mod_defs
           ((N
              (Mod_ty_struct (id 2) (val_defs ()) (constr_defs ())
                (ty_defs ((Ty_def_opaque s ()) (Ty_def_opaque t ())))
                (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
         (owned_mods (2))))
      (Top_mod K
        (Mod_struct
          ((Top_mod N
             (Mod_struct
               ((Top_type_def (Ty_def_alias t (Ty_cons (0 int) ())))
                 (Top_type_def (Ty_def_alias s (Ty_cons (0 int) ()))))
               (Mod_ty_struct (id 4) (val_defs ()) (constr_defs ())
                 (ty_defs
                   ((Ty_def_alias s (Ty_cons (0 int) ()))
                     (Ty_def_alias t (Ty_cons (0 int) ()))))
                 (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
          (Mod_ty_struct (id 3) (val_defs ()) (constr_defs ()) (ty_defs ())
            (mod_sigs ())
            (mod_defs
              ((N
                 (Mod_ty_struct (id 4) (val_defs ()) (constr_defs ())
                   (ty_defs
                     ((Ty_def_alias s (Ty_cons (0 int) ()))
                       (Ty_def_alias t (Ty_cons (0 int) ()))))
                   (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
            (owned_mods (4)))))
      (Top_mod L1
        (Mod_restrict
          (Mod_name K
            (Mod_ty_struct (id 3) (val_defs ()) (constr_defs ()) (ty_defs ())
              (mod_sigs ())
              (mod_defs
                ((N
                   (Mod_ty_struct (id 4) (val_defs ()) (constr_defs ())
                     (ty_defs
                       ((Ty_def_alias s (Ty_cons (0 int) ()))
                         (Ty_def_alias t (Ty_cons (0 int) ()))))
                     (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
              (owned_mods (4))))
          (Mod_ty_struct (id 1) (val_defs ()) (constr_defs ()) (ty_defs ())
            (mod_sigs ())
            (mod_defs
              ((N
                 (Mod_ty_struct (id 2) (val_defs ()) (constr_defs ())
                   (ty_defs ((Ty_def_opaque s ()) (Ty_def_opaque t ())))
                   (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
            (owned_mods (2)))
          (Mod_ty_struct (id 5) (val_defs ()) (constr_defs ()) (ty_defs ())
            (mod_sigs ())
            (mod_defs
              ((N
                 (Mod_ty_struct (id 6) (val_defs ()) (constr_defs ())
                   (ty_defs ((Ty_def_opaque s ()) (Ty_def_opaque t ())))
                   (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
            (owned_mods (6)))))
      (Top_mod L2
        (Mod_restrict
          (Mod_name K
            (Mod_ty_struct (id 3) (val_defs ()) (constr_defs ()) (ty_defs ())
              (mod_sigs ())
              (mod_defs
                ((N
                   (Mod_ty_struct (id 4) (val_defs ()) (constr_defs ())
                     (ty_defs
                       ((Ty_def_alias s (Ty_cons (0 int) ()))
                         (Ty_def_alias t (Ty_cons (0 int) ()))))
                     (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
              (owned_mods (4))))
          (Mod_ty_struct (id 1) (val_defs ()) (constr_defs ()) (ty_defs ())
            (mod_sigs ())
            (mod_defs
              ((N
                 (Mod_ty_struct (id 2) (val_defs ()) (constr_defs ())
                   (ty_defs ((Ty_def_opaque s ()) (Ty_def_opaque t ())))
                   (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
            (owned_mods (2)))
          (Mod_ty_struct (id 7) (val_defs ()) (constr_defs ()) (ty_defs ())
            (mod_sigs ())
            (mod_defs
              ((N
                 (Mod_ty_struct (id 8) (val_defs ()) (constr_defs ())
                   (ty_defs ((Ty_def_opaque s ()) (Ty_def_opaque t ())))
                   (mod_sigs ()) (mod_defs ()) (owned_mods ())))))
            (owned_mods (8))))))
    |}];

  print_typed
    {|
               let result =
                 let id = fun x -> x in
                 (id 1, id "xx")
               |};
  [%expect
    {|
    ((Top_let result
       (Exp_let id
         (Exp_lam
           (x (Exp_var x (Ty_var (Unbound '_t/1 4)))
             (Ty_arrow (Ty_var (Unbound '_t/1 4)) (Ty_var (Unbound '_t/1 4)))))
         (Exp_tuple
           ((Exp_app
              (Exp_var id
                (Ty_arrow (Ty_var (Link (Ty_cons (0 int) ())))
                  (Ty_var (Link (Ty_cons (0 int) ())))))
              (Exp_const (Const_int 1) (Ty_cons (0 int) ()))
              (Ty_var (Link (Ty_cons (0 int) ()))))
             (Exp_app
               (Exp_var id
                 (Ty_arrow (Ty_var (Link (Ty_cons (0 string) ())))
                   (Ty_var (Link (Ty_cons (0 string) ())))))
               (Exp_const (Const_string "\"xx\"") (Ty_cons (0 string) ()))
               (Ty_var (Link (Ty_cons (0 string) ())))))
           (Ty_tuple
             ((Ty_var (Link (Ty_cons (0 int) ())))
               (Ty_var (Link (Ty_cons (0 string) ()))))))
         (Ty_tuple
           ((Ty_var (Link (Ty_cons (0 int) ())))
             (Ty_var (Link (Ty_cons (0 string) ()))))))))
    |}];

  print_typed {|
             let _ = 1

             let result = _
|};
  [%expect {| name `_` not found |}];
  print_typed
    {|
     module M = struct end
     module X = M
     module Bad = M(M)
     |};
  [%expect {| try apply a structure |}];
  print_typed
    {|
     module type M = sig end
     module F = functor (X:M) -> struct end
     let x = F.x
     |};
  [%expect {| try get field from functor |}];
  print_typed
    {|
     module M = functor(X:sig end) -> struct
     end
 
     module F = M.N
     |};
  [%expect {| try get field from functor |}];
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
    ((Top_mod_sig MT
       (Mod_ty_struct (id 1) (val_defs ())
         (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
         (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
         (owned_mods ())))
      (Top_mod M
        (Mod_restrict
          (Mod_struct ((Top_type_def (Ty_def_adt t () ((Nil ())))))
            (Mod_ty_struct (id 2) (val_defs ())
              (constr_defs ((Nil ((() (Ty_cons (2 t) ())) 0))))
              (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ())
              (mod_defs ()) (owned_mods ())))
          (Mod_ty_struct (id 1) (val_defs ())
            (constr_defs ((Nil ((() (Ty_cons (1 t) ())) 0))))
            (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
            (owned_mods ()))
          (Mod_ty_struct (id 3) (val_defs ())
            (constr_defs ((Nil ((() (Ty_cons (3 t) ())) 0))))
            (ty_defs ((Ty_def_adt t () ((Nil ()))))) (mod_sigs ()) (mod_defs ())
            (owned_mods ())))))
    |}]
