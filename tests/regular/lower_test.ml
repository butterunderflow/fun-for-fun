open Syntax.Parsing
module L = Lam.Tree
module C = Lam.Compile

[@@@warning "-26"]

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: full program lowering" =
  let print_lowered str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let typed, _env = Typing.Tools.type_check_program prog in
    typed |> C.compile_program |> L.sexp_of_expr |> print_sexp
  in

  print_lowered {| let x = 1 |};
  [%expect {| (Exp_mod_obj ((Field_simple x (Exp_const (Const_int 1))))) |}];

  print_lowered
    {| 
     module M =
       struct
         type () t = Nil


         let x = Nil

         let y = 1 
       end
     let c = M.x

     let d = M.y
                 |};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple M
         (Exp_mod_obj
           ((Field_simple x (Exp_constr 0))
             (Field_simple y (Exp_const (Const_int 1))))))
        (Field_simple c (Exp_field (Exp_var M) x))
        (Field_simple d (Exp_field (Exp_var M) y))))
    |}];

  print_lowered
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

     let rec f = fun x -> x
     and 
     g = fun x -> f 1

     |};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple MJ
         (Exp_mod_obj
           ((Field_simple x (Exp_const (Const_int 1)))
             (Field_simple y (Exp_const (Const_int 1)))
             (Field_simple z (Exp_const (Const_int 1))))))
        (Field_simple Simple
          (Exp_mod_obj
            ((Field_simple x (Exp_const (Const_int 1)))
              (Field_simple y (Exp_const (Const_int 2))))))
        (Field_simple M
          (Exp_lam
            ((MI)
              (Exp_mod_obj
                ((Field_simple K (Exp_app (Exp_var MI) ((Exp_var Simple))))))
              (Simple))))
        (Field_simple F
          (Exp_lam
            ((MI)
              (Exp_mod_obj
                ((Field_simple x (Exp_const (Const_int 1)))
                  (Field_simple y (Exp_const (Const_int 1)))
                  (Field_simple z (Exp_const (Const_int 1)))))
              ())))
        (Field_simple MMM (Exp_field (Exp_app (Exp_var M) ((Exp_var F))) K))
        (Field_letrec
          ((f ((x) (Exp_var x) ()))
            (g ((x) (Exp_app (Exp_var f) ((Exp_const (Const_int 1)))) (f)))))))
    |}];

  print_lowered
    {|

     let rec f = fun x -> x
     and 
     g = fun x -> f 1

|};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_letrec
         ((f ((x) (Exp_var x) ()))
           (g ((x) (Exp_app (Exp_var f) ((Exp_const (Const_int 1)))) (f)))))))
    |}];
  print_lowered
    {|
     type () int_l
     = Cons of int
     | Nil

     let x = Nil

     let z = Cons 1

     let f = fun p -> 
         match x with
        | Cons y -> y
        | Nil    -> 0

     |};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple x (Exp_constr 1))
        (Field_simple z
          (Exp_app (Exp_payload_constr 0) ((Exp_const (Const_int 1)))))
        (Field_simple f
          (Exp_lam
            ((p)
              (Exp_switch (Exp_var x)
                (((Pat_constr 0 ((Pat_var y))) (Exp_var y))
                  ((Pat_constr 1 ()) (Exp_const (Const_int 0)))))
              (x))))))
    |}];

  print_lowered
    {|
     module M = struct
       let x = 1
     end

     module type I = sig
        val x : int
     end

     module F = functor (X: I) -> struct
       let y = M.x
     end
     |};

  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple M
         (Exp_mod_obj ((Field_simple x (Exp_const (Const_int 1))))))
        (Field_simple F
          (Exp_lam
            ((X) (Exp_mod_obj ((Field_simple y (Exp_field (Exp_var M) x)))) (M))))))
    |}];
  print_lowered
    {|
     module M = struct
       let x = 1
     end
     module type I = sig
        val x : int
     end
     module F1 =
       functor (X: I) -> 
       functor (Y: I) ->
       struct
         let y = M.x
      end
     |};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple M
         (Exp_mod_obj ((Field_simple x (Exp_const (Const_int 1))))))
        (Field_simple F1
          (Exp_lam
            ((X)
              (Exp_lam
                ((Y) (Exp_mod_obj ((Field_simple y (Exp_field (Exp_var M) x))))
                  (M)))
              (M))))))
    |}];

  print_lowered
    {|
     module M = struct
       let x = 1
     end
     module type I = sig
        val x : int
     end

     module F2 =
     functor (X: I) -> 
     struct
       module N = struct
         let x = 2
       end

       module F3 = functor (Y: I) -> 
                   struct
                     let y = N.x
                   end
     end
     |};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple M
         (Exp_mod_obj ((Field_simple x (Exp_const (Const_int 1))))))
        (Field_simple F2
          (Exp_lam
            ((X)
              (Exp_mod_obj
                ((Field_simple N
                   (Exp_mod_obj ((Field_simple x (Exp_const (Const_int 2))))))
                  (Field_simple F3
                    (Exp_lam
                      ((Y)
                        (Exp_mod_obj
                          ((Field_simple y (Exp_field (Exp_var N) x))))
                        (N))))))
              ())))))
    |}];

  print_lowered {|
external add : int -> int -> int = "ff_add"
|};
  [%expect {| (Exp_mod_obj ((Field_simple add (Exp_external ff_add)))) |}];

  print_lowered {|
let x = 1

let y = 2

let z = x = y
|};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple x (Exp_const (Const_int 1)))
        (Field_simple y (Exp_const (Const_int 2)))
        (Field_simple z (Exp_cmp Eq (Exp_var x) (Exp_var y)))))
    |}];
  print_lowered
    {|
     let even =
      let rec even = fun x -> odd 1
      and odd = fun x -> even 1
     in
     even
     |};
  [%expect
    {|
    (Exp_mod_obj
      ((Field_simple even
         (Exp_letrec
           ((even
              ((x) (Exp_app (Exp_var odd) ((Exp_const (Const_int 1)))) (odd)))
             (odd
               ((x) (Exp_app (Exp_var even) ((Exp_const (Const_int 1)))) (even))))
           (Exp_var even)))))
    |}]
