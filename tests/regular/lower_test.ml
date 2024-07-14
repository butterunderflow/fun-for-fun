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
    let typed, _env = Typing.Check.tc_program prog (Typing.Env.init ()) in
    typed |> C.compile_program |> L.sexp_of_expr |> print_sexp
  in

  print_lowered {| let x = 1 |};
  [%expect {| (EModObject ((FSimple x (EConst (CInt 1))))) |}];

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
    (EModObject
      ((FSimple M
         (EModObject ((FSimple x (ECons 0)) (FSimple y (EConst (CInt 1))))))
        (FSimple c (EField (EVar M) x)) (FSimple d (EField (EVar M) y)))) |}];

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
    (EModObject
      ((FSimple MJ
         (EModObject
           ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 1)))
             (FSimple z (EConst (CInt 1))))))
        (FSimple Simple
          (EModObject
            ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 2))))))
        (FSimple M
          (ELam
            ((MI) (EModObject ((FSimple K (EApp (EVar MI) ((EVar Simple))))))
              (Simple))))
        (FSimple F
          (ELam
            ((MI)
              (EModObject
                ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 1)))
                  (FSimple z (EConst (CInt 1)))))
              ())))
        (FSimple MMM (EField (EApp (EVar M) ((EVar F))) K))
        (FLetRec
          ((f ((x) (EVar x) ()))
            (g ((x) (EApp (EVar f) ((EConst (CInt 1)))) (f)))))))
    |}];

  print_lowered
    {|

     let rec f = fun x -> x
     and 
     g = fun x -> f 1

|};
  [%expect
    {|
    (EModObject
      ((FLetRec
         ((f ((x) (EVar x) ()))
           (g ((x) (EApp (EVar f) ((EConst (CInt 1)))) (f)))))))
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
    (EModObject
      ((FSimple x (ECons 1)) (FSimple z (EApp (EConsWith 0) ((EConst (CInt 1)))))
        (FSimple f
          (ELam
            ((p)
              (ESwitch (EVar x)
                (((PCons 0 ((PVar y))) (EVar y))
                  ((PCons 1 ()) (EConst (CInt 0)))))
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
    (EModObject
      ((FSimple M (EModObject ((FSimple x (EConst (CInt 1))))))
        (FSimple F
          (ELam ((X) (EModObject ((FSimple y (EField (EVar M) x)))) (M))))))
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
    (EModObject
      ((FSimple M (EModObject ((FSimple x (EConst (CInt 1))))))
        (FSimple F1
          (ELam
            ((X) (ELam ((Y) (EModObject ((FSimple y (EField (EVar M) x)))) (M)))
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
    (EModObject
      ((FSimple M (EModObject ((FSimple x (EConst (CInt 1))))))
        (FSimple F2
          (ELam
            ((X)
              (EModObject
                ((FSimple N (EModObject ((FSimple x (EConst (CInt 2))))))
                  (FSimple F3
                    (ELam
                      ((Y) (EModObject ((FSimple y (EField (EVar N) x)))) (N))))))
              ())))))
    |}];

print_lowered {|
external add : int -> int -> int = "ff_add"
|};
  [%expect {| (EModObject ((FSimple add (EExt ff_add)))) |}];

print_lowered {|
let x = 1

let y = 2

let z = x = y
|};
  [%expect {|
    (EModObject
      ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 2)))
        (FSimple z (ECmp Eq (EVar x) (EVar y)))))
    |}]
