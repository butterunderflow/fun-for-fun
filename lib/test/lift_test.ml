open Syntax.Parsing
module C = Clos.Closure
module L = Clos.Lift
module C1 = Lam.Compile

[@@@warning "-26"]

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: full program lowering" =
  let print_lifted str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let typed, _env = Typing.Check.tc_program prog (Typing.Env.init ()) in
    typed
    |> C1.compile_program
    |> L.lift
    |> fun (e, fns) ->
    Printf.printf "Lifted main expression: \n";
    print_sexp (C.sexp_of_expr e);
    Printf.printf "\nGlobal C functions: \n";
    List.iter (fun fn -> print_sexp (C.sexp_of_func fn)) fns
  in

  print_lifted {| let x = 1 |};
  [%expect
    {|
    Lifted main expression:
    (EModObject ((FSimple x (EConst (CInt 1)))))

    Global C functions: |}];

  print_lifted
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
    Lifted main expression:
    (EModObject
      ((FSimple M
         (EModObject ((FSimple x (ECons 0)) (FSimple y (EConst (CInt 1))))))
        (FSimple c (EField (EVar M) x)) (FSimple d (EField (EVar M) y))))

    Global C functions: |}];

  print_lifted
    {|

     let rec f = fun x -> x
     and 
     g = fun x -> f 1

     |};
  [%expect
    {|
    Lifted main expression:
    (EModObject ((FLetRec ((f g) ((f f/1) (g g/2))))))

    Global C functions:
    (f/1 (f g) (x) (EVar x))
    (g/2 (f g) (x) (EApp (EVar f) ((EConst (CInt 1)))))
    |}];

  print_lifted {|
external add : int -> int -> int = "ff_add"
|};
  [%expect
    {|
    Lifted main expression:
    (EModObject ((FSimple add (EExt ff_add))))

    Global C functions:
    |}];
  print_lifted {|
let x = 1

let y = 2

let z = fun z -> x = y
|};
  [%expect {|
    Lifted main expression:
    (EModObject
      ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 2)))
        (FSimple z (EClosure ((x y) z/1)))))

    Global C functions:
    (z/1 (x y) (z) (ECmp Eq (EVar x) (EVar y)))
    |}];

print_lifted {|
               let rec sum = fun x ->
               if x = 0
               then 1
               else 2
|};
  [%expect {|
    Lifted main expression:
    (EModObject ((FLetRec ((sum) ((sum sum/1))))))

    Global C functions:
    (sum/1 (sum) (x)
      (EIf (ECmp Eq (EVar x) (EConst (CInt 0))) (EConst (CInt 1))
        (EConst (CInt 2))))
    |}]

