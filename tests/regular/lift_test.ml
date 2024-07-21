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
    let typed, _env = Typing.Tools.type_check_program prog in
    typed
    |> C1.compile_program
    |> L.lift
    |> fun (main, fns) ->
    Printf.printf "Main function name: \n";
    Printf.printf "%s" (Ident.to_string main);
    Printf.printf "\nGlobal C functions: \n";
    List.iter (fun fn -> print_sexp (C.sexp_of_func fn)) fns
  in

  print_lifted {| let x = 1 |};
  [%expect
    {|
    Main function name:
    main/1
    Global C functions:
    (main/1 () () (EModObject ((FSimple x (EConst (CInt 1))))))
    |}];

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
    Main function name:
    main/1
    Global C functions:
    (main/1 () ()
      (EModObject
        ((FSimple M
           (EModObject ((FSimple x (ECons 0)) (FSimple y (EConst (CInt 1))))))
          (FSimple c (EField (EVar M) x)) (FSimple d (EField (EVar M) y)))))
    |}];

  print_lifted
    {|

     let rec f = fun x -> x
     and 
     g = fun x -> f 1

     |};
  [%expect
    {|
    Main function name:
    main/1
    Global C functions:
    (main/1 () () (EModObject ((FLetRec (() ((f f/2) (g g/3)))))))
    (f/2 (f g) (x) (EVar x))
    (g/3 (f g) (x) (EApp (EVar f) ((EConst (CInt 1)))))
    |}];

  print_lifted {|
external add : int -> int -> int = "ff_add"
|};
  [%expect
    {|
    Main function name:
    main/1
    Global C functions:
    (main/1 () () (EModObject ((FSimple add (EExt ff_add)))))
    |}];
  print_lifted {|
let x = 1

let y = 2

let z = fun z -> x = y
|};
  [%expect
    {|
    Main function name:
    main/1
    Global C functions:
    (main/1 () ()
      (EModObject
        ((FSimple x (EConst (CInt 1))) (FSimple y (EConst (CInt 2)))
          (FSimple z (EClosure ((x y) z/2))))))
    (z/2 (x y) (z) (ECmp Eq (EVar x) (EVar y)))
    |}];

  print_lifted
    {|
               let rec sum = fun x ->
               if x = 0
               then 1
               else 2
|};
  [%expect
    {|
    Main function name:
    main/1
    Global C functions:
    (main/1 () () (EModObject ((FLetRec (() ((sum sum/2)))))))
    (sum/2 (sum) (x)
      (EIf (ECmp Eq (EVar x) (EConst (CInt 0))) (EConst (CInt 1))
        (EConst (CInt 2))))
    |}]
