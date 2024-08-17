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
    (main/1 () () (Exp_mod_obj ((Field_simple x (Exp_const (Const_int 1))))))
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
      (Exp_mod_obj
        ((Field_simple M
           (Exp_mod_obj
             ((Field_simple x (Exp_constr 0))
               (Field_simple y (Exp_const (Const_int 1))))))
          (Field_simple c (Exp_field (Exp_var M) x))
          (Field_simple d (Exp_field (Exp_var M) y)))))
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
    (main/1 () () (Exp_mod_obj ((Field_letrec (() ((f f/2) (g g/3)))))))
    (f/2 (f g) (x) (Exp_var x))
    (g/3 (f g) (x) (Exp_app (Exp_var f) ((Exp_const (Const_int 1)))))
    |}];

  print_lifted {|
external add : int -> int -> int = "ff_add"
|};
  [%expect
    {|
    Main function name:
    main/1
    Global C functions:
    (main/1 () () (Exp_mod_obj ((Field_simple add (Exp_external ff_add)))))
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
      (Exp_mod_obj
        ((Field_simple x (Exp_const (Const_int 1)))
          (Field_simple y (Exp_const (Const_int 2)))
          (Field_simple z (Exp_closure ((x y) z/2))))))
    (z/2 (x y) (z) (Exp_cmp Eq (Exp_var x) (Exp_var y)))
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
    (main/1 () () (Exp_mod_obj ((Field_letrec (() ((sum sum/2)))))))
    (sum/2 (sum) (x)
      (Exp_if (Exp_cmp Eq (Exp_var x) (Exp_const (Const_int 0)))
        (Exp_const (Const_int 1)) (Exp_const (Const_int 2))))
    |}]
