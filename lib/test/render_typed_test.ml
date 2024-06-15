open Syntax.Parsing
open Typing

module DefaultPP = Render.MakePP (Render.ShowAllConfig)

let%expect_test "Test: pretty print typed expression" =
  let print_typed str =
    Ident.refresh ();
    let e = parse_string_expr str in
    let typed = Check.tc_expr e (Env.init ()) in
    let fmt = Format.std_formatter in
    DefaultPP.pp_expr fmt typed
  in
  print_typed "1";
  [%expect {| 1 is () 0.int |}];
  print_typed {|let x = 1 in x|};
  [%expect {|
    let x = 1 is () 0.int
    in x is () 0.int |}];

  print_typed {|fun x -> let y = x in y |};
  [%expect {|
    fun x ->
      let y = x is {'_t/1}
      in y is {'_t/2} |}];

  print_typed {| if true then 1 else 2 |};
  [%expect {|
    if
      true is () 0.bool
    then
      1 is () 0.int
    else
      2 is () 0.int |}];

  print_typed {| (1, 2, 3,4) |};
  [%expect {|
    (1 is () 0.int, 2 is () 0.int, 3 is () 0.int, 4 is () 0.int) |}];

  print_typed
    {|
               let rec  f = fun x -> x
               and y = fun x -> f 1
               in
               f 1|};
  [%expect
    {|
    let rec f = fun x ->
                  x is {() 0.int}
    and y = fun x ->
              f is ({() 0.int}
                     ->{() 0.int}) 1 is () 0.int
    in f is (() 0.int
              ->() 0.int) 1 is () 0.int |}]

let%expect_test "Test: pretty print typed program" =
  let print_typed str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let typed, _env = Check.tc_program prog (Env.init ()) in
    let fmt = Format.std_formatter in
    DefaultPP.pp_prog fmt typed
  in

  print_typed
    {|
     type () int_l 
     = Cons of int
     | Nil 
           let co = (Cons 1)

     let c = Nil

     let f =
         match c with
         | Cons x -> x
         | Nil    -> 0
|};
  [%expect {|
    type () int_l =
    | Cons of () 0.int
    | Nil

    let co = Cons 1 is () 0.int

    let c = Nil

    let f = match c is () 0.int_l with
            | (Cons x is () 0.int) -> x is () 0.int
            | Nil -> 0 is () 0.int |}]
