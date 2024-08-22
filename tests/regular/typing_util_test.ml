open Syntax.Parsing
open Typing
module U = Util
module P = Syntax.Parsetree

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: type def reorg" =
  let print_ty_defs_reorgnized str =
    Ident.refresh ();
    let prog = parse_string_program str in
    let[@warning "-8"] (Some defs) =
      List.find_map
        (fun top ->
          match top with
          | P.TopTypeDef defs -> Some defs
          | _ -> None)
        prog
    in
    defs |> U.reorg_ty_defs |> P.sexp_of_ty_def_group |> print_sexp
  in
  print_ty_defs_reorgnized {|
type z = y

and y = x

and x = int
     |};
  [%expect
    {|
    ((TDAlias x (TCons int ())) (TDAlias y (TCons x ()))
      (TDAlias z (TCons y ())))
    |}];

  print_ty_defs_reorgnized
    {|
type z = y

and y = x

and x = int

and a = x

and b = a 
and c = b
     |};
  [%expect
    {|
    ((TDAlias x (TCons int ())) (TDAlias a (TCons x ())) (TDAlias b (TCons a ()))
      (TDAlias c (TCons b ())) (TDAlias y (TCons x ())) (TDAlias z (TCons y ())))
    |}]
