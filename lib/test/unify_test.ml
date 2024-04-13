open Syntax.Parsing
open Syntax.Parsetree
open Typing.Unify
open Typing.Types

[@@@warning "-33"]

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: unify typing" =
  let print_type ty =
    ty |> Syntax.Parsetree.sexp_of_type_expr |> print_sexp
  in

  let type_of = parse_string_type_expr in
  let ret_ty = type_of "'ret" in
  let tv = type_of "'t" in
  unify tv int_ty <$> tv |> print_type;
  [%expect {| (TCons int ()) |}];

  unify (TArrow (tv, tv)) (TArrow (int_ty, ret_ty)) <$> ret_ty |> print_type;
  [%expect {| (TCons int ()) |}]
