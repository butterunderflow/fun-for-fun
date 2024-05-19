[@@@warning "-33"]

open Typing.Unify
open Typing.Types_in

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: unify typing" =
  let print_type ty = ty |> sexp_of_ty |> print_sexp in

  let type_of x = TVarI (Ident.mk_ident 0 x) in
  let ret_ty = type_of "'ret" in
  let tv = type_of "'t" in
  unify tv int_ty <$> tv |> print_type;
  [%expect {| (TConsI (0 int) ()) |}];

  unify (TArrowI (tv, tv)) (TArrowI (int_ty, ret_ty))
  <$> ret_ty |> print_type;
  [%expect {| (TConsI (0 int) ()) |}]
