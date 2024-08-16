[@@@warning "-33"]

open Typing.Unify
open Typing.Types_in

let print_sexp s =
  Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum ?indent:(Some 2) s)

let%expect_test "Test: unify typing" =
  let print_type ty = ty |> sexp_of_ty |> print_sexp in

  let type_of x = TVarI { contents = Unbound (Ident.mk_ident 0 x, 0) } in
  let ret_ty = type_of "'ret" in
  let tv = type_of "'t" in
  unify tv int_ty;
  print_type tv;
  [%expect {| (TVarI (Link (TConsI (0 int) ()))) |}];
  unify (TArrowI (tv, tv)) (TArrowI (int_ty, ret_ty));
  print_type tv;

  [%expect {| (TVarI (Link (TConsI (0 int) ()))) |}];
  let t1 = TVarI { contents = Unbound (Ident.mk_ident 0 "'t", 1) } in
  let t2 = TVarI { contents = Unbound (Ident.mk_ident 1 "'t", 33) } in
  print_type t1;
  print_type t2;
  [%expect
    {|
    (TVarI (Unbound 't/0 1))
    (TVarI (Unbound 't/1 33))
    |}];
  unify t1 t2;
  print_type t1;
  print_type t2;
  [%expect
    {|
    (TVarI (Link (TVarI (Unbound 't/1 1))))
    (TVarI (Unbound 't/1 1))
    |}]
