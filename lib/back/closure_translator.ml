open Clos.Closure

(* module C = Cabs *)
module F = Format

let trans_expr buf (_e : expr) =
  Printf.bprintf buf
{|
#include<stdio.h>
#include"fun_rt.h"

int main()
{
  test_rt();
  printf("Hello World");
}
|}

let translate (e, _fns) =
  let buf = Buffer.create 50 in
  trans_expr buf e;
  Buffer.contents buf
