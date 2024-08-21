open Sexplib.Conv
module T = Syntax.Parsetree

type constant = T.constant [@@deriving sexp]

type expr =
  | ETuple of expr list
  | EModObject of object_field list
  | EStruct of (string * expr) list
  | EVar of string
  | EExt of string
  | ECons of int
  | EConsWith of int
  | EConst of constant
  | EApp of expr * expr list
  | ESwitch of expr * branch list
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | ELam of lambda
  | ELetRec of (string * lambda) list * expr
  | EField of expr * string
  | ECmp of T.cmp_op * expr * expr
  | ESeq of expr * expr

and pattern =
  | PVar of string
  | PVal of constant
  | PCons of int * pattern option
  | PTuple of pattern list

and object_field =
  | FSimple of string * expr
  | FLetRec of (string * lambda) list

and lambda =
  string list (* lambda parameters *)
  * expr
  * string list ref (* free variables, filled in free varialble analysis *)

and branch = pattern * expr [@@deriving sexp]

let dbg e =
  let s = sexp_of_expr e in
  Sexplib.Sexp.to_string_hum ?indent:(Some 2) s
