open Sexplib.Conv
module T = Syntax.Parsetree

type constant = T.constant [@@deriving sexp]

type expr =
  | ETuple of expr list
  | EModObject of object_field list
  | EStruct of (string * expr) list
  | EVar of string
  | ECons of int
  | EConsWith of int
  | EConst of constant
  | EApp of expr * expr
  | ESwitch of expr * branch list
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | ELam of lambda
  | ELetRec of (string * lambda) list * expr
  | EField of expr * string

and pattern =
  | PVar of string
  | PVal of constant
  | PCons of int * pattern option
  | PTuple of pattern list

and object_field =
  | FSimple of string * expr
  | FLetRec of (string * lambda) list

and lambda = string * expr * string list ref

and branch = pattern * expr [@@deriving sexp]
