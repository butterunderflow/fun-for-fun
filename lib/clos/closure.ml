open Sexplib.Conv
module T = Syntax.Parsetree
module L = Lam.Tree

type constant = T.constant [@@deriving sexp]

type expr =
  | ETuple of expr list
  | EModObject of object_field list
  | EStruct of (string * expr) list
  | EVar of string
  | ECons of int
  | EConst of constant
  | EApp of expr * expr
  | ESwitch of expr * branch list
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | EClosure of closure
  | ELetRec of closure_rec * expr
  | EField of expr * string

and pattern = L.pattern

and object_field =
  | FSimple of string * expr
  | FLetRec of closure_rec

and closure = string list * Ident.ident

and closure_rec = string list * (string * Ident.ident) list

and branch = pattern * expr

and func =
  Ident.ident (* function name *)
  * string list (* captures *)
  * string (* parameter *)
  * expr
[@@deriving sexp]
