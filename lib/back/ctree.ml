(* most of this file are copied from cabs -- abstract syntax for FrontC *)

type statement =
  | NOP  (** No operation. Useful for empty else-part in condition. *)
  | COMPUTATION of expression
      (** A simple expression, usually an assignment. *)
  | BLOCK of body  (** A block between braces *)
  | SEQUENCE of statement * statement  (** Two statement separated by ";" *)
  | IF of expression * statement * statement
      (** "if" statement with or without else-part. *)
  | WHILE of expression * statement  (** "while" statement. *)
  | DOWHILE of expression * statement  (** "do ... while" statement *)
  | FOR of expression * expression * expression * statement
      (** "for" statement. *)
  | BREAK  (** "break" statement. *)
  | CONTINUE  (** "continue" statement. *)
  | RETURN of expression
      (** "return" statement with an expression or with NOTHING. *)
  | SWITCH of expression * statement
      (** "switch" statement. Cases are put in the sub-statement as labels. *)
  | CASE of expression * statement  (** "case" statement as a label. *)
  | DEFAULT of statement  (** "default" statement as a label. *)
  | LABEL of string * statement
      (** "label" statement whose sub-statement follows colon ":". *)
  | GOTO of string  (** "goto" statement. *)

and expression =
  | NOTHING
      (** Null-expression. Useful for return with no value or table
      declaration without size. *)
  | UNARY of unary_operator * expression  (** Unary operator use. *)
  | BINARY of binary_operator * expression * expression
      (** Binary operator use. *)
  | QUESTION of expression * expression * expression
      (** "condition ? then-expression : else-expression" operator. *)
  | CAST of base_type * expression  (** "(type)expresson" type casting. *)
  | CALL of expression * expression list  (** Function call. *)
  | COMMA of expression list
      (** Sequence of expression separated with ",". *)
  | CONSTANT of constant  (** Constant value. *)
  | VARIABLE of string  (** Access to an identifier. *)
  | EXPR_SIZEOF of expression  (** "sizeof" with expression. *)
  | TYPE_SIZEOF of base_type  (** "sizeof" with type. *)
  | INDEX of expression * expression  (** Access to an array item; *)
  | MEMBEROF of expression * string  (** Indirection through ".". *)
  | MEMBEROFPTR of expression * string
      (** Pointer indirection through "->". *)
  | GNU_BODY of body  (** GNU braces inside an expression. *)
  | DESIGNATED of string * expression
      (** Designated initialization, in compound constants only. *)
  | EXPR_LINE of expression * string * int
      (** Record the file and line of the expression. *)

and body = definition list * statement

and proto = base_type * name list
(** Prototype of a function with return type, function declaration and
 * variable argument "..." boolean. *)

and definition =
  | FUNDEF of name * body  (** Definition of a function. *)
  | DECDEF of decl_name
      (** Declaration of function or definition of a variable. *)
  | TYPEDEF of decl_name  (** Definition of a typedef. *)
  | ONLYTYPEDEF of decl_name
      (** Definition of lonely "struct", "union" or "enum". *)

and name = string * base_type

(** Base type *)
and base_type =
  | NO_TYPE  (** Old K&R declaration without type *)
  | VOID  (** "void" type *)
  | BOOL  (** C99 boolean (_Bool) type  *)
  | CHAR of sign  (** "char" type with sign modifier *)
  | INT of size * sign  (** "int" type with size and sign modifiers *)
  | FLOAT of bool  (** "float" type with long (true) modifier *)
  | DOUBLE of bool  (** "double" type with long (true) modifier *)
  | COMPLEX_FLOAT  (** float complex *)
  | COMPLEX_DOUBLE  (** double complex *)
  | COMPLEX_LONG_DOUBLE  (** long double complex *)
  | PTR of base_type  (** Pointer "*" to the given type *)
  | RESTRICT_PTR of base_type
      (** REstricted pointer "*" to the given type. *)
  | ARRAY of base_type * expression
      (** Array of the given type with the given expression size (may be NOTHING) *)
  | STRUCT of string * decl_name list
      (** "struct" of the given name (may be empty) with given fields (may also be empty) *)
  | UNION of string * decl_name list
      (** "union" of the given name (may be empty) with given fields (may also be empty) *)
  | PROTO of proto  (** Prototype of a function *)
  | NAMED_TYPE of string  (** Named type coming from typedef *)
  | ENUM of string * enum_item list
      (** "union" of the given name (may be empty) with given values (may also be empty) *)
  | CONST of base_type  (** "const" modifier *)
  | VOLATILE of base_type  (** "volatile" modifier *)
  | BUILTIN_TYPE of string
      (** a machine-specific or complier-specific builtin type  *)
  | TYPE_LINE of string * int * base_type
      (** Not a type, just to record the file/line of an identifier. *)

and decl_name = storage * name
(** A name group, that is, a simple type following by many name
 * declaration as [int v, *p, t\[256\];]. *)

(** Signess of int and bitfields *)
and sign =
  | NO_SIGN  (** No sign modifier *)
  | SIGNED  (** "signed" modifier *)
  | UNSIGNED  (** "unsigned" modifier *)

(** Storage of names *)
and storage =
  | NO_STORAGE  (** No storage modifier *)
  | AUTO  (** "auto" modifier *)
  | STATIC  (** "static" modifier *)
  | EXTERN  (** "extern" modifier *)
  | REGISTER  (** "register" modifier *)

(** Unary operators identifiers. *)
and unary_operator =
  | MINUS  (** "-" operator. *)
  | PLUS  (** "+" operator. *)
  | NOT  (** "!" operator. *)
  | BNOT  (** "~" operator. *)
  | MEMOF  (** "*" operator. *)
  | ADDROF  (** "&" operator. *)
  | PREINCR  (** "++" pre-incrementation. *)
  | PREDECR  (** "--" pre-decrementation. *)
  | POSINCR  (** "++" post-incrementation. *)
  | POSDECR  (** "--" post-decrementation. *)

(** Size of int *)
and size =
  | NO_SIZE  (** No size modifier *)
  | SHORT  (** "short" modifier *)
  | LONG  (** "long" modifier *)
  | LONG_LONG  (** GNU "long long" modifier *)

(* Binary operators identifiers. *)
and binary_operator =
  | ADD  (** "+" operator. *)
  | SUB  (** "-" operator. *)
  | MUL  (** "*" operator. *)
  | DIV  (** "/" operator. *)
  | MOD  (** "%" operator. *)
  | AND  (** "&&" operator. *)
  | OR  (** "||" operator. *)
  | BAND  (** "&" operator. *)
  | BOR  (** "|" operator. *)
  | XOR  (** "^" operator. *)
  | SHL  (** "<<" operator. *)
  | SHR  (** ">>" operator. *)
  | EQ  (** "==" operator. *)
  | NE  (** "!=" operator. *)
  | LT  (** "<" operator. *)
  | GT  (** ">" operator. *)
  | LE  (** "<=" operator. *)
  | GE  (** ">=" operator. *)
  | ASSIGN  (** "=" operator. *)
  | ADD_ASSIGN  (** "+=" operator. *)
  | SUB_ASSIGN  (** "-=" operator. *)
  | MUL_ASSIGN  (** "*=" operator. *)
  | DIV_ASSIGN  (** "/=" operator. *)
  | MOD_ASSIGN  (** "%=" operator. *)
  | BAND_ASSIGN  (** "&=" operator. *)
  | BOR_ASSIGN  (** "|=" operator. *)
  | XOR_ASSIGN  (** "^=" operator. *)
  | SHL_ASSIGN  (** "<<=" operator. *)
  | SHR_ASSIGN  (** ">>=" operator. *)

(** Constant values. *)
and constant =
  | CONST_INT of string  (** Integer constant. *)
  | CONST_FLOAT of string  (** Float constant. *)
  | CONST_CHAR of string  (** Character constant with escapes resolved. *)
  | CONST_STRING of string  (** String constant with escapes resolved. *)
  | CONST_COMPOUND of expression list
      (** Compound values between braces. Only valid for variable
      initialization. *)

and enum_item = string * expression
