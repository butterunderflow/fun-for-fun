%{
open Parsetree
let mk_type_ref fon t_args =
  match fon with
  | Some p, n -> TField(p, n, t_args)
  | None, n ->  TCons(n, t_args)

%}

/* Constants */
%token <int> INT
%token <bool> BOOL
%token <string> STRING

%token TYPE
%token EOF
%token LET
%token REC
%token END
%token MODULE
%token SIG
%token STRUCT
%token VAL
%token FUNCTOR
%token FUN
%token ARROW
%token MATCH
%token WITH
%token <string> IDENT
%token <string> MIDENT
%token <string> TYPEVAR
%token EQ
%token STAR
%token OR
%token IN
%token AND
%token LBRACKET
%token RBRACKET
%token COMMA
%token DOT
%token OF
%token COLON
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMI

%nonassoc below_COMMA
%left     COMMA                        /* (e , e , e) */
%left     STAR                         /* (e * e * e) */

%nonassoc below_APP
%nonassoc LPAREN


%type <Parsetree.program> program
%type <Parsetree.top_level list> top_levels
%type <Parsetree.constant> constant
%type <Parsetree.evariant> variant

%type <Parsetree.ety> type_expr_dbg
%type <Parsetree.mod_expr> mod_expr_dbg
%type <Parsetree.emod_ty> mod_type_dbg
%type <Parsetree.expr> expr_dbg
%type <Parsetree.pattern> pattern_dbg

/* Start symbols */
%start program type_expr_dbg mod_expr_dbg mod_type_dbg expr_dbg pattern_dbg
%%


program:
    | tops=top_levels EOF { tops } ;

top_levels:
    | (* empty *) { [] }
    | td=type_def rest=top_levels
        { TopTypeDef td :: rest }
    | LET x=IDENT EQ e=expr rest=top_levels  { TopLet (x, e) :: rest }
    | LET REC funcs=separated_list(AND, function_bind) rest=top_levels
        { TopLetRec funcs :: rest }
    | MODULE m_name=MIDENT
        EQ m_body=mod_expr rest=top_levels
        { TopMod (m_name, m_body) :: rest }
    | MODULE TYPE m_name=MIDENT EQ mt=mod_type rest=top_levels
        { TopModSig (m_name, mt) :: rest }
;

mod_expr:
    | LPAREN me=mod_expr RPAREN { me }
    | m_name=MIDENT { MEName m_name }
    | STRUCT m_body=top_levels END { MEStruct m_body }
    | FUNCTOR LPAREN mp=mod_para RPAREN ARROW f_body=mod_expr { MEFunctor (mp, f_body) }
    | m = mod_expr DOT n = MIDENT { MEField (m, n) }
    | m1 = mod_expr LPAREN m2 = mod_expr RPAREN { MEApply (m1, m2) } 
    | m1 = mod_expr COLON mt1 = mod_type { MERestrict (m1, mt1) }
;

mod_para :
    | m_name=MIDENT COLON m_type=mod_type { (m_name, m_type) }

functor_bind:
    | m_name=MIDENT LPAREN mp=mod_para RPAREN ARROW m_body=mod_expr
       { (m_name, (mp, m_body)) }

type_def:
    | TYPE LPAREN tvs=separated_list(COMMA, TYPEVAR) RPAREN n=IDENT
        EQ vs=separated_list(OR, variant) END
                { TDAdt (n, (List.map Ident.from tvs), vs) }

pattern:
    | n=IDENT { PVar n } (* variable pattern *)
    | c=constant { PVal c }
    | c=MIDENT pat=pattern? { PCons (c, pat) }
    | m=mod_expr DOT n=MIDENT pat=pattern? { PFieldCons (m, n, pat) }
    | LPAREN pats=separated_nontrivial_llist(COMMA, pattern) RPAREN
         { PTuple (pats) }
    ;

parameter:
    | n=IDENT { PBare n }
    | LPAREN n=IDENT COLON t=type_expr RPAREN { PAnn (n, t) }

function_bind:
    | name=IDENT EQ FUN para=parameter ARROW b=expr
       { (name, (para, b)) }

variant:
    | c=MIDENT OF payload=type_expr  { (c, Some payload) }
    | c=MIDENT                       { (c, None) };

field_def:
    | n=IDENT COLON t=type_expr  { (n, t) }

field_or_name:
    | p=mod_expr DOT n=IDENT  { (Some p, n) }
    | n=IDENT             { (None, n) }

type_expr:
    | LPAREN t_args = separated_list(COMMA, type_expr) RPAREN fon=field_or_name
        { mk_type_ref fon t_args }
    | LPAREN te=type_expr RPAREN { te }
    | ts=separated_nontrivial_llist(STAR, type_expr) { TTuple ts }
    | t_arg = type_expr fon=field_or_name { mk_type_ref fon [t_arg] }
    | n=IDENT { TCons (n, []) }
    | tv=TYPEVAR { TVar (Ident.from tv) }
    | arg=type_expr ARROW ret=type_expr { TArrow (arg, ret) }
    | LBRACE fields=separated_nontrivial_llist(SEMI, field_def) RBRACE { TRecord fields }

path:
    | m_name=MIDENT { MEName m_name }
    | m = path DOT n = MIDENT { MEField (m, n) }

expr:
    | c=constant { EConst c }
    | func=expr arg=expr { EApp (func, arg) } %prec below_APP
    | LPAREN e=expr RPAREN { e }
    | c=MIDENT { ECons c }
    | p=path DOT v=IDENT { EField (p, v) }
    | p=path DOT v=MIDENT { EFieldCons (p, v) }
    | v=IDENT { EVar v }
    | LET x=IDENT EQ e1=expr IN e2=expr { ELet (x, e1, e2) }
    | LET REC binds=separated_nonempty_list(AND, function_bind) IN body=expr { ELetrec (binds, body) }
    | tu=tuple_expr { tu }
    | FUN para=parameter ARROW body=expr { ELam (para, body) }
    | MATCH e=expr WITH OR? branches=separated_nonempty_list(OR, branch)
           { ECase (e, branches) }
    ;

branch: p=pattern ARROW e=expr { ( p, e ) }

tuple_expr:
    | es = separated_nontrivial_llist(COMMA, expr) %prec below_COMMA
        { ETuple es }

mod_type:
    | m=MIDENT                      { MTName m }
    | p=mod_expr DOT m=MIDENT       { MTField (p, m) }
    | SIG comps=list(sig_comp) END  { MTSig comps }
    | FUNCTOR
        LPAREN p=MIDENT COLON p_ty=mod_type RPAREN
        ARROW body=mod_type         { MTFunctor (p, p_ty, body) }


sig_comp:
    | VAL v=IDENT COLON ty=type_expr { TValueSpec (v, ty) }
    | TYPE LPAREN tvs=separated_list(COMMA, TYPEVAR) RPAREN t=IDENT
        { TAbstTySpec (t, (List.map Ident.from tvs)) }
    | def=type_def                   { TManiTySpec def }
    | MODULE m_name=MIDENT EQ mt=mod_type { TModSpec (m_name, mt) }
;

constant:
    | i = INT { CInt i }
    | b = BOOL { CBool b }
    | s = STRING { CString s } ;

(* debug rules: which are normal rules append with an eof *)
type_expr_dbg:
    | te=type_expr EOF { te }

mod_expr_dbg:
    | me=mod_expr EOF { me }

pattern_dbg:
    | p=pattern EOF { p }

mod_type_dbg:
    | me=mod_type EOF { me }

expr_dbg:
    | e=expr EOF      { e }



(* [reversed_separated_nontrivial_llist(separator, X)] recognizes a list of at
   least two [X]s, separated with [separator]s, and produces an OCaml list in
   reverse order -- that is, the last element in the input text appears first
   in this list. Its definition is left-recursive. *)

reversed_separated_nontrivial_llist(separator, X):
  xs = reversed_separated_nontrivial_llist(separator, X)
  separator
  x = X
    { x :: xs }
| x1 = X
  separator
  x2 = X
    { [ x2; x1 ] }

(* [separated_nontrivial_llist(separator, X)] recognizes a list of at least
   two [X]s, separated with [separator]s, and produces an OCaml list in direct
   order -- that is, the first element in the input text appears first in this
   list. *)
%inline separated_nontrivial_llist(separator, X):
  xs = rev(reversed_separated_nontrivial_llist(separator, X))
    { xs }
