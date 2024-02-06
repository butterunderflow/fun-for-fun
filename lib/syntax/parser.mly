%{
    open Parsetree
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
%token ARROW
%token <string> IDENT
%token <string> MIDENT
%token <string> TYPEVAR
%token EQ
%token OR
%token AND
%token LBRACKET
%token RBRACKET
%token COMMA
%token DOT
%token OF
%token COLON
%token LPAREN
%token RPAREN


%type <Parsetree.program> program
%type <Parsetree.top_level list> top_levels
%type <Parsetree.constant> constant
%type <Parsetree.variant> variant

%type <Parsetree.path> path_dbg
%type <Parsetree.type_expr> type_expr_dbg
%type <Parsetree.mod_expr> mod_expr_dbg

/* Start symbols */
%start program path_dbg type_expr_dbg mod_expr_dbg
%%


program:
    | tops=top_levels EOF { tops } ;

top_levels:
    | (* empty *) { [] }
    | td=type_def rest=top_levels
        { TopTypeDef td :: rest }
    | LET p=pattern EQ e=expr rest=top_levels  { TopLet (p, e) :: rest }
    | LET REC funcs=separated_list(AND, function_bind) rest=top_levels
        { TopLetRec funcs :: rest } 
    | MODULE m_name=MIDENT 
        EQ m_body=mod_expr rest=top_levels
        { TopMod (m_name, m_body) :: rest }
    | MODULE REC functors=separated_list(AND, functor_bind) rest=top_levels { TopModRec functors :: rest } ;

mod_expr : 
    | m_name=MIDENT { MEName m_name }
    | STRUCT m_body=top_levels END { MEStruct m_body }

mod_para : 
    | m_name=MIDENT COLON m_type=mod_type { (m_name, m_type) }

functor_bind: 
    | m_name=MIDENT LPAREN m_paras=separated_list(COMMA, mod_para) RPAREN ARROW m_body=mod_expr 
       { (m_name, (m_paras, m_body)) }


type_def: 
    | TYPE LPAREN tvs=separated_list(COMMA, TYPEVAR) RPAREN n=IDENT 
        EQ vs=separated_list(OR, variant) END { TDAdt (n, tvs, vs) }
    | TYPE n=IDENT EQ te=type_expr { TDAlias(n, te) } ;

pattern: 
    | n=IDENT { PVar n }

parameter:
    | n=IDENT { PBare n }
    | LPAREN n=IDENT COLON t=type_expr RPAREN { PAnn (n, t) }

function_bind:
    | name=IDENT paras=list(parameter) EQ b=expr
       { (name, paras, b) }

variant: 
    | c=MIDENT OF payload=type_expr  { (c, Some payload) } 
    | c=MIDENT                       { (c, None) };

type_expr: 
    | LPAREN t_args = separated_list(COMMA, type_expr) RPAREN n=IDENT 
        { TCons(n, t_args) }
    | t_arg = type_expr n=IDENT { TCons(n, [t_arg]) }
    | n=IDENT { TCons (n, []) }
    | tv=TYPEVAR { TVar tv } ;

expr:
    | c = constant { EConst c }
    | v = IDENT { EVar v }
    | func=expr arg=expr { EApp (func, arg) };


path:
    | n = MIDENT { PName n } 
    | p = path DOT n = MIDENT { PMem (p, n) } 
    | p1 = path LPAREN p2 = path RPAREN { PApply (p1, p2) } ;
  

mod_type: 
    | SIG END { MTSig [] }

constant: 
    | i = INT { CInt i }
    | b = BOOL { CBool b }
    | s = STRING { CString s } ;

(* debug rules: which are normal rules append with an eof *)
path_dbg: 
    | p=path EOF { p }

type_expr_dbg: 
    | te=type_expr EOF { te }

mod_expr_dbg:
    | me=mod_expr EOF { me }
