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
%type <Parsetree.path> path_dbg
%type <Parsetree.top_level list> top_levels
%type <Parsetree.constant> constant
%type <Parsetree.variant> variant


/* Start symbols */
%start program path_dbg
%%


program:
    | tops=top_levels EOF { tops } ;

top_levels:
    | (* empty *) { [] }
    | TYPE n=IDENT LBRACKET tvs=separated_list(COMMA, TYPEVAR) RBRACKET
        EQ vs=separated_list(OR, variant) 
      END 
        rest=top_levels
        { Top_type (n, tvs, vs) :: rest }
    | LET p=pattern EQ e=expr rest=top_levels  { Top_let (p, e) :: rest }
    | LET REC funcs=separated_list(AND, function_bind) 
        rest=top_levels
        { Top_letrec funcs :: rest }

pattern: 
    | n=IDENT { PVar n }

parameter:
    | n=IDENT { PBare n }
    | LPAREN n=IDENT COLON t=type_expr RPAREN { PAnn (n, t) }

function_bind:
    | name=IDENT paras=list(parameter) EQ b=expr
       { (name, paras, b) }

variant: 
    | c=IDENT OF LPAREN arg_types=separated_list(COMMA, type_expr) RPAREN  { (c, arg_types) } ;

type_expr: 
    | n=IDENT LBRACKET t_args=separated_list(COMMA, type_expr) RBRACKET 
        { TCons(n, t_args) }
    | n=IDENT { TCons (n, []) }
    | tv=TYPEVAR { TVar tv }

expr:
    | c = constant { EConst c }
    | v = IDENT { EVar v }
    | func=expr arg=expr { EApp (func, arg) };


path:
    | n = MIDENT { PName n } 
    | p = path DOT n = MIDENT { PMem (p, n) } 
    | p1 = path LPAREN p2 = path RPAREN { PApply (p1, p2) } ;
  

constant: 
    | i = INT { CInt i }
    | b = BOOL { CBool b }
    | s = STRING { CString s } ;

(* debug rules: which are normal rulesi append with an eof *)
path_dbg: 
    | p=path EOF { p }


