(* this file is part of gOplot *)
(* menhir file *)

%{
    
 open Mathparse

%}

%token <int> INT
%token <float> FLOAT
%token PLUS MINUS TIMES DIV
%token POWER
%token LPAREN RPAREN

%token <string> ID
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

(* = pas vraiment nécessaire de vérifier les précédences, ocaml le fera. On peut
donc regrouper tous les multi-op *)


%left POWER /* puissances entières uniquement, mais résultat flottant */

%start <Mathparse.expr> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { Int i }
| f = FLOAT
    { Float f }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { Minop (Plus, e1,e2)}
| e1 = expr DIV e2 = expr
    { Finop (Div, e1,e2)}
| e1 = expr MINUS e2 = expr
    { Minop (Minus, e1,e2)}
| e1 = expr TIMES e2 = expr
    { Minop (Times, e1,e2)}
| e1 = expr POWER e2 = expr
    { Finop (Power, e1,e2)}
| f = ID LPAREN e = expr RPAREN
    { Fun (f, e) }
| x = ID
    { Var x }
| MINUS e = expr %prec UMINUS
    { Uminus e }

