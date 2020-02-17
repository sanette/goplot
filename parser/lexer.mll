(* this file is part of gOplot *)

{

open Parser

exception Error of string
    
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule line = parse
| ([^'\n']* '\n') as line
    { line }
| eof
    { exit 0 }

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| digit+ '.' digit* as f
    { FLOAT (float_of_string f) }
| digit+ as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '^'
    { POWER }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| id as word
    { ID word }
| eof
    { exit 0 }

| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

