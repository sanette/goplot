exception Error of string

module Math = Mathparse
  
let parse s =
  let linebuf = Lexing.from_string (s^"\n") in
    try
      let result = Parser.main Lexer.token linebuf in
	Mathparse.typeur result
    with
      | Parser.Error ->
          raise (Error (Printf.sprintf "At offset %d: syntax error." (Lexing.lexeme_start linebuf)))
      |  e -> raise e
	   
(*  retourne la fonction et la formule (chaine) *)
let get_fun s var =
  let tresult = parse s in
  let funresult = Mathparse.ocamlfun tresult var in
    Mathparse.fun2float_fun funresult, Mathparse.tprinter tresult;;

let get_float s =
  let (f,_) = get_fun s "no variable" in
    f 0.;;

let get_string s =
  let tresult = parse s in
    Mathparse.tprinter tresult;;

let get_int s =
  let tresult = parse s in
  let funresult = Mathparse.ocamlfun tresult "no variable" in
    Mathparse.fun2int funresult;;

(* fonction , chaine *)
let get_fun2 s var1 var2 =
  let tresult = parse s in
  Mathparse.ocamlfun2 tresult var1 var2, Mathparse.tprinter tresult;;

