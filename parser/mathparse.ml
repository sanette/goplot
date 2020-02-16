(* faire aussi les complexes *)

exception Undefined_function of string

type mult_inop = Plus | Minus | Times

(*refaire power...*)
type float_inop = Div | Power

let create_hashtable size init =
  let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

let int2int_table = 
  create_hashtable 8 [
(*      ("Gamma", "fact"); *)
      ("round", ""); (* pas bon *)
      ("floor", ""); (* idem  *)
      ("abs", "abs")
    ]

let float2int_table = 
  create_hashtable 8 [
      ("floor", "floor");
      ("round", "round");
    ]

let float2float_table = 
  create_hashtable 16 [
      ("sin", "sin");
      ("cos", "cos");
      ("tan", "tan");
      ("arcsin", "asin");
      ("arccos", "acos");
      ("arctan", "atan");
      ("sinh", "sinh");
      ("cosh", "cosh");
      ("tanh", "tanh");
      ("exp", "exp");
      ("sqrt", "sqrt");
      ("ln", "log");
      ("abs", "abs_float");
      ("frac", "Mathutils.frac");
      ("arcsinh", "Gsl.Math.asinh");
      ("arccosh", "Gsl.Math.acosh");
      ("arctanh", "Gsl.Math.atanh");
      ("Gamma", "Mathutils.gamma");
      ("sinc", "Mathutils.sinc");
      ("AiryAi", "Mathutils.airyai");
      ("AiryBi", "Mathutils.airybi");
      ("erf", "Gsl.Sf.erf")
    ]

let int2float_table = 
  create_hashtable 8 [
      ("factorial", "Gsl.Sf.fact")
    ]

let floatconst_table = 
  create_hashtable 16 [
      ("e", Gsl.Math.e);
      ("Pi", Gsl.Math.pi);
      ("Pi_2", Gsl.Math.pi_2);
      ("Pi_4", Gsl.Math.pi_4)
    ]



type expr =
  | Int of int
  | Float of float
  | Minop of mult_inop * expr * expr
  | Finop of float_inop  * expr * expr
  | Fun of string * expr
  | Var of string
  | Uminus of expr

(* afficheur bête non typé. Juste pour débugger *)
let rec printer e =
  match e with
    | Int i -> string_of_int i
    | Float f -> string_of_float f
    | Minop (o,e1,e2) -> let opname = 
	(match o with
	   | Plus -> "+"
	   | Minus  -> "-" 
	   | Times -> "*") in
	Printf.sprintf "%s (%s) %s" (printer e1) opname (printer e2)
    | Finop (Div,e1,e2) -> Printf.sprintf "%s (/) %s" 
	(printer e1) (printer e2)
    | Finop (Power,e1,e2) -> Printf.sprintf "%s (^) %s" 
	(printer e1) (printer e2)
    | Fun (f,e) -> Printf.sprintf "%s(%s)" f (printer e)
    | Var x -> x
    | Uminus e  -> Printf.sprintf "-%s" (printer e)

type intexpr =
  | IInt of int
  | IMinop of mult_inop * intexpr * intexpr
  | IIFun of string * intexpr
  | IFFun of string * floatexpr
  | IUminus of intexpr

and floatexpr =
  | FIntConversion of intexpr
  | FFloat of float
  | FMinop of mult_inop * floatexpr * floatexpr
  | FFinop of float_inop * floatexpr * floatexpr
  | FFFun of string * floatexpr
  | FIFun of string * intexpr
  | FVar of string
  | FUminus of floatexpr

type expr_typee =
  | I of intexpr
  | F of floatexpr

(* transforme un expression  en expression typée *)
let rec typeur e =
  match e with
    | Int i -> I (IInt i)
    | Float f -> F (FFloat f)
    | Minop (o, e1, e2) -> begin
	match (typeur e1, typeur e2) with
	  | (I ie1, I ie2) -> (I (IMinop (o, ie1, ie2)))
	  | (F fe1, F fe2) -> (F (FMinop (o, fe1, fe2)))
	  | (I ie1, F fe2) -> (F (FMinop (o, FIntConversion ie1, fe2)))
	  | (F fe1, I ie2) -> (F (FMinop (o, fe1, FIntConversion ie2)))
      end
    | Finop (o, e1, e2) -> begin 
	match (typeur e1, typeur e2) with 
	  | (I ie1, I ie2) -> (F (FFinop (o, FIntConversion ie1, FIntConversion ie2)))
	  | (F fe1, F fe2) -> (F (FFinop (o, fe1, fe2)))
	  | (I ie1, F fe2) -> (F (FFinop (o, FIntConversion ie1, fe2)))
	  | (F fe1, I ie2) -> (F (FFinop (o, fe1, FIntConversion ie2)))
      end
    | Fun (f,e) -> begin
	match (typeur e) with
	  | I ie -> begin
	      try
		let token = Hashtbl.find int2int_table f in
		  I (IIFun (token, ie))
	      with Not_found -> try
		let token = Hashtbl.find int2float_table f in
		  F (FIFun (token, ie))
	      with Not_found -> try 
		let token = Hashtbl.find float2float_table f in
		  F (FFFun (token, (FIntConversion ie)))
	      with Not_found -> try 
		let token = Hashtbl.find float2int_table f in
		  I (IFFun (token, (FIntConversion ie)))
	      with Not_found -> raise (Undefined_function f)
	    end
	  | F fe -> begin
	      try
		let token = Hashtbl.find float2float_table f in
		  F (FFFun (token, fe))
	      with Not_found -> try
		let token = Hashtbl.find float2int_table f in
		  I (IFFun (token, fe))
	      with Not_found -> raise (Undefined_function f)
	    end
      end
    | Var x ->  begin 
	try 
	  let token = Hashtbl.find floatconst_table x in
	    F (FFloat token)
	with Not_found -> F (FVar x)
      end
    | Uminus e -> begin
	match (typeur e) with
	  | (I ie) -> I (IUminus ie)
	  | (F fe) -> F (FUminus fe)
      end

(* afficheur *)
let tprinter etypee =
  let rec loop et profondeur =
    let p = profondeur + 1 in
    let binprint e1 op e2 =
      let s = Printf.sprintf "%s %s %s"
	(loop e1 p) op (loop e2 p) in
	if profondeur = 0 then s else Printf.sprintf "(%s)" s 
    in
      match et with
	| I ie -> begin
	    match ie with
	      | IInt i -> string_of_int i
	      | IMinop (o, ie1, ie2) ->  let opname = 
		  (match o with
		     | Plus -> "+"
		     | Minus  -> "-" 
		     | Times -> "*") in
		  binprint (I ie1) opname (I ie2)
	      | IIFun (f, ie1) -> Printf.sprintf "%s(%s)" f (loop (I ie1) p)
	      | IFFun (f, fe1) -> Printf.sprintf "%s(%s)" f (loop (F fe1) p)
	      | IUminus ie1 -> Printf.sprintf "-%s" (loop (I ie1) p)
	  end
	| F fe -> begin
	    match fe with
	      | FIntConversion ie -> Printf.sprintf "float %s" (loop (I ie) p)
	      | FFloat f -> string_of_float f
	      | FMinop (o, fe1, fe2) ->  let opname = 
		  (match o with
		     | Plus -> "+."
		     | Minus  -> "-." 
		     | Times -> "*.") in
		  binprint (F fe1) opname (F fe2)
	      | FFinop (Div, fe1, fe2) -> binprint (F fe1) "/." (F fe2)
	      | FFinop (Power, fe1, fe2) -> binprint (F fe1) "**" (F fe2)
	      | FFFun (f,fe1) -> Printf.sprintf "%s(%s)" f (loop (F fe1) p)
	      | FIFun (f,ie1) -> Printf.sprintf "%s(%s)" f (loop (I ie1) p)
	      | FVar x -> x
	      | FUminus fe1 -> Printf.sprintf "-.%s"(loop (F fe1) p)
	  end in
    loop etypee 0
      

(* meme chose mais cette fois retourne une vraie fonction ocaml *)
type ocamlfunction =
  | FFocaml of (float -> float)
  | IIocaml of (int -> int)
  | IFocaml of (int -> float)
  | FIocaml of (float -> int)
      (* compliqué ! utiliser un type paire à la place ex (float,float) ??  ou des
	 fonctions Number -> Number avec Number=Float|Int *)

exception Type_mismatch (* should never happen in the parsing funcs *)
exception Unknown_variable of string

let fconst c = FFocaml (fun _ -> c)

(* à compléter *)
let string2name_table =
  create_hashtable 64 [
    ("sin", FFocaml sin);
    ("cos", FFocaml cos);
    ("tan", FFocaml tan);
    ("asin", FFocaml asin);
    ("acos", FFocaml acos);
    ("atan", FFocaml atan);
    ("sinh", FFocaml sinh);
    ("cosh", FFocaml cosh);
    ("tanh", FFocaml tanh);
    ("exp", FFocaml exp);
    ("log", FFocaml log);
    ("sqrt", FFocaml sqrt);
    ("abs_float", FFocaml abs_float);
    ("abs", IIocaml abs);
    ("floor", FIocaml (fun x -> int_of_float (floor x)));
    ("Mathutils.frac", FFocaml Mathutils.frac);
    ("Gsl.Math.pi", fconst Gsl.Math.pi);
    ("Gsl.Math.asinh", FFocaml Gsl.Math.asinh);
    ("Gsl.Math.acosh", FFocaml Gsl.Math.acosh);
    ("Gsl.Math.atanh", FFocaml Gsl.Math.atanh);
    ("Mathutils.gamma", FFocaml Mathutils.gamma);
    ("Mathutils.sinc", FFocaml Mathutils.sinc);
    ("Mathutils.airyai", FFocaml Mathutils.airyai);
    ("Mathutils.airybi", FFocaml Mathutils.airybi);
    ("Gsl.Sf.erf", FFocaml Gsl.Sf.erf);
    ("Gsl.Sf.fact", IFocaml Gsl.Sf.fact)
  ]
    
let ocamlname s = 
  try
    Hashtbl.find string2name_table s
  with Not_found -> raise Not_found;; (* à modifier ... *)

let binop o f1 f2 = 
 match (f1, f2) with
   | (FFocaml ff1, FFocaml ff2) -> 
       FFocaml (fun x -> 
		match o with
		  | Plus -> (ff1 x) +. (ff2 x)
		  | Minus -> (ff1 x) -. (ff2 x)
		  | Times -> (ff1 x) *. (ff2 x))
   | (IIocaml if1, IIocaml if2) -> 
       IIocaml (fun x -> 
		match o with
		  | Plus -> (if1 x) + (if2 x)
		  | Minus -> (if1 x) - (if2 x)
		  | Times -> (if1 x) * (if2 x))
   | (IFocaml ff1, IFocaml ff2) -> 
       IFocaml (fun x -> 
		match o with
		  | Plus -> (ff1 x) +. (ff2 x)
		  | Minus -> (ff1 x) -. (ff2 x)
		  | Times -> (ff1 x) *. (ff2 x))
   | (FIocaml ff1, FIocaml ff2) -> 
       FIocaml (fun x -> 
		match o with
		  | Plus -> (ff1 x) + (ff2 x)
		  | Minus -> (ff1 x) - (ff2 x)
		  | Times -> (ff1 x) * (ff2 x))
   | _ -> raise Type_mismatch
       
let div f1 f2 = 
 match (f1, f2) with
   | (FFocaml ff1, FFocaml ff2) -> FFocaml (fun x -> (ff1 x) /. (ff2 x))
   | (IFocaml ff1, IFocaml ff2) -> IFocaml (fun x -> (ff1 x) /. (ff2 x))
   | _ -> raise Type_mismatch

let power f1 f2 = 
 match (f1, f2) with
   | (FFocaml ff1, FFocaml ff2) -> FFocaml (fun x -> (ff1 x) ** (ff2 x))
   | (IFocaml ff1, IFocaml ff2) -> IFocaml (fun x -> (ff1 x) ** (ff2 x))
   | _ -> raise Type_mismatch
       
let compose f1 f2 =
    match (f1, f2) with
    | (IIocaml ii1, IIocaml ii2) -> IIocaml (fun x -> ii1 (ii2 x))
    | (FFocaml ff1, IFocaml if2) -> IFocaml (fun x -> ff1 (if2 x))
    | (IFocaml if1, IIocaml ii2) -> IFocaml (fun x -> if1 (ii2 x))
    | (FIocaml fi1, IFocaml if2) -> IIocaml (fun x -> fi1 (if2 x))
    | (FFocaml ff1, FFocaml ff2) -> FFocaml (fun x -> ff1 (ff2 x))
    | (IIocaml ii1, FIocaml fi2) -> FIocaml (fun x -> ii1 (fi2 x))
    | (FIocaml fi1, FFocaml ff2) -> FIocaml (fun x -> fi1 (ff2 x))
    | (IFocaml if1, FIocaml fi2) -> FFocaml (fun x -> if1 (fi2 x))
    | _ -> raise Type_mismatch

let minus f =
match f with
  | FFocaml f -> FFocaml (fun x -> -. (f x))
  | IFocaml f -> IFocaml (fun x -> -. (f x))
  | IIocaml f -> IIocaml (fun x -> - (f x))
  | FIocaml f -> FIocaml (fun x -> - (f x))


(* function of one variable *)
let ocamlfun expr_t var =
let rec loop et =
  match et with
    | I ie-> begin
	match ie with
	  | IInt i -> FIocaml (fun _ -> i)
	  | IMinop (o, ie1, ie2) ->  
	      let f1 = loop (I ie1) and f2 = loop (I ie2) in
		binop o f1 f2
	  | IIFun (f, ie1) -> let fname = ocamlname f
			      and f1 = loop (I ie1) in
	      compose fname f1
	  | IFFun (f, fe1) -> let fname = ocamlname f
			      and f1 = loop (F fe1) in
	      compose fname f1
	  | IUminus ie1 -> let f1 = loop (I ie1) in
	      minus f1
      end
    | F fe -> begin
	match fe with
	  | FIntConversion ie -> let f = loop (I ie) in
	      (match f with
		| FIocaml fi -> FFocaml (fun x -> float (fi x))
		| IIocaml ii -> IFocaml (fun x -> float (ii x))
		| _ -> raise Type_mismatch)
	  | FFloat f -> FFocaml (fun _ -> f)
	  | FMinop (o, fe1, fe2) ->  
	      let f1 = loop (F fe1) and f2 = loop (F fe2) in
		binop o f1 f2
	  | FFinop (Div, fe1, fe2) ->
	      let f1 = loop (F fe1) and f2 = loop (F fe2) in
		div f1 f2
	  | FFinop (Power, fe1, fe2) ->
	      let f1 = loop (F fe1) and f2 = loop (F fe2) in
		power f1 f2
	  | FFFun (f, fe1) -> let fname = ocamlname f
			      and f1 = loop (F fe1) in
	      compose fname f1
	  | FIFun (f, ie1) -> let fname = ocamlname f
			      and f1 = loop (I ie1) in
	      compose fname f1
	  | FVar v when var=v -> FFocaml (fun x -> x)
	  | FVar y -> raise (Unknown_variable y)
	  | FUminus fe1 ->  let f1 = loop (F fe1) in
	      minus f1
      end in
  loop expr_t
	
(*let fun2float_const funresult =
  match funresult with
    | FFocaml f -> f 0.
    | FIocaml f -> float (f 0.)
    | IFocaml f -> f 0
    | IIocaml f -> float (f 0);;*)

let fun2float_fun funresult =
  match funresult with
    | FFocaml f -> f
    | FIocaml f -> fun x -> float (f x)
    | IFocaml f -> fun x -> f (int_of_float x)
    | IIocaml f -> fun x -> float (f (int_of_float x));;

(* inutilisé *)
let fun2float_const funresult = fun2float_fun funresult 0.;;

let fun2int funresult = 
  match funresult with
    | FFocaml _ | IFocaml _ -> raise Type_mismatch
    | FIocaml f -> f 0.
    | IIocaml f -> f 0;;


(* à deux variables float*float->float *)
let compose2 f1 f2 =
    match f1 with
    | IIocaml ii1 -> (fun x -> float (ii1 (int_of_float (f2 x))))
    | FFocaml ff1 -> (fun x -> ff1 (f2 x))
    | IFocaml if1 -> (fun x -> if1 (int_of_float (f2 x)))
    | FIocaml fi1 -> (fun x -> float (fi1 (f2 x)))

(* trop compliqué... *)
let ocamlfun2 expr_t var1 var2 =
  let rec loop et =
    match et with
      | I ie-> begin
	  match ie with
	    | IInt i -> (fun _ -> (float i))
	    | IMinop (o, ie1, ie2) ->  
		let f1 = loop (I ie1) and f2 = loop (I ie2) in
		  (match o with  
		     | Plus -> (fun z -> (f1 z) +. (f2 z))
		     | Minus -> (fun z -> (f1 z) -. (f2 z))
		     | Times -> (fun z -> (f1 z) *. (f2 z)))
	    | IIFun (f, ie1) -> let fname = ocamlname f
				and f1 = loop (I ie1) in
		compose2 fname f1
	    | IFFun (f, fe1) -> let fname = ocamlname f
				and f1 = loop (F fe1) in
		compose2 fname f1
	    | IUminus ie1 ->  let f1 = loop (I ie1) in
		(fun z  -> -. (f1 z))
	end
      | F fe -> begin
	  match fe with
	    | FIntConversion ie -> loop (I ie)
	    | FFloat f ->(fun _ -> f)
	    | FMinop (o, fe1, fe2) -> 
		let f1 = loop (F fe1) and f2 = loop (F fe2) in
		  (match o with  
		     | Plus -> (fun z -> (f1 z) +. (f2 z))
		     | Minus -> (fun z -> (f1 z) -. (f2 z))
		     | Times -> (fun z -> (f1 z) *. (f2 z)))
	    | FFinop (o, fe1, fe2) -> 
		let f1 = loop (F fe1) and f2 = loop (F fe2) in
		  (match o with  
		     | Div -> (fun z -> (f1 z) /. (f2 z))
		     | Power -> (fun z -> (f1 z) ** (f2 z)))
	    | FFFun (f, fe1) -> let fname = ocamlname f
				and f1 = loop (F fe1) in
		compose2 fname f1
	    | FIFun (f, ie1) -> let fname = ocamlname f
				and f1 = loop (I ie1) in
		compose2 fname f1
	    | FVar v when var1=v -> fst
	    | FVar v when var2=v -> snd
	    | FVar y -> raise (Unknown_variable y)
	    | FUminus fe1 ->  let f1 = loop (F fe1) in
		(fun z  -> -. (f1 z))
	end in
    loop expr_t
      
