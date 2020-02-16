(* utils maths *)
print_endline "Initialisation GSL errors...";;

Gsl.Error.init ();;

let sinc x =
  if x = 0. then 1.
  else sin x /. x;;

(* la fonction gamma retourne (a,b) où a et b sont de type Gsl_fun (un
   champ resultat, un champ erreur) *)
(* argument entre -pi et pi *)

let arg_gamma x y =
  let (_,b) =  Gsl.Sf.lngamma_complex_e x y in
  b.Gsl.Fun.res;;

let ( ^ ) x n = Gsl.Math.pow_int x n;;
(* let rec ( ^ ) n p = if p = 0 then 1 
   else if p mod 2 = 0 then n ^ (p/2) else n*(n ^ p/2);; *)

let gamma x = 
  try Gsl.Sf.gamma x
  with 
  | Gsl.Error.Gsl_exn _ -> nan
  | e -> raise e


let airyai x = Gsl.Sf.airy_Ai x Gsl.Fun.SIMPLE

let airybi x = Gsl.Sf.airy_Bi x Gsl.Fun.SIMPLE

let frac x = if x > 0. then x -. floor x else x -. floor x -. 1.
