(* lecture fichier *)

open Init
open Scanf

let dumb () = ref None

let golist filename =
  let buffer = Scanning.from_file filename in
    let version = try 
      Scanf.bscanf buffer "%%%% Goplot version %s\n\n" (fun s -> s)
    with e -> raise e in
      print_endline version;
      if version <> "0.2" then raise Not_found 
      else let rec loop golist = 
	let keyword = try
	  Scanf.bscanf buffer "%s " (fun s -> s)
	with End_of_file -> "end"
	  | e -> raise e in
	  print_endline ("Keyword=" ^ keyword);
	  if keyword = "end" then golist
	  else let obj = 
	    (* les chaines scanf doivent etre les memes que dans output.ml ! *)
	    (* (sans le mot clef et avec un \n à la fin) *)
	    (match keyword with
	       | "axis" -> Scanf.bscanf buffer "{ x = %s@; y = %s@}\n" 
		   (fun sx0 sy0 -> Axis ({ x=sx0; y=sy0 }, dumb ()))
	       | "func" -> Scanf.bscanf buffer "{ formula = %s@; var = %s@} , { min = %s@; max = %s@}\n"
		   (fun sf x sx0 sx1 -> Func ({ formula=sf; var=String.trim x}, { min=sx0; max=sx1 }, dumb ()) )
	       | "param" -> Scanf.bscanf buffer "{ formula = %s@; var = %s@} , { formula = %s@; var = %s@} , { min = %s@; max = %s@}\n"
		   (fun sfx sx sfy sy ss0 ss1 -> Param ({ formula=sfx; var=String.trim sx}, { formula=sfy; var=String.trim sy}, 
							{ min=ss0; max=ss1 }, dumb ()))
	       | "text" -> Scanf.bscanf buffer "{ msg =%s@\n format = %s@; pos = { x = %s@; y = %s@}; size = %d ; alignment = %s@}\n"
		   (fun msg sformat sx sy size salign -> 
		      let format = (match String.trim sformat |> String.lowercase_ascii with
				      | "plain_text" -> Plain_text
				      | "latex_formula" -> Latex_formula
				      | "full_latex" -> Full_latex
				      | _ -> raise Not_found) in
			Text ({ msg=msg; format=format; pos={ x=sx; y=sy }; size=size; 
				alignment=salign }, dumb ()))
	       | "view" -> Scanf.bscanf buffer "{x=%s@; y=%s@},\n{x=%s@; y=%s@}\n"
		   (fun sx0 sy0 sx1 sy1 -> View ({ x=sx0; y=sy0 }, { x=sx1; y=sy1}))
	       | "color" -> Scanf.bscanf buffer "{r=%f; g=%f; b=%f }\n"
		   (fun r g b -> Color { Plt.r=r; Plt.g=g; Plt.b=b })
	       | "anim" -> Scanf.bscanf buffer "{formula2=%s@; var1=%s@; var2=%s@},\n{min=%s@; max=%s@},\n{min=%s@; max=%s@}\n"
		   (fun sf x t sx0 sx1 st0 st1 -> Anim_func ({ formula2=sf; var1=x; var2=t }, { min=sx0; max=sx1 }, 
							     { min=st0; max=st1 }))
	       | "surf3d" -> Scanf.bscanf buffer "{formula2=%s@; var1=%s@; var2=%s@},\n{formula2=%s@; var1=%s@; var2=%s@},\n{formula2=%s@; var1=%s@; var2=%s@},\n{min=%s@; max=%s@},\n{min=%s@; max=%s@}\n"
		   (fun sfx u1 v1 sfy u2 v2 sfz u3 v3 su0 su1 sv0 sv1 -> 
		      Surface ({ formula2=sfx; var1=u1; var2=v1 }, 
			       { formula2=sfy; var1=u2; var2=v2 }, 
			       { formula2=sfz; var1=u3; var2=v3 }, 
			       { min=su0; max=su1 }, { min=sv0; max=sv1 }, dumb ()))
	       | "grid" -> Scanf.bscanf buffer "{formula2=%s@; var1=%s@; var2=%s@},\n{min=%s@; max=%s@},\n{min=%s@; max=%s@}\n"
		   (fun sf x y sx0 sx1 sy0 sy1 -> Grid ({ formula2=sf; var1=x; var2=y }, 
							{ min=sx0; max=sx1 }, { min=sy0; max=sy1 }, dumb ()))
	       | "pause" -> Scanf.bscanf buffer "%d\n"
		   (fun t -> Pause (Soft, t))
	       | "freeze" -> Scanf.bscanf buffer "%d\n"
		   (fun t -> Pause (Freeze, t))
	       | "rotate" -> Scanf.bscanf buffer "{x3=%s@; y3=%s@; z3=%s@},\n%s@,\n{min=%s@; max=%s@}\n"
		   (fun sx sy sz sa st0 st1 -> Motion {move=Rotate ({x3=sx; y3=sy; z3=sz}, sa);
						       trange={min=st0; max=st1}})
	       | _ -> raise Not_found) in
	    loop ((ref obj)::golist) in
	loop []
	  
let file golistr filename =
  let list = golist filename in
    golistr := list
