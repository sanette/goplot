(************************************************************)
(***** transformations type goplot -> fichier goplot ********)
(***** (chaines de caractères) ******************************)
(************************************************************)

open Init

let axis ({ x=sx0; y=sy0 }, _) =
  Printf.sprintf "axis {x=%s; y=%s}" sx0 sy0

let func ({ formula=sf; var=x}, { min=sx0; max=sx1 }, _) =
  Printf.sprintf "func {formula=%s; var=%s},\n{min=%s; max=%s}"
    sf x sx0 sx1

let param ({ formula=sfx; var=sx}, { formula=sfy; var=sy}, 
		 { min=ss0; max=ss1 }, _) =
  Printf.sprintf "param {formula=%s; var=%s},\n{formula=%s; var=%s},\n{min=%s; max=%s}" 
    sfx sx sfy sy ss0 ss1

let text ({ msg=msg; format=format; pos={ x=sx; y=sy }; size=size; 
		  alignment=salign }, _) =
  Printf.sprintf "text {msg=%s\n format=%s; pos={x=%s; y=%s}; size=%d; alignment=%s}"
    msg (match format with
	   | Plain_text -> "Plain_text"
	   | Latex_formula -> "Latex_formula"
	   | Full_latex -> "Full_latex") sx sy size salign

let view ({ x=sx0; y=sy0 }, { x=sx1; y=sy1}) =
  Printf.sprintf "view {x=%s; y=%s},\n{x=%s; y=%s}"
    sx0 sy0 sx1 sy1

let color { Plt.r=r; Plt.g=g; Plt.b=b } =
  Printf.sprintf "color {r=%f; g=%f; b=%f}"
    r g b

let anim ({ formula2=sf; var1=x; var2=t }, { min=sx0; max=sx1 }, 
	  { min=st0; max=st1 }) =
  Printf.sprintf "anim {formula2=%s; var1=%s; var2=%s},\n{min=%s; max=%s},\n{min=%s; max=%s}"
    sf x t sx0 sx1 st0 st1

let surf3d ({ formula2=sfx; var1=u1; var2=v1 }, 
	    { formula2=sfy; var1=u2; var2=v2 }, 
	    { formula2=sfz; var1=u3; var2=v3 }, 
	    { min=su0; max=su1 }, { min=sv0; max=sv1 }, _)  =
  Printf.sprintf "surf3d {formula2=%s; var1=%s; var2=%s},
{formula2=%s; var1=%s; var2=%s},
{formula2=%s; var1=%s; var2=%s},
{min=%s; max=%s},\n{min=%s; max=%s}"
    sfx u1 v1 sfy u2 v2 sfz u3 v3 su0 su1 sv0 sv1

let grid ({ formula2=sf; var1=x; var2=y }, 
	  { min=sx0; max=sx1 }, { min=sy0; max=sy1 }, _)  =
  Printf.sprintf "grid {formula2=%s; var1=%s; var2=%s},\n{min=%s; max=%s},\n{min=%s; max=%s}"
    sf x y sx0 sx1 sy0 sy1

let pause (pt, t) =
  Printf.sprintf "%s %d"
    (match pt with
       | Freeze -> "freeze"
       | Soft -> "pause") t

let motion {move=mv; trange={min=st0; max=st1}} =
  let (_tmin, _tmax) = (Parseutil.get_float st0), (Parseutil.get_float st1) in
    match mv with
      | Translate _ -> raise Not_Implemented (*......*)
      | (Rotate ({x3=sx; y3=sy; z3=sz}, sa)) -> Printf.sprintf
	  "rotate {x3=%s; y3=%s; z3=%s},\n%s,\n{min=%s; max=%s}"
	    sx sy sz sa st0 st1
      | Zoom _ -> raise Not_Implemented (*......*)


let write channel go =
  let s =  match go with
    | Axis p -> axis p
    | Func f -> func f
    | Param f -> param f 
    | Text t ->  text t
    | View v -> view v
    | Color c -> color c
    | Anim_func a -> anim a
    | Surface s -> surf3d s
    | Grid g -> grid g
    | Pause p -> pause p 
    | Motion m -> motion m in
    output_string channel (s ^ "\n")

let olist channel golist =
  List.iter (fun gor -> write channel !gor) (List.rev golist)

let file golist filename = 
  let channel = open_out filename in
    output_string channel ("%% Goplot version " ^ Init.version ^ "\n\n");
    olist channel golist;
    output_string channel ("end\n");
    close_out channel
