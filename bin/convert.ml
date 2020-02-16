(********************************************************)
(***** transformations type goplot -> type oplot ********)
(***** les modifs sont sauvées dans l'objet lui-même ****)
(********************************************************)

open Init

(* calcule et retourne l'objet axis *)
let axis ({ x=sx0; y=sy0 }, o) =
  match !o with
    | Some obj -> obj
    | None -> let x0 = Parseutil.get_float sx0
	      and y0 = Parseutil.get_float sy0 in
      let obj = Plt.axis x0 y0 in 
	o := Some obj; obj

let func ({ formula=sf; var=x}, { min=sx0; max=sx1 }, o) =
  match !o with
    | Some obj -> obj
    | None -> let (f,_) = Parseutil.get_fun sf x
	      and x0 = Parseutil.get_float sx0
	      and x1 = Parseutil.get_float sx1 in
      let obj = Plt.adapt_plot f x0 x1 in
	o := Some obj; obj

let param ({ formula=sfx; var=sx}, { formula=sfy; var=sy}, 
		 { min=ss0; max=ss1 }, o) =
  match !o with
    | Some obj -> obj
    | None -> let (fx,_) = Parseutil.get_fun sfx sx
	      and (fy,_) = Parseutil.get_fun sfy sy
	      and s0 = Parseutil.get_float ss0
	      and s1 = Parseutil.get_float ss1 in
      let obj = Plt.parametric_plot fx fy s0 s1 in
	o := Some obj; obj

let text ({ msg=msg; format=format; pos={ x=sx; y=sy }; size=size; 
		  alignment=salign }, o) =
  match !o with
    | Some obj -> obj
    | None -> let x0 = Parseutil.get_float sx
	      and y0 = Parseutil.get_float sy
	      and align = (match salign with
			     | "CENTER" -> Plt.CENTER
			     | "LEFT" -> Plt.LEFT
			     | "RIGHT" -> Plt.RIGHT
			     | _ -> raise Not_found)
	      and (command, ftext) = match format with
		| Plain_text -> Plt.text, msg
		| Latex_formula -> Plt.latex, "$\\displaystyle " ^ msg ^ "$"
		| Full_latex -> Plt.latex, msg in
      let obj = command ftext ~size ~align x0 y0 in
	o := Some obj; obj

(* celui-là on n'a pas besoin de le sauver ? *)
let view ({ x=sx0; y=sy0 }, { x=sx1; y=sy1}) =
  let x0 = Parseutil.get_float sx0
  and x1 = Parseutil.get_float sx1
  and y0 = Parseutil.get_float sy0
  and y1 = Parseutil.get_float sy1 in
    Plt.view x0 y0 x1 y1

let color { Plt.r=r; Plt.g=g; Plt.b=b } =
  Plt.color r g b

let anim ({ formula2=sf; var1=x; var2=t }, { min=sx0; max=sx1 }, 
	  { min=st0; max=st1 }) =
  let (f,_) = Parseutil.get_fun2 sf x t
  and (x0, x1) = (Parseutil.get_float sx0), (Parseutil.get_float sx1) 
  and (t0, t1) = (Parseutil.get_float st0), (Parseutil.get_float st1) in
    Plt.anim_plot (fun t x -> f (x,t)) ~t0 ~t1 x0 x1

let surf3d ({ formula2=sfx; var1=u1; var2=v1 }, 
	    { formula2=sfy; var1=u2; var2=v2 }, 
	    { formula2=sfz; var1=u3; var2=v3 }, 
	    { min=su0; max=su1 }, { min=sv0; max=sv1 }, _)  =
  let (fx,_) = Parseutil.get_fun2 sfx u1 v1
  and (fy,_) = Parseutil.get_fun2 sfy u2 v2
  and (fz,_) = Parseutil.get_fun2 sfz u3 v3
  and (umin, umax) = (Parseutil.get_float su0), (Parseutil.get_float su1) 
  and (vmin, vmax) = (Parseutil.get_float sv0), (Parseutil.get_float sv1) in
    Plt.surf3d_plot (fun u v -> fx (u,v)) 
      (fun u v -> fy (u,v)) (fun u v -> fz (u,v)) umin vmin umax vmax

let grid ({ formula2=sf; var1=x; var2=y }, 
	  { min=sx0; max=sx1 }, { min=sy0; max=sy1 }, _)  =
  let (f,_) = Parseutil.get_fun2 sf x y
  and (xmin, xmax) = (Parseutil.get_float sx0), (Parseutil.get_float sx1) 
  and (ymin, ymax) = (Parseutil.get_float sy0), (Parseutil.get_float sy1) in
    Plt.grid_plot (fun x y -> f (x,y)) xmin ymin xmax ymax

let pause (pt, t) =
  let command = match pt with
    | Freeze -> Plt.freeze
    | Soft -> Plt.pause in 
    command t

(* non sauvé *)
let motion {move=mv; trange={min=st0; max=st1}} =
  let (tmin, tmax) = (Parseutil.get_float st0), (Parseutil.get_float st1) in
    match mv with
      | Translate _ -> raise Not_Implemented (*......*)
      | (Rotate ({x3=sx; y3=sy; z3=sz}, sa)) -> Plt.rotate
	  (Parseutil.get_float sx) (Parseutil.get_float sy) 
	    (Parseutil.get_float sz) (Parseutil.get_float sa)
	    (tmax -. tmin) (* à revoir *)
      | Zoom _ -> raise Not_Implemented (*......*)


let convert2oplot go =
  match go with
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
    | Motion m -> motion m

(* Convertit pour oplot seulement les objets qui n'ont pas déjà été
   convertis. *)
let olist golist =
  Debug.print "Converting to Oplot...";
  List.rev (List.map (fun go -> convert2oplot !go) golist)

let keyword go = 
  match go with
    | Axis _ -> "axis"
    | Func _ -> "func"
    | Param _ -> "param" 
    | Text (t,_) -> begin match t.format with
	| Plain_text -> "text"
	| Latex_formula -> "formula"
	| Full_latex -> "latex"
      end
    | View _ -> "view"
    | Color _ -> "color"
    | Anim_func _ -> "anim"
    | Surface _ -> "surf3d"
    | Grid _ -> "grid"
    | Pause _ -> "freeze"
    | Motion m -> begin match m.move with
	| Translate _ -> "translate"
	| Rotate _ -> "rotate"
	| Zoom _ -> "zoom"
      end

let small_icon_filename go = 
  (keyword go) ^ "_icon32.png"
