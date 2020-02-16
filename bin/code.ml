(*********************************************************)
(*** convertit en source ocaml pour oplot ****************)
(*********************************************************)

open Init
module Plt = Oplot.Plt
               
let axis ({ x=sx0; y=sy0 }, _) =
  let (x0, y0) = (Parseutil.get_float sx0), (Parseutil.get_float sy0) in
    Printf.sprintf "axis (%s) (%s)" (string_of_float x0) (string_of_float y0)

let func ({ formula=sf; var=x}, { min=sx0; max=sx1 }, _) =
  let (x0, x1) = (Parseutil.get_float sx0), (Parseutil.get_float sx1) in
    Printf.sprintf "adapt_plot (fun %s -> %s) (%s) (%s)" x
      (Parseutil.get_string sf)
      (string_of_float x0) (string_of_float x1)

let param ({ formula=sfx; var=sx}, { formula=sfy; var=sy}, 
	   { min=ss0; max=ss1 }, _) =
  let fx = Parseutil.get_string sfx
  and fy = Parseutil.get_string sfy
  and s0 = Parseutil.get_float ss0
  and s1 = Parseutil.get_float ss1 in
    Printf.sprintf 
      "parametric_plot (fun %s -> %s) (fun %s -> %s) (%s) (%s)" sx fx sy fy 
      (string_of_float s0) (string_of_float s1)

let text ({ msg=msg; format=format; pos={ x=sx; y=sy }; size=size; 
	    alignment=salign }, _) =
  let (command, ftext) = match format with
    | Plain_text -> "text", msg
    | Latex_formula -> "latex", "$\\displaystyle" ^ msg ^ "$"
    | Full_latex -> "latex", msg in
  let (x0, y0) = (Parseutil.get_float sx), (Parseutil.get_float sy) in
    Printf.sprintf "%s %S ~size:%d ~align:%s (%s) (%s)"
      command ftext size salign (string_of_float x0) (string_of_float y0)   

let view ({ x=sx0; y=sy0 }, { x=sx1; y=sy1}) =
  let (x0, y0) = (Parseutil.get_float sx0), (Parseutil.get_float sy0)
  and (x1, y1) = (Parseutil.get_float sx1), (Parseutil.get_float sy1) in
    Printf.sprintf "view (%s) (%s) (%s) (%s)" 
      (string_of_float x0) (string_of_float y0)
      (string_of_float x1) (string_of_float y1)

let color { Plt.r=r; Plt.g=g; Plt.b=b } =
  Printf.sprintf "color %s %s %s" 
    (string_of_float r) (string_of_float g) (string_of_float b)

let anim ({ formula2=sf; var1=_; var2=_ }, { min=sx0; max=sx1 }, 
	  { min=st0; max=st1 }) =
  let (x0, x1) = (Parseutil.get_float sx0), (Parseutil.get_float sx1) 
  and (t0, t1) = (Parseutil.get_float st0), (Parseutil.get_float st1) in
    Printf.sprintf "anim_plot (fun t x -> %s) ~t0:(%s) ~t1:(%s) (%s) (%s) "
      (Parseutil.get_string sf)
      (string_of_float t0) (string_of_float t1)
      (string_of_float x0) (string_of_float x1)

let surf3d ({ formula2=sfx; var1=_; var2=_ }, 
	    { formula2=sfy; var1=_; var2=_ }, 
	    { formula2=sfz; var1=_; var2=_ }, 
	    { min=su0; max=su1 }, { min=sv0; max=sv1 }, _)  =
  let (umin, umax) = (Parseutil.get_float su0), (Parseutil.get_float su1) 
  and (vmin, vmax) = (Parseutil.get_float sv0), (Parseutil.get_float sv1) in
    Printf.sprintf 
      "surf3d_plot (fun u v -> %s) (fun u v -> %s) (fun u v -> %s) (%s) (%s) (%s) (%s)" 
      (Parseutil.get_string sfx)
      (Parseutil.get_string sfy)
      (Parseutil.get_string sfz)
      (string_of_float umin) (string_of_float vmin)
      (string_of_float umax) (string_of_float vmax)

let grid  ({ formula2=sf; var1=_; var2=_ }, 
	   { min=sx0; max=sx1 }, { min=sy0; max=sy1 }, _)  =
  let (xmin, xmax) = (Parseutil.get_float sx0), (Parseutil.get_float sx1) 
  and (ymin, ymax) = (Parseutil.get_float sy0), (Parseutil.get_float sy1) in
    Printf.sprintf 
      "grid_plot (fun x y -> %s) (%s) (%s) (%s) (%s)"
      (Parseutil.get_string sf)
      (string_of_float xmin) (string_of_float ymin)
      (string_of_float xmax) (string_of_float ymax)

let pause (pt, t) =
  let keyword = match pt with
    | Freeze -> "Freeze"
    | Soft -> "Pause" in 
    Printf.sprintf "%s %d" keyword t
      
let motion {move=mv; trange={min=st0; max=st1}} =
  let (tmin, tmax) = (Parseutil.get_float st0), (Parseutil.get_float st1) in
    match mv with
      | Translate _ -> raise Not_Implemented (*......*)
      | (Rotate ({x3=sx; y3=sy; z3=sz}, sa)) -> Printf.sprintf
	  "rotate (%s) (%s) (%s) (%s) (%s)"
	    (string_of_float (Parseutil.get_float sx))
	    (string_of_float (Parseutil.get_float sy))
	    (string_of_float (Parseutil.get_float sz))
	    (string_of_float (Parseutil.get_float sa))
	    (string_of_float (tmax -. tmin)) (* revoir *)
      | Zoom _ -> raise Not_Implemented (*......*)
	  
let obj go =
  try
    match go with
      | Axis a -> axis a
      | Func f -> func f
      | Param f -> param f
      | Text t -> text t
      | View v -> view v
      | Anim_func f -> anim f
      | Surface s -> surf3d s
      | Grid g -> grid g
      | Color c -> color c
      | Pause p -> pause p
      | Motion m -> motion m
  with _ -> "ERROR!"

let dump olist channel =
  let codelist = List.map (fun o -> obj !o) olist in
  let rec loop codl number = 
    match codl with
	[] -> ()
      | hd :: r -> 
	  Printf.fprintf channel "let p%d = %s;;\n" number hd;
	  loop r (number + 1)
  in loop (List.rev codelist) 1;
    let l = List.length codelist in
      if l >= 1 then begin
	Printf.fprintf channel "let sh = [ p1 ";
	for i = 2 to (List.length olist) do
	  Printf.fprintf channel "; p%d " i;
	done;
	Printf.fprintf channel "];;\n";
      end;
      flush channel
	
