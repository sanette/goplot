open Init

let timer = GMain.Timeout.add ~ms:20 ~callback:(fun () -> (); true)

let splash =
  let open Plt in
  let green1 = {r=0.51; g=0.82; b=0.29} in
  (* let green2 = {r=0.43; g=0.65; b=0.29} in *)
  let red1 = {r=0.76; g=0.; b=0.} in
  let t1 = text ~size:32 "Bienvenue sur gOplot!" 0. 0.5
  (* and t2 = text "Appuyez sur la touche ESCAPE ou Q" 0. 0.
   * and t3 = text "pour fermer cette fenêtre graphique." 0. (-0.1) *)
  and t4 = text "La touche F permet de basculer entre" 0. (-0.3)
  and t5 = text "le mode fenêtre et le mode plein écran." 0. (-0.4)
  and p = parametric_plot (fun s -> sqrt(s/.20.) *. sin(s)) (fun s -> sqrt(s/.20.) *. cos(s)) (0.) (15.7079632679)
  and v = view (-1.) (-1.) 1. 1. in
    [v; Color green1; p; Color red1; t1; Color black;(* t2;t3; *)t4; t5]

(*  let entry = textentry "Text:" (match latex with
				   | 0 -> "A title"
				   | 1 -> "\\int_0^1 f(x)dx"
				   | 2 -> "Some \\LaTeX{} code"
				   | _ -> raise Not_found) main_vbox#add in
*)


(* objets par défaut. Certains peuvent être modifiés. *)

let dumb = ref None
(* attention cet objet devient une référence partagée. En principe pas grave car
   elle est ré-affectée à chaque création d'objet. *)

let axis = Axis ({ x="0"; y="0"}, dumb)

let view = View ({ x="-1"; y="-1" }, { x="1"; y="1" })

let color = ref (Color { r=0.; g=1.; b=0. })

let func = Func ({ formula="sin(x)"; var="x"}, { min="-Pi"; max="Pi"}, dumb)

let anim = Anim_func ({ formula2="sin(x+t)"; var1="x"; var2="t" },
		      { min="-Pi"; max="Pi"}, { min="0"; max="0" })

let param = Param ({ formula="sqrt(s)*sin(s)"; var="s"},
		   { formula="sqrt(s)*cos(s)"; var="s"}, { min="0"; max="5*Pi"}, dumb)

let surf3d = Surface ({ formula2="(1+cos(u)/2)*cos(v)/2"; var1="u"; var2="v" },
		      { formula2="(1+cos(u)/2)*sin(v)/2"; var1="u"; var2="v" },
		      { formula2="sin(u)/4"; var1="x"; var2="y" },
		      { min="-Pi"; max="Pi"}, { min="0"; max="2*Pi" }, dumb)

let grid = Grid ({ formula2="(1+cos(x)/2)*cos(y)/4"; var1="x"; var2="y" },
		 { min="-Pi"; max="3*Pi"}, { min="-Pi"; max="3*Pi" }, dumb)

let text = Text ({ msg="Hello world"; format=Plain_text; pos={ x="0"; y="0" };
		   size=24; alignment="CENTER"}, dumb)

let formula = Text ({ msg="\\int_0^1 f(x) dx"; format=Latex_formula;
		      pos={ x="0"; y="0" }; size=24; alignment="CENTER"}, dumb)

let latex = Text ({ msg="Here you \\textbf{must} use $f(x)=\\sin(x)$.";
		    format=Full_latex; pos={ x="0"; y="0" };
		    size=24; alignment="CENTER"}, dumb)

let pause = Pause (Soft, 0)

let motion = Motion { move=(Rotate ({x3="0"; y3="1"; z3="0"}, "0.05"));
		      trange={min="0"; max="4"} }

let rotate = ({x3="0"; y3="1"; z3="0"}, "0.05")
let translate =  {x3="1"; y3="0"; z3="0"}
let zoom = "??"


(* fichier code *)
let header =
"(* OCaml toplevel file created by goplot *)
#use \"topfind\";;
#thread;;
#require \"oplot\";;
open Oplot.Plt;;
\n\n"

let footer = (* + fullscreen *)
"display sh;;\n"
