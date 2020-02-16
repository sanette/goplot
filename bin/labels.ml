(* initialisation des messages, labels, menus, etc... *)
(* codage UTF-8 *)

open Debug;;

let () = if debug then print_endline "( labels"

let main = ref "Oplot main sheet"
let draw = ref "_Draw sheet"
let save_oplot = ref "_Export to Ocaml"
let file = ref "_File"
let detach = ref "Deta_ch"
let view = ref "_View"
let fplot = ref "Function"
let param = ref "Curve"
let surf = ref "Surface"
let grid = ref "Graph"
let motion = ref "Motion"
let axis = ref "Axis"
let color = ref "Color"
let text = ref "Text"
let formula = ref "Formula"
let oview = ref "View"
let anim = ref "Animated plot"
let add = ref "Click on the object\nyou want to create"
let fullscreen = ref "Fullscreen"
let hide_tools = ref "Hide toolbar"
let viewps = ref "View postscript"
let viewfig = ref "Launch xfig"
let view_screenshot = ref "View screenshot"
let verify = ref "_Verify sheet"
let outils = ref "_Tools"
let gallery = ref "Gallery"
let math = ref "_List of mathematical functions"

let fplot_tip = ref "Graph y=f(x)"
let param_tip = ref "2D Parametric curve: (x(s),y(s))"
let formula_tip =ref "Mathematical formula in LaTeX format"
let latex_tip = ref "Write text and/or mathematical formulas\nusing LaTeX"
let view_tip = ref "Set coordinates of window corners\n(for subsequent drawings)"

let assign var s =
  if Glib.Utf8.validate s then var := s

let inits () = 
  let lang = try Scanf.sscanf GtkInit.locale
                   "LC_CTYPE=%s@;" (fun s -> s)
    (* Sys.getenv "LANG" *) with _ -> "" in
  let langue =  if String.contains lang '.' then 
      String.sub lang 0 (String.index lang '.')
    else lang
  in if Debug.debug then print_endline langue;
  match langue with
    "fr_FR" -> (
      assign main "Oplot: feuille principale";
      assign draw "_Lancer le tracé";
      assign save_oplot "_Exporter en Ocaml";
      assign file "_Fichier";
      assign detach "Déta_cher";
      assign view "_Affichage";
      assign fplot "Fonction";
      assign param "Courbe";
      assign surf "Surface";
      assign grid "Relief";
      assign motion "Mouvement";
      assign axis "Axes";
      assign color "Couleur";
      assign text "Texte";
      assign formula "Formule";
      assign oview "Fenêtrage";
      assign anim "Tracé animé";
      assign add "Cliquez sur l'objet\nque vous voulez créer";
      assign fullscreen "Plein écran";
      assign hide_tools "Cacher les outils";
      assign viewps "Afficher postscript";
      assign viewfig "Lancer xfig";
      assign view_screenshot "Capture d'écran";
      assign verify "Vérifier la feuille";
      assign outils "_Outils";
      assign gallery "Galerie";
      assign math "_Liste des fonctions mathématiques";

      assign fplot_tip "Graphe y=f(x)";
      assign param_tip "Courbe paramétrique 2D: (x(s),y(s))";
      assign formula_tip  "Formule mathématique au format LaTeX";
      assign latex_tip "Texte et/ou formules mathématiques\nen LaTeX";
      assign view_tip "Impose les coordonnées des coins de la fenêtre\n(pour les tracés suivants)"
    )
  | _ -> ()

(* pour enlever les '_' *)
let strip s =
  try let i = String.index s '_' in
    ((String.sub s 0 i) ^ (String.sub s (i+1) (String.length s -i -1))) with 
      | Not_found -> s
      | e -> raise e

let () = if debug then print_endline "labels )"
