(* things to do the first time goplot is launched: detection of tools, opengl,
   etc. *)


open Init
open Gutil
module Osys = Oplot.Plt.Internal

let assistant_box cancel =
  let window = GWindow.window ~title:"Welcome to gOplot"
      ~modal:true () in
  let main_vbox = GPack.vbox ~spacing:2 ~border_width:5
      ~packing:window#add () in
  let cadre = GBin.frame ~border_width:5 ~shadow_type:`IN
      ~packing:main_vbox#add () in
  let cadre_box = GPack.vbox ~packing:cadre#add ~show:true () in
  let title_box = GPack.hbox ~packing:cadre_box#pack () in
  let _ = GMisc.image ~file:(concat imagedir "pelotte_icon64.png")
      ~packing:title_box#pack  () in
  let title = GMisc.label ~text:"Bienvenue sur gOplot" ~packing:title_box#add ()
  in set_font_style ~size:18 title;

  let button_box = GPack.hbox ~spacing:2 ~packing:main_vbox#pack () in
  let button = GButton.button ~stock:`CANCEL
      ~packing:(button_box#pack ~from:`END) () in
  button#connect#clicked ~callback:
    (fun () -> (window#destroy (); cancel ())) |> ignore;
  window, cadre_box, button_box;;

let print_box markup cadre : GPack.box =
  let box = GPack.hbox ~packing:cadre#add ~border_width:5 () in
  GMisc.label ~line_wrap:true ~markup ~packing:box#pack ()
  |> ignore;
  box;;


(* effectue une suite de (messages,actions) dans une fenêtre fixe *)
let assistant steps cancel =
  let (window, cadre, button_box) = assistant_box cancel in
  let rec loop list i =
    print_endline (Printf.sprintf "Assistant page %d." i);
    match list with
    | [] -> window#destroy ()
    | (markup, callback)::rest -> begin
 let box = print_box markup cadre in
 let button = GButton.button ~stock:`OK
     ~packing:(button_box#pack ~from:`END) () in
 button#grab_default ();
 button#connect#clicked ~callback:
   (fun () -> begin
        callback ();
        box#destroy ();
        button#destroy();
        loop rest (i+1)
             end)
        |> ignore;
 ()
      end in
  loop steps 0;
  window#show ();;


let wizard show =
  let list = [("Il semble que ce soit la première fois que vous utilisiez <span \
                color=\"red\" weight=\"bold\">gOplot</span>.
Je vais détecter les outils présents sur votre système.",
        fun () -> (print_endline "Action 1"; Unix.sleep 1));
       ("Un
coup
pour
rien",
        fun () -> (print_endline "Action 2"; Unix.sleep 1));
       ("L'initialisation est terminée !", show)] in
  assistant list show;;


(**********************)

let bienvenue cadre =
  print_box "Il semble que ce soit la première fois que vous utilisiez <span \
             color=\"red\" weight=\"bold\">gOplot</span>.
Je vais détecter les outils présents sur votre système." cadre;;

let check_postscript cadre =
  let warning =
    match (Osys.has_gs, Osys.has_fig2dev) with
    | true, true -> (bold "ghostscript") ^ " et " ^ (bold "fig2dev") ^
                    " sont présents sur votre système.\n\
                     Très bien."
    | true, false ->
      "fig2dev n'a pas été trouvé sur votre système.\n\
       Installez le paquetage \"transfig\" afin de pouvoir exporter en \
       postscript
et imprimer vos graphiques."
    | false, true ->
      "ghostscript n'a pas été trouvé sur votre système,
ce qui est très étonnant.
Installez le paquetage \"ghostscript\" afin de pouvoir exporter en postscript
et imprimer vos graphiques."
    | false, false ->
      "ghostscript n'a pas été trouvé sur votre système,
ce qui est assez étonnant.
Installez les paquetages \"ghostscript\" et \"transfig\"
afin de pouvoir exporter en postscript
et imprimer vos graphiques." in
  print_box warning cadre

let check_latex cadre =
  let warning =
    match (Osys.has_latex, Osys.has_gs) with
    | true, true when (Osys.pngalpha ()) -> begin
        try
          ignore (Osys.latex_to_sdl "$\\displaystyle\\int_0^1f(t)dt$" 36);
          (bold "LaTeX") ^ " est bien installé. Parfait."
        with
        | Osys.Shell_error _ ->
          "Zut. Quelque chose ne va pas avec "
          ^ (bold "LaTeX") ^ "."
        | _ -> "OOps !" end
    | true, true ->
      (bold "LaTeX") ^ " est installé mais "
      ^ (bold "postscript") ^
      "ne possède pas le device \"pngalpha\".\n\
       Du coup, les sorties écran de formules mathématiques ne seront pas de \
       très bonne qualité.\n\
       Essayez d'installer une autre version de "
      ^ (bold "ghostscript") ^ "."
    | true, _ ->
      (bold "LaTeX") ^
      " est bien installé...\nmais ne sert à rien tant que "
      ^ (bold "ghostscript")
      ^ " n'est pas installé.\n\
         Vous ne pourrez pas écrire de formules mathématiques."
    | false, _ ->
      (bold "LaTeX") ^
      " n'a pas été détecté.\n\
       Vous ne pourrez pas écrire de formules mathématiques." in
  print_box warning cadre;;

let fin cadre =
  print_box "L'initialisation est terminée !" cadre;;


(***************************)

let assistant2 steps cancel =
  let (window, cadre, button_box) = assistant_box cancel in
  let rec loop list i =
    print_endline (Printf.sprintf "Assistant page %d." i);
    match list with
    | [] -> (window#destroy (); cancel ())
    | step::rest -> begin
        let box = step cadre in
        let button = GButton.button ~stock:`OK
            ~packing:(button_box#pack ~from:`END) () in
        button#grab_default ();
        button#connect#clicked ~callback:
          (fun () -> begin
               box#destroy ();
               button#destroy();
               loop rest (i+1)
             end) |> ignore
      end in
  loop steps 0;
  window#show ();;

let wizard2 show =
  let steps = [ bienvenue; check_postscript; check_latex; fin ] in
    assistant2 steps show;;
