(* utilitaires GTK *)
   	       
let set_font_style ?(stretch=`CONDENSED) ?(weight=`NORMAL) 
    (* ?(family="DejaVuSans") *) ?(style=`NORMAL) ?(size=12) label = (* size = 12 par exemple, stretch = `CONDENSED, weight = `BOLD,  style = `ITALIC *)
  let fontdesc = label#misc#pango_context#font_description in
    Pango.Font.modify fontdesc ~size:(size*Pango.scale) ~stretch (*~family*) ~weight ~style ();
    label#misc#modify_font fontdesc;;


let bold s = Printf.sprintf "<span weight=\"bold\">%s</span>" s;;
