(* GUI pour oplot. VU NGOC San juillet 2006 *)
(* ce fichier est codé en utf-8 *)

(* 0.3: avec gtkgl *)

(* 0.61: gestion toolbar remaniee. option cacher toolbar*)

open Init
open Gutil

exception No_such_item of string;;

let fullscreen = ref false;;

(* liste de widgets à désactiver si la liste d'objets à tracer est vide *)
let desactivate = ref [];;
type desactivate_widget = Desactivate | Do_not_desactivate;;
let update_desactivate_list item desact =
  let addlist e l = l := e :: !l in
  match desact with
  | Desactivate -> addlist item desactivate
  | Do_not_desactivate -> ();;

(**********************************************************************)

let create_oplot_file olist filename =
  let channel  = open_out filename in    
  output_string channel Default.header;
  Code.dump olist channel;
  output_string channel Default.footer;
  close_out channel
      
let entry_callback style entry =
  let s = entry#text in
  let t = Style.name style
  and (out,message_type) = 
    try
      match style with
      | Style.Text -> s, `INFO
      | Style.Math var -> let (_, formule) = Parseutil.get_fun s var in
        "CORRECT.\n\nCode: " ^ formule, `INFO
      | Style.Float -> "value=" ^ (string_of_float (Parseutil.get_float s)), `INFO
      | Style.Int -> "value=" ^ (string_of_int (Parseutil.get_int s)), `INFO
      | Style.Math2 (var1, var2)-> let (_, formule) = Parseutil.get_fun2 s var1 var2 in
        "CORRECT.\n\nCode: " ^ formule, `INFO
    with
    | Parseutil.Error err -> ("ERROR:\n\n" ^ err), `ERROR
    (*      | Parser.Error -> "ERROR:\n\nSyntax error", `ERROR*)
    | Parseutil.Math.Undefined_function f -> 
      ("ERROR:\n\nUndefined function: " ^ f), `ERROR
    | Parseutil.Math.Unknown_variable v ->
      ("ERROR:\n\nUnknown variable: " ^ v), `ERROR
    | Parseutil.Math.Type_mismatch ->
      "ERROR:\n\n integer value expected", `WARNING
    | e -> raise e in
  let message = Printf.sprintf "%s: %s\n" t out in
  print_endline message;
  flush stdout;
  let msg_win = GWindow.message_dialog ~message_type 
      ~buttons:(GWindow.Buttons.close) ~message ~title:"Entry check" 
      ~position:`MOUSE 
      ~modal:true () in
  msg_win#connect#response ~callback:(fun _ -> msg_win#destroy ())
  |> ignore;
  msg_win#show ();;

let get_coords ?(zentry:GEdit.entry option) entry1 entry2 =
  let x = entry1#text
  and y = entry2#text 
  and z = (match zentry with
      | None -> "0"
      | Some entry -> entry#text) in
  (x,y,z)

let ask_coord message ?(xlabel=" x=") ?(ylabel=" y=") 
    ?zlabel xs ys ?(zstring="") packing =

  let entryframe = GBin.frame ~label:message ~border_width:5 ~packing () in

  (*  let vbox =  GPack.vbox ~packing:entryframe#add () in
      GMisc.label ~text:message ~packing:vbox#pack (); *)
  let hbox = GPack.hbox ~packing: (*vbox#pack*) entryframe#add ~border_width:5 () in

  let get_mouse_btn = GButton.button ~packing:hbox#pack () in
  GMisc.label ~text:xlabel ~packing:hbox#pack ()
  |> ignore;
  let entry1 = GEdit.entry ~max_length: 50 ~packing: hbox#add () in
  Style.set_entry_style Style.Float entry1 ~callback:entry_callback;
  entry1#set_text xs; (* (Printf.sprintf "%g" x)*)

  GMisc.label ~text:(" " ^ ylabel) ~packing:hbox#pack ()
  |> ignore;
  let entry2 = GEdit.entry ~max_length: 50 ~packing: hbox#add () in
  Style.set_entry_style Style.Float entry2 ~callback:entry_callback;
  entry2#set_text ys; (*(Printf.sprintf "%g" y) *)

  let entry3 = match zlabel with
    | None -> entry2
    | Some label -> begin
        GMisc.label ~text:(" " ^ label) ~packing:hbox#pack ()
        |> ignore;
        let entry = GEdit.entry ~max_length: 50 ~packing: hbox#add () in
        Style.set_entry_style Style.Float entry ~callback:entry_callback;
        entry#set_text zstring; entry end in

  (* à finir... *)
  get_mouse_btn#connect#clicked ~callback:
    (fun () -> GMain.Timeout.add ~ms:100 ~callback:
        (fun () ->
           entry1#set_text (Osys.get_mouse_x () |> string_of_int)
           |> ignore; true)
               |> ignore
    )
  |> ignore;

  (entry1,entry2,entry3)
     
(*    let button = GButton.button ~label: "OK" ~packing: hbox#add () in *)
(*      button#connect#clicked ~callback: *)
(*        (fun () ->  *)
(*    let (getx,gety) = get_coords entry1 entry2 in *)
(*      x:=getx; y:=gety;  *)
(*      Printf.printf "x= %s, y=%s\n" (string_of_float !x) (string_of_float !y); *)
(*      flush stdout); *)
(*      button#grab_default () *)
     
let textentry ?(style = Style.Text) label text packing =
  let entryframe = GBin.frame ~label  ~border_width:5 ~packing () in
  let entrybox = GPack.hbox  ~border_width:5 ~packing:entryframe#add () in
  let entry = GEdit.entry ~text ~packing:entrybox#add () in
    Style.set_entry_style style entry ~callback:entry_callback;
    entry


let validatebutton packing =
  let buttonbox =  GPack.vbox  ~border_width:5 ~packing () in
  let button = GButton.button ~stock:`OK (*~label: "Create"*)
      ~packing:buttonbox#add () in
  button

let curseur_hor value digits ~lower ~upper step_incr page_incr packing =
  let range =  GData.adjustment ~value ~lower ~upper:(upper +. page_incr)
      ~step_incr ~page_incr () in
  let hbox = GPack.hbox ~width:200 ~packing () in
  let curseur = GRange.scale `HORIZONTAL ~value_pos:`LEFT
      ~adjustment:range ~digits ~draw_value:true
      ~packing:hbox#add ~show:true () in
  curseur;;
(* utiliser curseur#adjustment#value *)

let ask_font () =
  let selection =
    GWindow.font_selection_dialog ~modal:true () in
  selection #cancel_button#connect#clicked ~callback:selection#destroy
  |> ignore;
  selection #ok_button#connect#clicked ~callback:
    (fun () -> 
       let font = selection#selection#font_name in 
       print_endline font;
       selection#destroy ())
  |> ignore;
  selection#show ()

let ask_font2 () =
  let window = GWindow.window ~title: "Font selection" ~border_width: 0 () in
  let main_vbox = GPack.vbox ~packing: (window#add) () in
  let selection =
    GMisc.font_selection ~packing:main_vbox#add ~show:true () in
  window#show();
  selection#font_name;;

(*****)

let init_object_dialog title header icon =
  let window = GWindow.window ~title ~border_width: 0 ~position:`MOUSE
      ~show:true () in
  let main_vbox = GPack.vbox ~border_width:5 ~packing:window#add () in
  let head_box = GPack.hbox ~border_width:5 ~packing:main_vbox#pack() in
  GMisc.image ~file:(imagepath icon) ~packing:head_box#pack ()
  |> ignore;
  GMisc.label  ~line_wrap:true ~text:header ~packing:head_box#add ()
  |> ignore;
  main_vbox, window#destroy 

(********************************************************)
(*** cree des objets goplots ****************************)  
(********************************************************)

let view v action =
  let text = "Enter new boundary coordinates\nfor the window:" in
  let main_vbox,close = init_object_dialog "(Re)define view range" text
      "view_icon" in
  let (point1,point2) = v in
  let (entry1,entry2,_) =  ask_coord "Bottom-left corner:"
      point1.x point1.y main_vbox#pack in
  let (entry3,entry4,_) =  ask_coord "Top-right corner:"
      point2.x point2.y main_vbox#pack in

  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let (x0,y0,_) = get_coords entry1 entry2 in
      let (x1,y1,_) = get_coords entry3 entry4 in
      action (View ({ x=x0; y=y0 }, { x=x1; y=y1 }));
      close ())
  |> ignore;
  button #grab_default ();; (* sert à quoi ? *)

let axis a action =
  let main_vbox,close = init_object_dialog
      "Create axis" "Create new axis" "axis_icon" in
  let (point,_) = a in
  let (entry1,entry2,_) =  ask_coord "Origine des axes:" 
      point.x point.y main_vbox#pack in

  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let (x0,y0,_) = get_coords entry1 entry2 in
      action (Axis ({ x=x0; y=y0}, ref None));
      close ())
  |> ignore;
  button #grab_default ();;


let color co action =
  let to_rgb c =
    let to_float x = (float x) /. 65535. in
    (to_float (Gdk.Color.red c), 
     to_float (Gdk.Color.green c), 
     to_float (Gdk.Color.blue c)) 
  and to_gtk c = 
    let to_int x = int_of_float (x *. 65535.) in
    GDraw.color (`RGB (to_int c.Plt.r, to_int c.Plt.g, to_int c.Plt.b))
  in
  let csd = GWindow.color_selection_dialog ~title:"Change pen color" ~position:`MOUSE () in
  csd # colorsel # set_color (to_gtk co);
  csd # colorsel # set_has_palette true;
  csd # ok_button # connect#clicked ~callback:(fun () ->
      let (r,g,b) = to_rgb (csd#colorsel#color) in
      Default.color := (Color {Plt.r=r; Plt.g=g; Plt.b=b});
      action !Default.color;
      csd#destroy ())
  |> ignore;
  csd # cancel_button # connect#clicked ~callback:csd#destroy
  |> ignore;
  csd # show ()

let func (f,r,_) action =
  let title = "Create function plot" in
  let text = "Enter function using variable x:" in
  let main_vbox,close = init_object_dialog title text "func_icon" in
  let label,text,style = ("f(x)=", f.formula, Style.Math "x") in
  let entry = textentry ~style label text main_vbox#add in
  let (xmin_entry,xmax_entry,_) =  ask_coord ~xlabel:"x_min=" ~ylabel:"x_max=" "x range:" r.min r.max main_vbox#pack in
  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let (xmin,xmax,_) = get_coords xmin_entry xmax_entry
      and t = entry#text in
      action (Func ( {formula=t; var="x"}, { min=xmin; max=xmax }, ref None));
      close  ())
  |> ignore;
  button #grab_default ()

let anim (f,r1,r2) action =
  let title = "Create realtime animated function plot" in
  let text = "Enter function using spacial variable x\nand time variable t (expressed in seconds):" in
  let main_vbox,close = init_object_dialog title text "anim_icon" in
  let label,text,style = ("f(x,t)=", f.formula2, Style.Math2 ("x", "t")) in
  let entry = textentry ~style label text main_vbox#add in
  let (xmin_entry,xmax_entry,_) =  ask_coord ~xlabel:"x_min=" ~ylabel:"x_max=" "x range:" r1.min r1.max main_vbox#pack in
  let (tmin_entry,tmax_entry,_) = ask_coord ~xlabel:" t_min" ~ylabel:" t_max=" "time range (t_max=0 for infinite range):" r2.min r2.max main_vbox#pack in
  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let (xmin,xmax,_) = get_coords xmin_entry xmax_entry 
      and (tmin,tmax,_) =  get_coords tmin_entry tmax_entry
      and t = entry#text in
      action (Anim_func ( {formula2=t; var1="x"; var2="t"},
                          { min=xmin; max=xmax }, { min=tmin; max=tmax}));
      close  ())
  |> ignore;
  button #grab_default ()

      
(* à unifier avec la précédente ? *)

let param (fx, fy, r, _) action =
  let text = "Enter x and y coordinates as functions of the parameter s:" in
  let main_vbox,close = init_object_dialog "Create parametric plot" text "param_icon" in
  let labelx,textx = ("x(s)=", fx.formula)
  and labely,texty = ("y(s)=", fy.formula) in 
  let entryx = textentry ~style:(Style.Math "s") labelx textx main_vbox#add in
  let entryy = textentry ~style:(Style.Math "s") labely texty main_vbox#add in
  let (smin_entry,smax_entry,_) =  ask_coord ~xlabel:"s_min=" ~ylabel:"s_max=" "s range:" r.min r.max main_vbox#pack in
  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let (smin,smax,_) = get_coords smin_entry smax_entry
      and (fx,fy) = entryx#text, entryy#text in
      action (Param ({ formula=fx; var="s" }, { formula=fy; var="s" },
                     { min=smin; max=smax }, ref None));
      close  ())
  |> ignore;
  button #grab_default ()

let surf3d (fx, fy, fz, r1, r2, _) action =
  let text = "Enter x, y and z coordinates as functions of the parameters (u,v):" in
  let main_vbox,close = init_object_dialog "Create parametric surface" text "surf3d_icon" in
  let labelx,textx = ("x(u,v)=", fx.formula2)
  and labely,texty = ("y(u,v)=", fy.formula2)
  and labelz,textz = ("z(u,v)=", fz.formula2)
  and style = Style.Math2 ("u", "v") in 
  let entryx = textentry ~style labelx textx main_vbox#add in
  let entryy = textentry ~style labely texty main_vbox#add in
  let entryz = textentry ~style labelz textz main_vbox#add in
  let (umin_entry,umax_entry,_) =  ask_coord ~xlabel:"u_min=" ~ylabel:"u_max=" "u range:" r1.min r1.max main_vbox#pack
  and (vmin_entry,vmax_entry,_) =  ask_coord ~xlabel:"v_min=" ~ylabel:"v_max=" "v range:" r2.min r2.max main_vbox#pack
  in
  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let (umin,umax,_) = get_coords umin_entry umax_entry
      and (vmin,vmax,_) = get_coords vmin_entry vmax_entry
      and (fx,fy,fz) = (entryx#text, entryy#text, entryz#text) in
      action (Surface ({ formula2=fx; var1="u"; var2="v" },
                       { formula2=fy; var1="u"; var2="v" },
                       { formula2=fz; var1="u"; var2="v" },
                       { min=umin; max=umax }, { min=vmin; max=vmax },
                       ref None));
      close ())
  |> ignore;
  button #grab_default () 


let grid (f, r1, r2, _) action =
  let text = "Enter z coordinate as a function of (x,y):" in
  let main_vbox,close = init_object_dialog "Create a mountain-like surface" text "grid_icon" in
  let label,text = ("z(x,y)=", f.formula2) in
  let entry = textentry ~style:(Style.Math2 ("x", "y")) label text main_vbox#add in
  let (xmin_entry,xmax_entry,_) =  ask_coord ~xlabel:"x_min=" ~ylabel:"x_max=" "x range:" r1.min r1.max main_vbox#pack
  and (ymin_entry,ymax_entry,_) =  ask_coord ~xlabel:"y_min=" ~ylabel:"y_max=" "y range:" r2.min r2.max main_vbox#pack
  in
  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let (xmin,xmax,_) = get_coords xmin_entry xmax_entry
      and (ymin,ymax,_) = get_coords ymin_entry ymax_entry
      and f = entry#text in
      action (Grid ({ formula2=f; var1="x"; var2="y" },
                    { min=xmin; max=xmax }, { min=ymin; max=ymax },
                    ref None));
      close ())
  |> ignore;
  button #grab_default () 

(******)

let get_combo_entry (combo : #GEdit.combo_box) column =
  match combo#active_iter with
  | None -> "NULL"
  | Some row -> combo#model#get ~row ~column

let text (t,_) action =
  let (text, image, latex) = match t.format with
    | Plain_text -> "Enter text:", "text_icon", 0
    | Latex_formula -> "Enter LaTeX formula:", "formula_icon", 1
    | Full_latex -> "Enter LaTeX code:", "latex_icon", 2 in
  let main_vbox,close = init_object_dialog "Create text or LaTeX object" 
      text image in
  let entry = textentry "Text:" t.msg main_vbox#add in
  let format_hbox  = GPack.hbox ~packing: main_vbox#pack () in
  GMisc.label ~text:"Text format:" ~packing:format_hbox#pack ()
  |> ignore;
  let (format_combo, (_, format_column)) = 
    GEdit.combo_box_text ~packing:format_hbox#pack 
      ~strings:[ "Plain text" ; "LaTeX formula" ; "Full LaTeX" ] () in
  format_combo#set_active latex;
  let (entry1,entry2,_) =  ask_coord "Text coordinates:" t.pos.x t.pos.y main_vbox#pack in
  let hbox = GPack.hbox ~packing: main_vbox#pack () in
  GMisc.label ~text:"Text size:" ~packing:hbox#pack ()
  |> ignore;
  let size_entry = GEdit.entry ~text:(string_of_int t.size) ~width_chars:5 
      (*~max_length:3*) ~packing: hbox#pack () in
  Style.set_entry_style Style.Int size_entry ~callback:entry_callback;
  GMisc.label ~text:"Text alignment:" ~packing:hbox#pack ()
  |> ignore;
  let size_menu = GPack.vbox ~border_width:5 ~packing:hbox#pack () in
  let (size_combo, (_, size_column)) = 
    GEdit.combo_box_text ~packing:size_menu#pack 
      ~strings:[ "CENTER" ; "LEFT" ; "RIGHT" ] () in
  size_combo#set_active (match t.alignment with 
      | "CENTER" -> 0
      | "LEFT" -> 1
      | "RIGHT" -> 2
      | _ -> raise (No_such_item t.alignment));

  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let text = Glib.Convert.locale_from_utf8 entry#text in
      (if text <> "" then 
         let tf = match (get_combo_entry format_combo format_column) with
           | "Plain text" -> Plain_text
           | "LaTeX formula" -> Latex_formula
           | "Full LaTeX" -> Full_latex
           | _ -> raise (No_such_item "text") in
         let (x0,y0,_) = get_coords entry1 entry2
         and align = get_combo_entry size_combo size_column 
         and size= Parseutil.get_int size_entry#text in
         action (Text ({ msg=text; format=tf; pos={x=x0; y=y0};
                         size=size; alignment=align }, ref None)));
      close ())
  |> ignore;
  button #grab_default ()

let pause (pt,d) action =
  let text = "Enter pause delay in milliseconds. Use 0 for infinite pause.\n(Remember that all pauses may be broken by typing space bar.)" in
  let main_vbox,close = init_object_dialog "Create pause" text "freeze_icon" in
  let entry = textentry ~style:Style.Int "Pause delay:" (string_of_int d) main_vbox#pack in
  let hbox = GPack.hbox ~packing: main_vbox#pack () in
  GMisc.label ~text:"Pause type:" ~packing:hbox#pack ()
  |> ignore;
  let type_menu = GPack.vbox ~border_width:5 ~packing:hbox#pack () in
  let (combo, (_, column)) = 
    GEdit.combo_box_text ~packing:type_menu#pack 
      ~strings:[ "Soft pause" ; "Freeze" ] () in
  combo#set_active (match pt with
      | Soft -> 0
      | Freeze -> 1);

  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () -> 
      let t = Parseutil.get_int entry#text
      and pause_type = (match get_combo_entry combo column with
          | "Freeze" -> Freeze
          | "Soft pause" -> Soft
          | _ -> raise (No_such_item "pause")) in
      action (Pause (pause_type, t));
      close  ())
  |> ignore;
  button #grab_default ()

let rotate (p,a) page =
  let header = "Enter rotation axis, angle, and duration" in
  let packing = fun o -> ignore (page o) in
  let main_vbox = GPack.vbox ~border_width:5 ~packing () in
  let head_box = GPack.hbox ~border_width:5 ~packing:main_vbox#pack() in
  GMisc.image ~file:(imagepath "rotate_icon") ~packing:head_box#pack ()
  |> ignore;
  GMisc.label ~text:header ~packing:head_box#add ()
  |> ignore;
  let (xentry,yentry,zentry) =  ask_coord "Axis:" ~zlabel:" z=" 
      p.x3 p.y3 ~zstring:p.z3 main_vbox#pack in
  let (angle_entry,time_entry,_) =  ask_coord "" 
      ~xlabel:" Angle (rad/frame)=" 
      ~ylabel:" Duration (sec)="  a "4" main_vbox#pack in

  let getresult () =
    let (x,y,z) = get_coords ~zentry xentry yentry
    and (a,t,_) = get_coords angle_entry time_entry in
    {move=(Rotate ({x3=x; y3=y; z3=z}, a));
     trange={min="0"; max=t}}

  in
  (main_vbox, getresult)

let translate p page =
  let header = "Enter velocity vector and duration" in
  let packing = fun o -> ignore (page o) in
  let main_vbox = GPack.vbox ~border_width:5 ~packing () in
  let head_box = GPack.hbox ~border_width:5 ~packing:main_vbox#pack() in
  GMisc.image ~file:(imagepath "translate_icon")
    ~packing:head_box#pack ()
  |> ignore;
  GMisc.label ~text:header ~packing:head_box#add () |> ignore;
  let (xentry,yentry,zentry) =  ask_coord 
      "Velocity vector (translation per frame):"
      ~zlabel:" z=" p.x3 p.y3 ~zstring:p.z3 main_vbox#pack in
  let time_entry = textentry ~style:Style.Float "Duration (sec):"  
      "4" main_vbox#pack in

  let getresult () =
    let (x,y,z) = get_coords ~zentry xentry yentry
    and t=time_entry#text in
    {move=(Translate {x3=x; y3=y; z3=z});
     trange={min="0"; max=t}}

  in
  (main_vbox, getresult)

let zoom s page =
  let header = "Enter scaling function and duration" in
  let packing = fun o -> ignore (page o) in
  let main_vbox = GPack.vbox ~border_width:5 ~packing () in
  let head_box = GPack.hbox ~border_width:5 ~packing:main_vbox#pack() in
  GMisc.image ~file:(imagepath "zoom_icon")
    ~packing:head_box#pack () |> ignore;
  GMisc.label ~text:header ~packing:head_box#add () |> ignore;
  let (xentry,yentry,zentry) =  ask_coord 
      "Velocity vector (translation per frame):"
      ~zlabel:" z=" s s ~zstring:s main_vbox#pack in
  let time_entry = textentry ~style:Style.Float "Duration (sec):"  
      "4" main_vbox#pack in

  let getresult () =
    let (x,y,z) = get_coords ~zentry xentry yentry
    and t=time_entry#text in
    {move=(Translate {x3=x; y3=y; z3=z});
     trange={min="0"; max=t}}

  in
  (main_vbox, getresult)
 
let motion {move=mv; trange={min=_st0; max=_st1}} action =
  let text = "Select the tab corresponding to the type of 3D motion\nyou want to perform." in
  let main_vbox,close = init_object_dialog "Create motion" text "motion_icon" in
  let onglets_box = GPack.notebook ~border_width:5 ~tab_border:5 
      ~show_tabs:true ~tab_pos:`TOP ~packing: main_vbox#add () in

  let rotate_args, translate_args, zoom_args, page = match mv with
    | Rotate (p,a) -> ((p,a), Default.translate, Default.zoom, 0)
    | Translate p -> (Default.rotate, p, Default.zoom, 1)
    | Zoom z -> (Default.rotate, Default.translate, z, 2) in

  let (_, getrotate) = let label = GMisc.label ~text:"Rotate" () in
    rotate rotate_args (onglets_box#append_page ~tab_label:(label#coerce))
  and (_, _gettranslate) = let label = GMisc.label ~text:"Translate" () in
    translate translate_args (onglets_box#append_page ~tab_label:(label#coerce))
  and (_, _getzoom) = let label = GMisc.label ~text:"Zoom" () in
    zoom zoom_args (onglets_box#append_page ~tab_label:(label#coerce)) in
  onglets_box#goto_page page;
  (************)

  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () ->
      let obj = match onglets_box#current_page with
        | 0 -> getrotate ()
        | 1 -> raise Not_Implemented
        | 2 -> raise Not_Implemented 
        | _ -> raise Not_Implemented in
      action (Motion obj);
      close  ())
  |> ignore;
  button #grab_default ()
 
(*****************************)

(* swap elements i and j in list *)
(* a remplacer par un truc plus efficace puisqu'on utilise que j=i+1 ...! *)
let list_swap i j l =
  let a = Array.of_list l in
  let x = a.(i-1) in a.(i-1)<- a.(j-1); a.(j-1)<-x;
  Array.to_list a

(* move down ieth elem from end of list. Last is 1 *)
(* lequel plus rapide: list_swap l n (n+1) ou list_down (n+1) l ?? *)
let list_down liste i=
  let rec loop l n res =
    match l with
 [] -> res
      | [e] -> e::res
      | e1::e2::r -> if n = i then loop r (n+2) (e1::e2::res)
 else loop (e2::r) (n+1) (e1::res)
  in loop (List.rev liste) 0 [];;

(* remove nth elem from end of list. Last is 1 *)
let list_remove liste numero =
  let rec loop l n res =
    match l with
 [] -> res
      | hd::r -> if n = numero then loop r (n+1) res
 else loop r (n+1) (hd::res)
  in loop (List.rev liste) 1 []

(* agit sur un objet *)
let action_obj obj action = 
  match obj with
    | Axis p -> axis p action
    | Func f -> func f action
    | Param f -> param f action
    | Text t ->  text t action
    | View v -> view v action
    | Color c -> color c action
    | Anim_func a -> anim a action
    | Surface s -> surf3d s action
    | Grid g -> grid g action
    | Pause p -> pause p action
    | Motion m -> motion m action

(* modif en place de l'objet *)
let edit_obj objr = 
  let action o = objr := o in
    action_obj !objr action

(* modifier l'objet numero numero de la liste...  *)
(* on commence à 1 *)    
let modify olistr numero =
  edit_obj (List.nth !olistr ((List.length !olistr) - numero))
    
let affiche_ligne olistr keyword message numero packing update =

  let remove numero = 
    olistr := list_remove !olistr numero in
  let swap n1 n2 = if n1 = n2 then ()
    else olistr:= List.rev (list_swap n1 n2 (List.rev !olistr)) in 
  let up numero = if numero=1 then () else swap (numero-1) numero in
  (*  else olistr := List.rev (list_swap (numero-1) numero (List.rev !olistr)) in (*ameliorer!*)*)
  let rec move n1 n2 = if n1 = n2 then () else (* à bulles... à améliorer *)
      let dn = if n2 > n1 then 1 else -1 in begin
        swap n1 (n1+dn);
        move (n1+dn) n2 
      end in
  let hbox = GPack.hbox ~spacing:2 ~packing () in
  (let button = GButton.button ~packing:hbox#pack () in
   let buttonbox = GPack.hbox ~spacing:0 ~packing:button#add () in
   GMisc.label ~justify:`RIGHT ~text:(Printf.sprintf "%2d" numero)
     ~packing:buttonbox#pack ()
   |> ignore;
   GMisc.image ~file:(imagepath ~size:16 (keyword ^ "_icon")) 
     ~packing:buttonbox#pack ()
   |> ignore;
   button#connect#clicked ~callback:(fun () ->
       print_int numero; flush stdout; modify olistr numero; update ())
   |> ignore;

   button#drag#source_set ~modi:[`BUTTON1] ~actions:[`MOVE] 
     [{Gtk.target="autre bouton"; Gtk.flags=[`SAME_APP]; Gtk.info=0}];
   let pixbuf = GdkPixbuf.from_file 
       (imagepath ~size:32 (keyword ^ "_icon")) in
   (* à mémoriser une fois pour toutes *)
   let (pixmap, masko) = GdkPixbuf.create_pixmap pixbuf in
   let pix = match masko with 
     | None -> new GDraw.pixmap pixmap
     | Some mask -> new GDraw.pixmap ~mask pixmap in
   button#drag#source_set_icon pix;
   button#drag#connect#beginning ~callback:(fun _ -> print_endline "Drag!")
   |> ignore;
   button#drag#connect#data_get ~callback:
     (fun _ sel ~info ~time -> sel#return (string_of_int numero);
       (* sel#return est obligatoire pour que ça marche... *)
       ignore (info, time);
       print_endline (Printf.sprintf "Drag button was %d" numero))
   |> ignore;
   button#drag#connect#data_received ~callback:(fun _ ~x ~y sel ~info ~time ->
       ignore (x,y,info,time);
       let from_num = int_of_string (sel#data) in
       move from_num numero; update ();
       print_endline 
         (Printf.sprintf "Drop: Received %d on %d." 
            from_num numero))
   |> ignore;
   button#drag#dest_set ~flags:[`ALL] ~actions:[`MOVE] 
     [{Gtk.target="autre bouton"; Gtk.flags=[`SAME_APP]; Gtk.info=0}];

  );

  let entry = GEdit.entry ~text:message ~width_chars:10 ~xalign:0. ~packing:hbox#add () in
  entry#set_editable false;
  let button = GButton.button ~packing:hbox#pack () in
  GMisc.image ~stock:`DELETE ~packing:button#add ()
  |> ignore;
  button#connect#clicked ~callback:(fun () ->
      print_int numero; flush stdout; remove numero; update ())
  |> ignore;

  (let button = GButton.button ~packing:hbox#pack () in
   GMisc.image ~stock:`GO_UP ~packing:button#add ()
   |> ignore;
   button#connect#clicked ~callback:(fun () -> print_int numero; flush stdout; up numero; update ()))
 
(* olist est une liste de ref de goplot_object *)
let affiche_liste olistr packing update = 
  let rec loop ol numero = 
    match ol with
      [] -> ()
    | hd::r -> let code = Code.obj !hd (* calculer une fois pour toutes ? *)
      and keyword = Convert.keyword !hd in 
      affiche_ligne olistr keyword code numero packing update |> ignore;
      loop r (numero+1)
  in loop (List.rev !olistr) 1
       
let rec update_liste olistr sheetbox packing =   
  let not_empty = (!olistr != []) in
    List.iter (fun item -> item not_empty) !desactivate;
    (!sheetbox)#destroy ();
    sheetbox:= GPack.vbox ~spacing:2 ~packing ();
    let update () = update_liste olistr sheetbox packing in
      affiche_liste olistr !sheetbox#pack update

(* add object (at the top of the list) *)
let add_object obj olistr update () =
  let action o = 
    olistr := (ref o) :: !olistr;
    update () in
    action_obj obj action
      
let make_toolbars olistr sheetbox scroll_packing tool_packing =
  let update () = update_liste olistr sheetbox scroll_packing in
  let buttons2d = 
    [ !Labels.param, Some "param_icon", Some !Labels.param_tip, 
      Some (add_object Default.param olistr update);
      !Labels.fplot, Some "func_icon", Some !Labels.fplot_tip, 
      Some (add_object Default.func olistr update);
      !Labels.axis, Some "axis_icon", None, 
      Some (add_object Default.axis olistr update);
      !Labels.text, Some "text_icon", None, 
      Some (add_object Default.text olistr update);
      !Labels.formula, Some "formula_icon", Some !Labels.formula_tip, 
      Some (add_object Default.formula olistr update);
      "LaTeX", Some "latex_icon", Some !Labels.latex_tip, 
      Some (add_object Default.latex olistr update);
      !Labels.oview, Some "view_icon", Some !Labels.view_tip,
      Some (add_object Default.view olistr update);
      !Labels.anim, Some "anim_icon", None,
      Some (add_object Default.anim olistr update);
      "Other", None, None, None] 
  and buttons3d = 
    [ !Labels.surf, Some "surf3d_icon", None,
      Some (add_object Default.surf3d olistr update);
      !Labels.grid, Some "grid_icon", None,
      Some (add_object Default.grid olistr update);
      !Labels.motion, Some "motion_icon", None,
      Some (add_object Default.motion olistr update);
      "Other", None, None, None] 
  and buttons_other = 
    [ !Labels.color, Some "color_icon", None,
      Some (add_object !Default.color olistr update);
      "Pause", Some "freeze_icon", None, 
      Some (add_object Default.pause olistr update);
      "Other", None, None, None] in

  (* let window = GWindow.window ~height:600 (* ~width:180*) ~title:"Create object" () in 
     rw := Some window;

     window #connect#destroy ~callback:
     (fun _ -> rw := None;
     update_liste olistr sheetbox scroll_packing); (*en principe inutile*)
  *)   
  let box1 = GPack.vbox ~packing:tool_packing () in
  let hbox = GPack.hbox~packing:box1#add () in

  (******************)
  let tip = GData.tooltips ~delay:1300 () in
  let toolbar1 = GButton.toolbar ~packing:hbox#add () in
  toolbar1#set_orientation `VERTICAL; 
  let item = GButton.tool_item () in
  (*let image = GMisc.image ~stock:`DIALOG_WARNING 
    (*~icon_size:`DIALOG*) ~packing:item#add () in *)
  let lab = GMisc.label ~text:"2D" ~packing:item#add () in
  set_font_style lab ~weight:`BOLD ~size:8;
  toolbar1#insert item;
  toolbar1#insert (GButton.separator_tool_item ());

  (* rem: le fait que les icones et/ou les labels s'affichent
     dépend du thème GTK *)
  let rec liste_tboutons (toolbar : GButton.toolbar) butlist = 
    match butlist with
    | [] -> ()
    | (_, _, _, None) :: tl -> liste_tboutons toolbar tl
    | (label, icon, the_tip, Some func) :: tl ->
      let tbutton = GButton.tool_button (*~label:label*) 
          ~expand:false ~homogeneous:false () in begin match icon with
        | Some filename -> 
          let image = GMisc.image 
              ~file:(imagepath ~size:32 filename) () in
          tbutton#set_icon_widget (image#coerce);
        | None -> ()
      end;
      begin let tip_label = match the_tip with
          | Some msg -> msg
          | None -> label
        in  tbutton#set_tooltip tip tip_label ""
      end;
      tbutton #connect#clicked ~callback:func |> ignore;
      let lab = GMisc.label ~text:label () in
      set_font_style lab ~size:8;
      tbutton#set_label_widget (lab#coerce);
      toolbar#insert tbutton;
      liste_tboutons toolbar tl
  in liste_tboutons toolbar1 buttons2d;
  (******************)
  GMisc.separator `VERTICAL ~packing: hbox#pack () |> ignore;
  GMisc.separator `HORIZONTAL ~packing: box1#pack () |> ignore;
  let toolbar2 = GButton.toolbar ~packing:hbox#add () in
  toolbar2#set_orientation `VERTICAL;
  let item = GButton.tool_item () in
  let lab = GMisc.label ~text:"3D" ~packing:item#add () in
  set_font_style lab ~weight:`BOLD ~size:8;
  toolbar2#insert item;
  toolbar2#insert (GButton.separator_tool_item ());
  liste_tboutons toolbar2 buttons3d;
  let item = GButton.tool_item () in
  let lab = GMisc.label ~text:"Divers" ~packing:item#add () in
  set_font_style lab ~weight:`BOLD ~size:8;
  toolbar2#insert (GButton.separator_tool_item ());
  toolbar2#insert item;
  toolbar2#insert (GButton.separator_tool_item ());
  liste_tboutons toolbar2 buttons_other;


  (*        let box2 = GPack.vbox ~spacing: 10 ~border_width: 1
            ~packing:box1#pack () in 

            let button = GButton.button ~stock:`CLOSE ~packing:box2#pack (*add*) () in
            button #connect#clicked ~callback:window#destroy;
            button #grab_default ();



            window #show ();
  *)
  (toolbar1,toolbar2)
       
(***************************************************************************)


let shell command = 
  Printf.kprintf (fun s -> ignore (Sys.command s)) command;;


(************ menus ********)
let filename = ref None

let defaultname filename = match filename with
    None -> "save.gpl"
  | Some name -> name

let ask_filename ?default ?(keep_name=true) title filename file_action =
  let selection =
    let filename = match default with
      | None -> defaultname !filename
      | Some s -> s in
    GWindow.file_selection ~title ~modal:true ~filename ~position:`MOUSE () in
  selection #cancel_button#connect#clicked ~callback:selection#destroy
  |> ignore;
  selection #ok_button#connect#clicked ~callback:(fun () -> 
      let name = selection#filename in 
      if keep_name then filename := Some name;
      if Debug.debug then print_endline name;
      file_action name;
      selection#destroy ())
  |> ignore;
  selection#show ()

let save_as olist filename =
  ask_filename "Save as:" filename (fun name -> Output.file olist name)

let save_oplot olist filename =
  (* TODO ajouter le module Mathutils *)
  let default = match !filename with
    | None -> "my_oplot.ml"
    | Some name -> Filename.remove_extension name ^ ".ml" in
  ask_filename ~default ~keep_name:false "Export to:" filename
    (fun name -> create_oplot_file olist name)

let save olist filename = 
  match !filename with
      None -> save_as olist filename
    | Some name -> (*create_oplot_file*) Output.file olist name
 
let open_file olistr update =
  ask_filename "Open:" filename (fun name -> Read.file olistr name; update ())


type menu_title = | Title_label of string
    | Title_stock of GtkStock.id

type menu_entry = | Stock of (GtkStock.id * desactivate_widget * (unit -> unit)) 
    | Label of (string * desactivate_widget * (unit -> unit))
    | Check of (string * desactivate_widget * (bool -> unit))

let create_menu_item this_menulist packing =
  let menu =  GMenu.menu ~packing () in
  ignore (GMenu.tearoff_item ~packing: menu#append ());
  let rec loop l = match l with
      [] -> ()
    | (Label (s, desact, action))::r -> 
      let item = GMenu.image_menu_item ~use_mnemonic:true ~label:s
          ~packing:menu#append () in 
      item#connect#activate ~callback:action |> ignore;
      update_desactivate_list (item#coerce#misc#set_sensitive) desact;
      loop r
    | (Stock (s, desact, action))::r ->
      let item = GMenu.image_menu_item ~stock:s ~packing:menu#append () in
      item#connect#activate ~callback:action |> ignore;
      update_desactivate_list (item#coerce#misc#set_sensitive) desact;
      (* pour griser l'entrer: item#coerce#misc#set_sensitive false; *)
      loop r
    (* GMisc.image ~stock:`DELETE ~packing:item#add (); *)  (* pour mettre une image, enlever le ~label du GMenu *)
    | (Check (s, desact, action))::r ->
      let item = GMenu.check_menu_item ~use_mnemonic:true ~label:s ~packing:menu#append () in
      item#connect#activate ~callback:(fun () -> action item#active)
      |> ignore;
      update_desactivate_list (item#coerce#misc#set_sensitive) desact;
      (*(fun () ->  variable := not !variable);*)
      loop r
  in
  loop this_menulist
 
let create_menu menulist packing = (* rajouter sous-menus ? *)
  let rec loop l = match l with
      [] -> ()
    | (label,this_menulist)::r ->
      let item = match label with
          Title_label s ->
          GMenu.image_menu_item ~use_mnemonic:true ~label:s ~packing ()
        | Title_stock s ->
          GMenu.image_menu_item ~right_justified:(s=`HELP) ~stock:s ~packing ()
      in
      create_menu_item this_menulist item#set_submenu;
      loop r in
  loop menulist


(*** ***)
(* call this function to refresh the opengl display immediately *)
let rec zone_display =
  let counter = ref 0 in
  (* : a mettre dans zone ? si jamais on affiche plusieurs fenêtres en même
     temps... *)
  fun zone ->
    if zone.area#misc#visible then (* utile ? *)
      begin
        Debug.print "display:%d..." !counter;
        let dt = Osys.gtk_mainloop zone.graph in
        zone.area#swap_buffers ();
        (* meme si le trace met 2ms, periodiquement, cela va prendre un peu
           plus de temps. Si cela arrive trop souvent, ce doit vouloir dire que
           GTK n'arrive plus a effectuer ses autres taches. Dans ce cas on
           reduit le framelength *)
        if dt >= zone.framelength then begin
          Printf.printf "(%d:%d)\n" dt zone.framelength;
          incr counter;
          if !counter = 5 then begin
            counter := 0;
            zone.framelength <- min (dt+2) ((zone.framelength*12)/10+2);
            renew_timer zone; (* update timer *)
            if Debug.debug then print_endline 
                (Printf.sprintf "Last frames take too long to draw.\n\
                                 Renewing timer with framelength: %d ms" 
                   zone.framelength)
          end
        end;
        Debug.print "done in %dms." dt
      end
    else Debug.print "Window is not visible."

(* this one first checks that the window is open *) 
and zone_redisplay zone =
  if zone.isopen then zone_display zone

(* if the picture requires it (animation), then this returns a timer that
   refreshes the display periodically when the window is open *)
and new_timer zone =
  if Osys.has_anim zone.graph then
    begin Debug.print "New timer";
      Some (GMain.Timeout.add ~ms:zone.framelength
              ~callback:(fun () -> 
                  zone_redisplay zone;
                  true)) (* true means repeat *)
    end
  else begin
    Debug.print "No timer needed"; None
  end

(* this one attaches the above timer to the zone *)
and renew_timer zone =
  (match zone.timer with
   | Some t -> GMain.Timeout.remove t
   | _ -> ());
  zone.timer <- new_timer zone
    
let remove_timer zone =
  match zone.timer with
    | Some t -> (GMain.Timeout.remove t; zone.timer <- None)     
    | None -> ()

let oplot_display sh zone =
  zone.graph <- Plt.Sheet sh;
  renew_timer zone;
  zone_display zone

(* inutilise... *)
let refresh_display olist zone =
  print_endline "Refresh...";
  let sh = Convert.olist olist in
    oplot_display sh zone

let display olist zone =
  let sh = Convert.olist olist in
  oplot_display sh zone;;

(* this one first checks whether a timer is about to end before attemping to
   draw once. If there is no timer, we add one to to trigger the drawing. *)
let cautious_zone_display =
  let timeout : (GMain.Timeout.id option) ref = ref None in
  fun zone -> 
    match (zone.timer, !timeout) with
    | None, None ->
      if Debug.debug then print_endline "Add timer";
      timeout := Some 
          (GMain.Timeout.add ~ms:zone.framelength ~callback:
             (fun () -> zone_redisplay zone;
               timeout := None;
               false (* une seule fois *)
               (* ou alors utiliser GMain.Idle.add ? *) 
             ))
    | _, _ -> if Debug.debug then print_endline "Busy"
(* pas la peine de retracer s'il y a deja un timer. *)
       
     
let zone_resize =
  (* let timeout : (GMain.Timeout.id option) ref = ref None in *)
  fun zone ~width ~height -> 
  if Debug.debug then print_endline (Printf.sprintf "Width:%d Height:%d\n" width height);
  Plt.resize_window width height;
  Osys.gl_resize ();
  cautious_zone_display zone
 (* ou plutot ne rien faire ici mais réagir à un événement 'resize' ??
    (mais dans ce cas faire attention à ne pas accumuler dans la file d'attente
    des événements...  *)


(* for devices other than gl *)
let use_oplot_device olist dev zone =
  let sh = Convert.olist olist in
  Plt.display ~dev sh;
    if zone.isopen then display olist zone;;

let viewps olist zone =
  use_oplot_device olist Plt.gv zone;;

let viewfig olist zone =
  use_oplot_device olist Plt.xfig zone;;

let view_screenshot zone = 
  let win = (* GtkBase.Widget.window zone.area#as_area *)
    zone.area#misc#window in
    (* c'était dur à trouver ! *)
  let width, height = Gdk.Drawable.get_size win in
    (* ça aussi... *)
  let pix = GdkPixbuf.create ~width ~height () in
    GdkPixbuf.get_from_drawable ~dest:pix win; (* ne marche pas sous mac *)
    ask_filename "Save PNG image as:" (ref (Some "image.png"))
      (fun name -> GdkPixbuf.save ~filename:name ~typ:"png" pix)

(*********************)
      
let toggle_fullscreen_old zone = 
  fullscreen := not !fullscreen;
  if !fullscreen then zone.window#fullscreen () else zone.window#unfullscreen ();
  let width, height = Gdk.Drawable.get_size (zone.window#misc#window) in
    zone_resize zone ~width ~height
   
let set_fullscreen zone b = 
  fullscreen := b;
  if b then zone.window#fullscreen () else zone.window#unfullscreen ();
  let width, height = Gdk.Drawable.get_size (zone.window#misc#window) in
    zone_resize zone ~width ~height

let toggle_fullscreen zone = 
  set_fullscreen zone (not !fullscreen)

(********************************)

let pref_window zone =
  let window = GWindow.window ~title:"Preferences" ~modal:true
      ~border_width: 5 ~position:`MOUSE () in
  let main_vbox = GPack.vbox ~packing: (window#add) () in

  let frate  = GPack.hbox ~packing:main_vbox#add () in
  GMisc.label ~text:"Desired frame rate (frame/sec): " ~packing:frate#pack ()
  |> ignore;
  let framerate = 1000. /. float zone.framelength in
  let framerate_slide = curseur_hor framerate 0
      ~lower:1. ~upper:500. 1. 10. frate#add in

  let scale  = GPack.hbox ~packing:main_vbox#pack () in
  GMisc.label ~text:"Graphics scale: " ~packing:scale#pack ()
  |> ignore;
  let scale_slide = curseur_hor (Plt.get_gl_scale ()) 1
      ~lower:0.5 ~upper:5. 0.1 10. scale#add in
  (* pourquoi page_incr=1. ne marche pas ?? *)

  let button = validatebutton main_vbox#pack in
  button#connect#clicked ~callback:(fun () ->  
      let fps = framerate_slide#adjustment#value in
      Debug.print "FPS=%f" fps;
      zone.framelength <- (int_of_float (1000. /. fps));
      let old_scale = Plt.get_gl_scale () in
      Plt.set_gl_scale scale_slide#adjustment#value;
      Debug.print "GL Scale=%f" (Plt.get_gl_scale ());
      (* let width = 500 |> Osys.iscale in
       * let height = 375 |> Osys.iscale in *)
      let width, height = Gdk.Drawable.get_size zone.area#misc#window in
      let width = Osys.scale (float width /. old_scale) |> int_of_float in
      let height = Osys.scale (float height /. old_scale) |> int_of_float in
      zone.area#set_size ~width ~height; (* ne marche pas bien, en partic en mode détaché *)
      Osys.gl_init ();
      Osys.gl_resize ();
      Debug.print "width=%i" width;
      Osys.force_refresh ();
      (* refresh (); *)
      cautious_zone_display zone;
      window#destroy ())
  |> ignore;
  button #grab_default ();
  window #show ()


(*********************************)
let set_cursor area =
  area#misc#realize ();
  Gdk.Window.set_cursor area#misc#window (Gdk.Cursor.create `HAND1)
    
let gtk_key _window zone ev =
  let key = GdkEvent.Key.keyval ev in
    if key = GdkKeysyms._Escape then print_endline "ESCAPE" else
      if key = GdkKeysyms._f then toggle_fullscreen zone else
      print_endline "some key";
    true

let disconnect obj signal_ref =
  match !signal_ref with
    | Some signal -> begin obj#misc#disconnect signal; signal_ref := None end
    | None -> ()
 
(* le zero est en haut *)
let get_mouse zone = 
  let (x,y) = Gdk.Window.get_pointer_location (zone.area)#misc#window in
    (x, Osys.get_window_height () - y)
  
(* variante avec un GdkEvent.Button *)
let get_ev_mouse ev =
  let (x,y) =  (GdkEvent.Button.x ev, GdkEvent.Button.y ev) in
    (int_of_float x, Osys.get_window_height () - int_of_float y)
      
(* appele lorsque le bouton de la souris est presse *)
let gtk_mouse =
  let motion_signal : (GtkSignal.id option) ref = ref None
  and stop_signal   : (GtkSignal.id option) ref = ref None
  in fun zone ev ->
    let i = GdkEvent.Button.button ev in
    if Debug.debug then print_endline (Printf.sprintf "button %d pressed." i);
    let (x,y) = get_mouse zone in
    if Debug.debug then begin
      let (xe,ye) = get_ev_mouse ev in
      print_endline (Printf.sprintf "Mouse position (%d, %d) or (%d, %d)." x y xe ye)
    end;
    Osys.set_mouse_x x;
    Osys.set_mouse_y y;
    (* let timeout : (GMain.Timeout.id option) ref = ref None in *)
    (* we disconnect the former signal if there was one *)
    disconnect zone.area motion_signal;
    motion_signal := Some (zone.area#event#connect#motion_notify ~callback:
                             (fun _ev ->
                                let (x,y) = get_mouse zone in
                                Osys.gl_mouse_motion x y;
                                if Debug.debug then print_endline (Printf.sprintf  "Motion: Mouse position (%d, %d)." x y);
                                cautious_zone_display zone;
                                true (* ? *)
                             )); 
    (* The value returned from this function indicates whether the event should be
       propagated further by the GTK event handling mechanism. Returning TRUE indicates
       that the event has been handled, and that it should not propagate
       further. Returning FALSE continues the normal event handling. See the section on
       Advanced Event and Signal Handling for more details on this propagation
       process. *)
    stop_signal := Some (zone.area#event#connect#button_release ~callback:
                           (fun _ev ->
                              if Debug.debug then print_endline (Printf.sprintf "Release button");
                              disconnect zone.area motion_signal;
                              true));  
    true (* ? *)
     
let enter_area signal window zone ev =
  Debug.print "Crossing state %d" (GdkEvent.Crossing.state ev);
  window#event#add [`KEY_PRESS];
  signal:=Some (window#event#connect#key_press ~callback:(gtk_key window zone));
  true
    
let leave_area signal window ev =
  Debug.print "Crossing state %d" (GdkEvent.Crossing.state ev);
  (match !signal with
   | Some s -> GtkSignal.disconnect window#as_widget s
   | None -> failwith "hein ?");
  true
    
    
(*********************************)
    
let attach zone parent button window =
  Debug.print "On attache...";
  let visible = zone.area#misc#visible in
  let widg = zone.area#misc#toplevel in
  zone.area#misc#reparent parent;
  set_cursor zone.area;
  zone.window <- window;

  button#set_label !Labels.detach;
  if visible then widg#destroy () else Debug.print "Not visible ??"
 
let detach zone _parent button width height =
  Debug.print "detache";
  let new_win = GWindow.window ~title:"Goplot graph" ~width ~height ~allow_grow:true ~allow_shrink:true () in
  zone.area#misc#reparent (new_win#misc#toplevel);
  new_win#connect#destroy ~callback:(fun () ->
      Debug.print "on ferme !";
      button#set_active false)
  |> ignore;
  button#set_label "Attach";
  (* il faut attendre un peu que GL se mette en place avant d'ajuster la
     taille: *)
  GMain.Timeout.add ~ms:60 ~callback:(fun () ->
      zone_resize zone ~width ~height; false)
  |> ignore;
  zone.area#event#add [`KEY_PRESS];
  zone.window <- new_win;
  new_win#event#connect#key_press ~callback:(gtk_key new_win zone)
  |> ignore;
  set_cursor zone.area;
  new_win#show ()
      
      
(*********************************)
      
let about () = 
  let image =  GdkPixbuf.from_file (imagepath ~size:128 "pelotte_icon") in
  let license = 
    let in_channel = open_in (concat goplotdir "gpl.txt") in
    let buf = Buffer.create 1024 in
    let rec loop () = (* TODO simplify this: *)
      Buffer.add_channel buf in_channel 4096; loop () in
    try loop () with End_of_file -> ();
    let content = (*Glib.Convert.locale_to_utf8*) Buffer.contents buf in
    close_in in_channel; content in
  let window = GWindow.about_dialog 
      ~authors:["San Vũ Ngọc"]
      ~comments:"A mathematical plotter"
      ~license
      ~logo:image (*......*)
      ~name:"gOplot"
      ~version:Init.version
      ~website:"https://github.com/sanette/goplot"
      (* ~type_hint:`DIALOG *)
      ~modal:true
      ~title:"gOplot"
      () in
  (* ne marche pas... *)
  (*window#connect#response ~callback:(fun _ -> print_endline "CLOSE");*)

  (*window#connect#close ~callback:window#destroy;*)
  let _ok = window#run () in
  window#destroy ();;


(*********************************) 

(* inutile*)
let splash () =
  let window = GWindow.window ~type_hint:`SPLASHSCREEN ~position:`CENTER_ALWAYS 
      ~resizable:false ~width:324 ~height:345 ~show:true () in
  GMisc.image ~file:(concat imagedir "pelotte_splash.png")
    ~packing:window#add ()
  |> ignore;
  window;;

(*********************************) 


let quitte () = 
  Osys.interrupt ();
  Plt.quit ();
  print_endline "Quitting goplot!";
  GMain.Main.quit ();
  exit 0;; (*utile ?*)

(***************************************************************************)

let main () =
  let quit_on_interrupt _ =  
    prerr_endline "User required interrupt!" in
  Sys.set_signal Sys.sigint (Sys.Signal_handle quit_on_interrupt); 
  (* ne marche pas avec CTRLc.. Faire aussi sigkill *)
  let olistr = ref [] in
  let window = GWindow.window (* ~width:900 *) ~title:"gOplot - a GUI for Oplot" () in
  window#connect#destroy ~callback:GMain.quit |> ignore;
  (*      window#set_resize_mode `IMMEDIATE; *)

  let vbox = GPack.vbox ~packing:window#add () in

  (* initialisation de la menubar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in

  (* mainbox *)
  let w = GPack.hbox ~spacing:2 ~packing:vbox#add () ~border_width:10 in
  let mainbox = GPack.hbox ~spacing:2 ~packing:w#add () ~border_width:10 in
  let toolbox = GPack.hbox ~packing:w#pack () in

  (* area opengl, n'a pas l'air tres compatible avec
     GBin.handle_box, dommage *)

  let width = 500 |> Osys.iscale in
  let height = 375 |> Osys.iscale in
  (*Dev.multisampling := false;*)
  let area =
    GlGtk.area [`USE_GL; `RGBA; `DEPTH_SIZE 8; `STENCIL_SIZE 8 ]
      (*  [`SAMPLE_BUFFERS 0; `SAMPLES 4; ne marche pas
          `DOUBLEBUFFER] *)
      ~width ~height ~packing:w#add () in
  Debug.print "load splash";
  let zone = { window = window;
               area = area; graph = (Plt.Sheet Default.splash);
               framelength = Osys.get_frame_length ();
               timer = None; isopen = false; anim = false} in
  area#connect#realize ~callback:(fun () ->
      Osys.gl_init ();
      Plt.resize_window width height;
      Osys.gl_resize ())
  |> ignore;
  area#connect#reshape ~callback:(zone_resize zone) |> ignore;
  area#connect#display ~callback:(fun () -> zone_redisplay zone) |> ignore;
  let signal : (GtkSignal.id option) ref = ref None in
  area#event#connect#enter_notify ~callback:(enter_area signal window zone)
  |> ignore;
  area#event#connect#leave_notify ~callback:(leave_area signal window)
  |> ignore;
  area#event#connect#button_press ~callback:(gtk_mouse zone)
  |> ignore;
  area#event#add [ `ALL_EVENTS ];
  set_cursor area;

  (* zone des objets goplot *)  
  let tmp = GBin.scrolled_window ~width:400 ~border_width:0 ~hpolicy: `AUTOMATIC 
      ~vpolicy: `AUTOMATIC ~shadow_type:`IN ~placement:`TOP_LEFT 
      ~packing:mainbox#add () in

  let scrolled = GBin.frame ~border_width:5 ~label:!Labels.main 
      ~packing:tmp#add_with_viewport () in  

  let sheetbox = ref (GPack.vbox ~spacing:2 ~packing:scrolled#add ()) in

  (* barre d'outils *)

  let (toolbar1,toolbar2) = make_toolbars olistr sheetbox scrolled#add toolbox#add in

  (****suite du menu****)      
  let update () =
    update_liste olistr sheetbox scrolled#add;
    display !olistr zone in
  
  let file_menulist = 
    [ Stock (`OPEN, Do_not_desactivate, fun () ->
          Debug.print "Open";
          open_file olistr update) ;
      Stock (`SAVE, Desactivate, fun () ->
          Debug.print "Save";
          save !olistr filename) ;
      Stock (`SAVE_AS, Desactivate, fun () ->
          Debug.print "Save as";
          save_as !olistr filename) ;
      Label (!Labels.save_oplot, Desactivate, fun () ->
          Debug.print "Export to oplot";
          save_oplot !olistr filename) ;
      Stock (`PRINT, Desactivate, fun () -> Debug.print "Print!";
               viewps !olistr zone) ;
      Stock (`QUIT , Do_not_desactivate, quitte) ] in
  let view_menulist = 
    [ Check (!Labels.fullscreen, Do_not_desactivate, 
             fun b -> set_fullscreen zone b) ;
      Check (!Labels.hide_tools, Do_not_desactivate,
             fun b -> if b then begin
                 toolbar1#misc#hide();
                 toolbar2#misc#hide() end
               else begin
                 toolbar1#misc#show();
                 toolbar2#misc#show() end);
      Label (!Labels.draw, Desactivate, fun () ->
          (* Osys.gl_init (); *)
          display !olistr zone) ;
      Label (!Labels.viewps, Desactivate, fun () -> viewps !olistr zone ) ;
      Label (!Labels.view_screenshot, Desactivate, 
             fun () -> view_screenshot zone) ] in
  let outils_menulist = 
    [ Label (!Labels.viewfig, Desactivate, 
             fun () -> viewfig !olistr zone) ;
      Stock (`PREFERENCES, Do_not_desactivate, 
             fun () -> pref_window zone) ] in
  let help_menulist = 
    [ Stock (`ABOUT , Do_not_desactivate, fun () ->
          (print_endline "gOplot, a GUI for the Oplot drawing library.\n By San \
                          Vu Ngoc\n (c) 2006"; about()) );
      Label (!Labels.math, Do_not_desactivate, Help.math_liste_dialog) ] in
  let menulist = [  ( Title_label !Labels.file , file_menulist ) ; 
                    ( Title_label !Labels.view , view_menulist ) ; 
                    ( Title_label !Labels.outils , outils_menulist )] in
  (* le (Stock `FILE) ne marche pas bien *)
  create_menu menulist menubar#append;

  let gallery_menulist = 
    [ Label ("Simple plot", Do_not_desactivate, 
             fun () -> olistr := Gallery.example; update ());
      Label ("Pinched torus", Do_not_desactivate, 
             fun () -> olistr := Gallery.pinched_torus; update ())] in
  let new_menulist = [( Title_label !Labels.gallery , gallery_menulist );
                      ( Title_stock `HELP , help_menulist )] in
  create_menu new_menulist menubar#append;
  (********)

  let actionbox = GPack.vbox ~spacing:2 ~packing:mainbox#pack () in
  GMisc.image ~file:(imagepath "pelotte_icon")
    ~packing:actionbox#pack () |> ignore;
  (let button = GButton.button ~use_mnemonic:true 
       (*~label:!Labels.draw*)
       ~packing:(actionbox#pack ~padding:5) () in
   let buttonbox = GPack.vbox ~packing:button#add () in
   GMisc.image ~file:(imagepath ~size:32 "play_icon")
     ~packing:buttonbox#pack () |> ignore;
   GMisc.label ~text:!Labels.draw ~use_underline:true
     ~packing:buttonbox#pack () |> ignore;
   button#connect#clicked ~callback:(fun () ->
       display !olistr zone) |> ignore;
   let des = button#coerce#misc#set_sensitive in
   desactivate := des :: !desactivate);

  (let button = GButton.toggle_button ~use_mnemonic:true 
       ~label:!Labels.detach ~active:false
       ~packing:(actionbox#pack ~padding:5) () in
   button#connect#toggled ~callback:
     (* attention le signal "clicked" est active lorsqu'on fait
        #set_active ! *)
     (fun () -> match button#active with
        | true -> begin
            Debug.print "On detache...";
            let awidth, aheight = Gdk.Drawable.get_size (area#misc#window)
            and width, height = Gdk.Drawable.get_size (window#misc#window) in
            detach zone w#coerce button awidth aheight;
            window#resize ~height:(max 50 (height-aheight)) ~width:(max 50 (width-awidth));
          end
        | false -> attach zone w#coerce button window
     )
   |> ignore);

  (let button = GButton.button ~stock:`STOP 
       ~packing:(actionbox#pack ~padding:5) () in
   button#connect#clicked ~callback:
     (fun () -> (* interrupt *)
        remove_timer zone)
   |> ignore);

  (let button = GButton.button ~stock:`QUIT 
       ~packing:(actionbox#pack ~padding:5) () in
   button#connect#clicked ~callback:quitte
   |> ignore;);

  (* update (); *)
  window, zone
  
(**********************************************************************)



(***************************************)

let _ =
  (*let splash_win = splash () in*)
  Osys.init ();
  Osys.(set_default_gl GTK);

  Labels.inits ();
  (* let oplot_channel = Event.new_channel () in *)
  let main_window, zone = main () in
  print_endline "Starting gOplot";
  let show = fun () -> begin
      main_window#show ();
      zone.isopen <- true
    end in
  if Osys.first_time () then begin
    print_endline "It seems that this is the first you run gOplot";
    Firsttime.wizard2 show
  end 
  else show ();
  (*splash_win#destroy ();*)
  GMain.Main.main ()
;;



(* 
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)
