module M = Parseutil.Math
             
let math_liste_dialog () =
  let makelist s1 _ l = s1::l in
  let int2int_list = List.sort compare
      (Hashtbl.fold makelist M.int2int_table [])
  and float2int_list = List.sort compare
      (Hashtbl.fold makelist M.float2int_table [])
  and int2float_list = List.sort compare
      (Hashtbl.fold makelist M.int2float_table [])
  and float2float_list = List.sort compare
      (Hashtbl.fold makelist M.float2float_table [])
  and floatconst_list = List.sort compare
      (Hashtbl.fold makelist M.floatconst_table []) in
  let c s1 s2 = if s1 = "" then s2 else s1 ^ ", " ^ s2 in
  let msg_win = GWindow.window ~title:(Labels.strip !Labels.math)
      ~width:500 ~position:`MOUSE () in
  let main_vbox = GPack.vbox ~packing: (msg_win#add) () in

  let add_line label text = 
    let frame = GBin.frame ~label ~border_width:5 ~packing:main_vbox#pack () in
    GText.view ~buffer:(GText.buffer ~text ()) ~editable:false
      ~wrap_mode:`WORD ~packing:frame#add () in

  List.fold_left c "" float2float_list |> add_line "float -> float"
  |> ignore;
  List.fold_left c "" float2int_list |> add_line "float -> integer" 
  |> ignore;
  List.fold_left c "" int2float_list |> add_line "integer -> float" 
  |> ignore;
  List.fold_left c "" int2int_list |> add_line "integer -> integer" 
  |> ignore;
  List.fold_left c "" floatconst_list |> add_line "float constants"
  |> ignore;


  let button = GButton.button ~stock:`QUIT
      ~packing:(main_vbox#pack ~padding:5) () in
  button#connect#clicked ~callback:msg_win#destroy |> ignore;
  msg_win#activate_focus () |> ignore;
  msg_win#show ();;

