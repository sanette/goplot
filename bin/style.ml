(* style de l'interface graphique gtk2 *)
type entry_t = Text | Float | Int 
	       | Math of string (* variable *)
	       | Math2 of (string * string);;

let name et = match et with
  | Text -> "Text"
  | Float -> "Floating point number"
  | Int -> "Integer number"
  | Math var -> "Function of variable " ^ var
  | Math2 (var1, var2) -> 
      Printf.sprintf "Function of variables (%s, %s)" var1 var2;;

(*  inutilisé *)
let get (e : GEdit.entry) = (e # misc) # style;; 

let mathentry_base = [`NORMAL, `RGB (231*256, 255*256, 239*256);
		      `SELECTED, `RGB (11*256, 120*256, 113*256)];;

let textentry_base = [`NORMAL, `RGB (255*256, 240*256, 238*256);
		      `SELECTED, `RGB (148*256, 80*256, 150*256)];;

let intentry_base = [`NORMAL, `RGB (244*256, 255*256, 223*256);
		     `SELECTED, `RGB (150*256, 99*256, 28*256)];;

let set_entry_style ?callback (et : entry_t) (e : GEdit.entry) =
  e # misc # modify_base (match et with
			    | Text -> textentry_base
			    | Int -> intentry_base
			    | Math _ | Float | Math2 _ -> mathentry_base);
  match callback with
    | Some f -> ignore(e#connect#activate ~callback:(fun () -> f et e))
    | None -> ();;


(*

type state_type =
    [ `ACTIVE
    | `INSENSITIVE
    | `NORMAL
    | `PRELIGHT
    | `SELECTED ]

type color =
    [ `BLACK
    | `COLOR of Gdk.color
    | `NAME of string
    | `RGB of int * int * int  chaque composante 0..65535
    | `WHITE ]


ocaml -I +lablgtk2 lablgtk.cma gtkInit.cmo
*)
