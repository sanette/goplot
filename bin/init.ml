let version = "0.2"

module Plt = Oplot.Plt
module Osys = Oplot.Internal

type zone = { mutable window:GWindow.window; (* necessaire ? *)
              mutable area:GlGtk.area; mutable graph:Plt.plot_object;
              mutable framelength:int; mutable timer:GMain.Timeout.id option;
              mutable isopen:bool; mutable anim:bool }

type range = { min:string; max:string}

type point = { x:string; y:string }

type point3d = { x3:string; y3:string; z3:string }

type func1 = { formula:string; var:string }

type func2 = { formula2:string; var1:string; var2:string }

type text_format = Plain_text | Latex_formula | Full_latex

type text = { msg:string; format:text_format; pos:point; size:int; alignment:string }

type pause_type = Soft | Freeze

type motion_type = 
  | Translate of point3d
  | Rotate of point3d * string (* angular speed *)
  | Zoom of string (*?*)

type motion = { move:motion_type; trange:range }

type goplot_object =
  | Axis of (point * ((Plt.plot_object option) ref))
  | Func of (func1 * range * ((Plt.plot_object option) ref))
  | Param of (func1 * func1 * range * ((Plt.plot_object option) ref))
  | Text of (text * ((Plt.plot_object option) ref))
  | View of (point * point)
  | Anim_func of (func2 * range * range)
  | Surface of (func2 * func2 * func2 * range * range * ((Plt.plot_object option) ref))
  | Grid of (func2 * range * range * ((Plt.plot_object option) ref))
  | Color of (Plt.color)
  | Pause of (pause_type * int)
  | Motion of motion

exception Not_Implemented

let concat = Filename.concat

(* let oplotdir = Osys.oplot_dir *)

let goplotdir =
  let basename, dirname = Filename.basename, Filename.dirname in
  let exe = Sys.executable_name in
  Debug.print "Executable: %s" (basename exe);
  Debug.print "Directory: %s" (basename (dirname exe));
  match (basename exe), (basename (dirname exe)) with
  | "goplot", "bin" ->
    Filename.concat (dirname (dirname exe)) "share/goplot"
  | _ -> Filename.concat (dirname (dirname exe)) "share"

let () = Debug.print "Using goplotdir=%s " goplotdir
    
let imagedir = concat goplotdir "images"

let imagepath ?(size=64) name =
  let s = Osys.iscale 1 * size in
  let rec loop s =
    if s = 0 then concat imagedir "warning.png"
    else let file = name ^ (string_of_int s) ^ ".png" 
                    |> concat imagedir in
      if Sys.file_exists file then file
      else loop (s-1) in
  loop s

