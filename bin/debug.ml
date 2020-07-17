let debug = Array.to_list Sys.argv
            |> List.mem "debug"

let print s =
  Printf.ksprintf (fun s -> if debug then print_endline s) s
