open Apm
open Inverted_index

let main =
  let argc = Array.length Sys.argv in 

  let usage0 = "usage: " ^ Sys.argv.(0) ^ " " in
  let usage1 = usage0 ^ "mkindex filename\n" in
  let usage2 = usage0 ^ "matchme filename num_matches first_name last_name\n" in
  let full_usage = usage1 ^ usage2 in

  if argc < 2 then 
    (print_string full_usage; exit 0)
  else if Sys.argv.(1) = "mkindex" then
    if argc < 3 then 
      (print_string usage1; exit 0)
    else
      mkindex Sys.argv.(2)
  else 
    if argc < 6 then 
      (print_string usage2; exit 0)
    else
      let args = [Sys.argv.(2); Sys.argv.(3); Sys.argv.(4); Sys.argv.(5)] in
      matchme (Array.of_list args)
;;
